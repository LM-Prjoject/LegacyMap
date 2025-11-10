package com.legacymap.backend.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.dto.request.NotificationCreateRequest;
import com.legacymap.backend.dto.response.NotificationResponse;
import com.legacymap.backend.dto.response.NotificationStatsResponse;
import com.legacymap.backend.entity.Notification;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.NotificationRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class NotificationService {

    private final NotificationRepository notificationRepository;
    private final UserRepository userRepository;
    private final ObjectMapper objectMapper;
    private final Map<UUID, SseEmitter> emitters = new ConcurrentHashMap<>();

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    @Transactional
    public NotificationResponse createNotification(UUID userId, NotificationCreateRequest request) {
        User user = loadUserOrThrow(userId);

        Notification notification = Notification.builder()
                .user(user)
                .title(request.getTitle())
                .message(request.getMessage())
                .type(request.getType() != null ? request.getType() : Notification.NotificationType.system)
                .isRead(false)
                .createdAt(OffsetDateTime.now(ZoneOffset.UTC))
                .build();

        if (request.getRelatedEntity() != null) {
            notification.setRelatedEntity(objectMapper.valueToTree(request.getRelatedEntity()));
        }

        Notification savedNotification = notificationRepository.save(notification);
        NotificationResponse response = mapToResponse(savedNotification);

        sendNotificationToUser(userId, response);

        return response;
    }

    public void sendNotificationToUser(UUID userId, NotificationResponse response) {
        SseEmitter emitter = emitters.get(userId);
        if (emitter != null) {
            try {
                emitter.send(SseEmitter.event()
                        .name("notification")
                        .data(response));
            } catch (IOException e) {
                log.warn("Lỗi gửi SSE đến user {}: {}", userId, e.getMessage());
                emitters.remove(userId);
            }
        }
    }

    public SseEmitter subscribe(UUID userId) {
        SseEmitter emitter = new SseEmitter(Long.MAX_VALUE);
        emitters.put(userId, emitter);

        emitter.onCompletion(() -> emitters.remove(userId));
        emitter.onTimeout(() -> emitters.remove(userId));
        emitter.onError(e -> emitters.remove(userId));

        log.info("User {} subscribed to notification SSE", userId);
        return emitter;
    }

    @Transactional(readOnly = true)
    public Page<NotificationResponse> getUserNotifications(UUID userId, Pageable pageable) {
        User user = loadUserOrThrow(userId);
        return notificationRepository.findByUserOrderByCreatedAtDesc(user, pageable)
                .map(this::mapToResponse);
    }

    @Transactional(readOnly = true)
    public List<NotificationResponse> getUnreadNotifications(UUID userId) {
        User user = loadUserOrThrow(userId);

        return notificationRepository.findByUserAndIsReadOrderByCreatedAtDesc(user, false)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public NotificationStatsResponse getNotificationStats(UUID userId) {
        User user = loadUserOrThrow(userId);

        Long totalCount = notificationRepository.countByUserAndIsRead(user, null);
        Long unreadCount = notificationRepository.countUnreadByUser(user);

        List<Notification> recentNotifications = notificationRepository
                .findByUserOrderByCreatedAtDesc(user, Pageable.ofSize(1))
                .getContent();

        OffsetDateTime lastNotificationTime = recentNotifications.isEmpty() ?
                null : recentNotifications.get(0).getCreatedAt();

        return NotificationStatsResponse.builder()
                .totalCount(totalCount)
                .unreadCount(unreadCount)
                .lastNotificationTime(lastNotificationTime)
                .build();
    }

    @Transactional
    public NotificationResponse markAsRead(UUID notificationId, UUID userId) {
        Notification notification = notificationRepository.findById(notificationId)
                .orElseThrow(() -> new AppException(ErrorCode.NOTIFICATION_NOT_FOUND));

        if (!notification.getUser().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        notification.setIsRead(true);
        Notification updatedNotification = notificationRepository.save(notification);

        return mapToResponse(updatedNotification);
    }

    @Transactional
    public void markAllAsRead(UUID userId) {
        User user = loadUserOrThrow(userId);
        int updatedCount = notificationRepository.markAllAsReadByUser(user);
        log.info("Marked {} notifications as read for user {}", updatedCount, userId);
    }

    @Transactional
    public void deleteNotification(UUID notificationId, UUID userId) {
        Notification notification = notificationRepository.findById(notificationId)
                .orElseThrow(() -> new AppException(ErrorCode.NOTIFICATION_NOT_FOUND));

        if (!notification.getUser().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        notificationRepository.delete(notification);
    }

    @Transactional
    public void cleanupOldNotifications(UUID userId, int daysToKeep) {
        User user = loadUserOrThrow(userId);
        OffsetDateTime cutoffDate = OffsetDateTime.now(ZoneOffset.UTC).minusDays(daysToKeep);

        int deletedCount = notificationRepository.deleteOldNotifications(user, cutoffDate);
        log.info("Deleted {} old notifications for user {}", deletedCount, userId);
    }

    /** Gửi các loại thông báo khác nhau **/
    public void sendEventReminderNotification(UUID userId, String eventTitle, OffsetDateTime eventTime, UUID eventId) {
        String formattedTime = eventTime
                .atZoneSameInstant(ZoneId.of("Asia/Ho_Chi_Minh"))
                .format(DateTimeFormatter.ofPattern("HH:mm dd/MM/yyyy"));

        NotificationCreateRequest request = new NotificationCreateRequest();
        request.setTitle("Nhắc nhở sự kiện");
        request.setMessage(String.format("Sự kiện \"%s\" sẽ diễn ra vào %s", eventTitle, formattedTime));
        request.setType(Notification.NotificationType.event_reminder);
        request.setRelatedEntity(Map.of(
                "type", "event",
                "id", eventId.toString()
        ));

        createNotification(userId, request);
    }

    public void sendFamilyTreeInviteNotification(UUID userId, String treeName, UUID treeId, UUID invitedBy) {
        NotificationCreateRequest request = new NotificationCreateRequest();
        request.setTitle("Lời mời tham gia cây gia phả");
        request.setMessage(String.format("Bạn được mời tham gia cây gia phả \"%s\"", treeName));
        request.setType(Notification.NotificationType.invite);
        request.setRelatedEntity(Map.of(
                "type", "family_tree",
                "id", treeId.toString(),
                "invitedBy", invitedBy.toString()
        ));

        createNotification(userId, request);
    }

    public void sendSystemNotification(UUID userId, String title, String message) {
        NotificationCreateRequest request = new NotificationCreateRequest();
        request.setTitle(title);
        request.setMessage(message);
        request.setType(Notification.NotificationType.system);
        createNotification(userId, request);
    }

    private NotificationResponse mapToResponse(Notification notification) {
        NotificationResponse response = new NotificationResponse();
        response.setId(notification.getId());
        response.setUserId(notification.getUser().getId());
        response.setTitle(notification.getTitle());
        response.setMessage(notification.getMessage());
        response.setType(notification.getType());
        response.setIsRead(notification.getIsRead());

        OffsetDateTime createdAt = notification.getCreatedAt() != null
                ? notification.getCreatedAt()
                : OffsetDateTime.now(ZoneOffset.UTC);

        ZoneId vnZone = ZoneId.of("Asia/Ho_Chi_Minh");
        response.setCreatedAt(createdAt.atZoneSameInstant(vnZone).toOffsetDateTime());

        if (notification.getRelatedEntity() != null) {
            response.setRelatedEntity(objectMapper.convertValue(
                    notification.getRelatedEntity(),
                    objectMapper.getTypeFactory().constructMapType(Map.class, String.class, Object.class)
            ));
        }

        return response;
    }
}
