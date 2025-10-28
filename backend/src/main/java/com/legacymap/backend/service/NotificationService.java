package com.legacymap.backend.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.dto.request.NotificationCreateRequest;
import com.legacymap.backend.dto.request.NotificationUpdateRequest;
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class NotificationService {

    private final NotificationRepository notificationRepository;
    private final UserRepository userRepository;
    private final ObjectMapper objectMapper;

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
                .type(request.getType() != null ? request.getType() : Notification.NotificationType.SYSTEM)
                .isRead(false)
                .build();

        try {
            if (request.getRelatedEntity() != null) {
                notification.setRelatedEntity(objectMapper.writeValueAsString(request.getRelatedEntity()));
            }
        } catch (JsonProcessingException e) {
            log.error("Error parsing related entity JSON", e);
            throw new AppException(ErrorCode.INVALID_INPUT_DATA);
        }

        Notification savedNotification = notificationRepository.save(notification);
        log.info("Created notification for user {}: {}", userId, request.getTitle());

        return mapToResponse(savedNotification);
    }

    @Transactional
    public void createNotificationForUsers(List<UUID> userIds, NotificationCreateRequest request) {
        for (UUID userId : userIds) {
            try {
                createNotification(userId, request);
            } catch (Exception e) {
                log.error("Failed to create notification for user {}", userId, e);
            }
        }
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

        LocalDateTime lastNotificationTime = recentNotifications.isEmpty() ?
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
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(daysToKeep);

        int deletedCount = notificationRepository.deleteOldNotifications(user, cutoffDate);
        log.info("Deleted {} old notifications for user {}", deletedCount, userId);
    }

    public void sendEventReminderNotification(UUID userId, String eventTitle, LocalDateTime eventTime, UUID eventId) {
        NotificationCreateRequest request = new NotificationCreateRequest();
        request.setTitle("Nhắc nhở sự kiện");
        request.setMessage(String.format("Sự kiện \"%s\" sẽ diễn ra vào %s",
                eventTitle, eventTime.format(java.time.format.DateTimeFormatter.ofPattern("HH:mm dd/MM/yyyy"))));
        request.setType(Notification.NotificationType.EVENT_REMINDER);
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
        request.setType(Notification.NotificationType.INVITE);
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
        request.setType(Notification.NotificationType.SYSTEM);

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
        response.setCreatedAt(notification.getCreatedAt());

        try {
            if (notification.getRelatedEntity() != null) {
                response.setRelatedEntity(objectMapper.readValue(
                        notification.getRelatedEntity(),
                        objectMapper.getTypeFactory().constructMapType(Map.class, String.class, Object.class)
                ));
            }
        } catch (JsonProcessingException e) {
            log.error("Error parsing related entity JSON for notification {}", notification.getId(), e);
        }

        return response;
    }
}
