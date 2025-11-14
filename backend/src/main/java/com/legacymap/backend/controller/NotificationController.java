package com.legacymap.backend.controller;

import com.legacymap.backend.dto.response.NotificationPageResponse;
import com.legacymap.backend.dto.response.NotificationResponse;
import com.legacymap.backend.dto.response.NotificationStatsResponse;
import com.legacymap.backend.service.NotificationService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/notifications")
@RequiredArgsConstructor
public class NotificationController {

    private final NotificationService notificationService;

    private UUID getCurrentUserId() {
        Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        return UUID.fromString(principal.toString());
    }

    @GetMapping
    public ResponseEntity<NotificationPageResponse> getNotifications(Pageable pageable) {
        UUID userId = getCurrentUserId();
        return ResponseEntity.ok(notificationService.getUserNotificationsWithStats(userId, pageable));
    }

    @GetMapping("/unread")
    public ResponseEntity<List<NotificationResponse>> getUnreadNotifications() {
        return ResponseEntity.ok(notificationService.getUnreadNotifications(getCurrentUserId()));
    }

    @GetMapping("/stats")
    public ResponseEntity<NotificationStatsResponse> getNotificationStats() {
        return ResponseEntity.ok(notificationService.getNotificationStats(getCurrentUserId()));
    }

    @GetMapping(value = "/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter streamNotifications(
            @RequestParam("userId") UUID userId,
            HttpServletRequest request
    ) {
        // Optional: validate JWT in cookie nếu cần
        // String token = extractJwtFromCookie(request);
        // if (token != null && jwtUtil.validateToken(token).equals(userId)) { ... }

        return notificationService.subscribe(userId);
    }

    @GetMapping("/latest")
    public ResponseEntity<List<NotificationResponse>> getLatestNotifications() {
        return ResponseEntity.ok(notificationService.getUserNotifications(getCurrentUserId(), Pageable.ofSize(10)).getContent());
    }

    @PutMapping("/{notificationId}/read")
    public ResponseEntity<NotificationResponse> markAsRead(@PathVariable UUID notificationId) {
        return ResponseEntity.ok(notificationService.markAsRead(notificationId, getCurrentUserId()));
    }

    @PutMapping("/read-all")
    public ResponseEntity<Void> markAllAsRead() {
        notificationService.markAllAsRead(getCurrentUserId());
        return ResponseEntity.ok().build();
    }

    @DeleteMapping("/{notificationId}")
    public ResponseEntity<Void> deleteNotification(@PathVariable UUID notificationId) {
        notificationService.deleteNotification(notificationId, getCurrentUserId());
        return ResponseEntity.ok().build();
    }

    @DeleteMapping("/cleanup")
    public ResponseEntity<Void> cleanupOldNotifications(@RequestParam(defaultValue = "30") int daysToKeep) {
        notificationService.cleanupOldNotifications(getCurrentUserId(), daysToKeep);
        return ResponseEntity.ok().build();
    }
}
