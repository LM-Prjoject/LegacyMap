package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.NotificationUpdateRequest;
import com.legacymap.backend.dto.response.NotificationResponse;
import com.legacymap.backend.dto.response.NotificationStatsResponse;
import com.legacymap.backend.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/notifications")
@RequiredArgsConstructor
public class NotificationController {

    private final NotificationService notificationService;

    @GetMapping
    public ResponseEntity<Page<NotificationResponse>> getNotifications(
            @RequestHeader("X-User-Id") UUID userId,
            Pageable pageable) {

        Page<NotificationResponse> responses = notificationService.getUserNotifications(userId, pageable);
        return ResponseEntity.ok(responses);
    }

    @GetMapping("/unread")
    public ResponseEntity<List<NotificationResponse>> getUnreadNotifications(
            @RequestHeader("X-User-Id") UUID userId) {

        List<NotificationResponse> responses = notificationService.getUnreadNotifications(userId);
        return ResponseEntity.ok(responses);
    }

    @GetMapping("/stats")
    public ResponseEntity<NotificationStatsResponse> getNotificationStats(
            @RequestHeader("X-User-Id") UUID userId) {

        NotificationStatsResponse stats = notificationService.getNotificationStats(userId);
        return ResponseEntity.ok(stats);
    }

    @PutMapping("/{notificationId}/read")
    public ResponseEntity<NotificationResponse> markAsRead(
            @PathVariable UUID notificationId,
            @RequestHeader("X-User-Id") UUID userId) {

        NotificationResponse response = notificationService.markAsRead(notificationId, userId);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/read-all")
    public ResponseEntity<Void> markAllAsRead(
            @RequestHeader("X-User-Id") UUID userId) {

        notificationService.markAllAsRead(userId);
        return ResponseEntity.ok().build();
    }

    @DeleteMapping("/{notificationId}")
    public ResponseEntity<Void> deleteNotification(
            @PathVariable UUID notificationId,
            @RequestHeader("X-User-Id") UUID userId) {

        notificationService.deleteNotification(notificationId, userId);
        return ResponseEntity.ok().build();
    }

    @DeleteMapping("/cleanup")
    public ResponseEntity<Void> cleanupOldNotifications(
            @RequestHeader("X-User-Id") UUID userId,
            @RequestParam(defaultValue = "30") int daysToKeep) {

        notificationService.cleanupOldNotifications(userId, daysToKeep);
        return ResponseEntity.ok().build();
    }
}
