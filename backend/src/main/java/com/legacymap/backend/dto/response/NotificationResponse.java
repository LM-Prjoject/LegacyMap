package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.Notification;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
public class NotificationResponse {
    private UUID id;
    private UUID userId;
    private String title;
    private String message;
    private Notification.NotificationType type;
    private Map<String, Object> relatedEntity;
    private Boolean isRead;
    private LocalDateTime createdAt;
}
