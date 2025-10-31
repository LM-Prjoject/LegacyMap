package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.Notification;
import lombok.Data;

import java.util.Map;

@Data
public class NotificationCreateRequest {
    private String title;
    private String message;
    private Notification.NotificationType type;
    private Map<String, Object> relatedEntity;
}
