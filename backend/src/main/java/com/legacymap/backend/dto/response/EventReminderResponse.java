package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.EventReminder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class EventReminderResponse {
    private UUID id;
    private UUID eventId;
    private UUID userId;
    private EventReminder.SendMethod sendMethod;
    private LocalDateTime scheduledAt;
    private LocalDateTime sentAt;
    private EventReminder.ReminderStatus status;
    private LocalDateTime createdAt;
}
