package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.EventReminder;
import lombok.Builder;
import lombok.Data;
import java.time.OffsetDateTime;
import java.util.UUID;

@Builder
@Data
public class EventReminderResponse {
    private UUID id;
    private UUID eventId;
    private UUID userId;
    private EventReminder.SendMethod sendMethod;
    private OffsetDateTime scheduledAt;
    private OffsetDateTime sentAt;
    private EventReminder.ReminderStatus status;
    private OffsetDateTime createdAt;
}


