package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Data;
import java.time.OffsetDateTime;

@Data
@Builder
public class NotificationStatsResponse {
    private Long totalCount;
    private Long unreadCount;
    private OffsetDateTime lastNotificationTime;
}
