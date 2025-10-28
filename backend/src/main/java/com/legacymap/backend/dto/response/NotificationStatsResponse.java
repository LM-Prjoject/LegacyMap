package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Data;
import java.time.LocalDateTime;

@Data
@Builder
public class NotificationStatsResponse {
    private Long totalCount;
    private Long unreadCount;
    private LocalDateTime lastNotificationTime;
}
