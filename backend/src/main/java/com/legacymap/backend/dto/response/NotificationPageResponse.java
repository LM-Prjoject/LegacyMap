package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Data;
import java.util.List;

@Data
@Builder
public class NotificationPageResponse {
    private List<NotificationResponse> content;
    private int page;
    private int size;
    private long totalElements;
    private int totalPages;
    private boolean last;
    private long unreadCount;
}