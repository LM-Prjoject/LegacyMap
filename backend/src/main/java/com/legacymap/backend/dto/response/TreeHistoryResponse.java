package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Data;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class TreeHistoryResponse {
    private UUID id;
    private String userName;           // Chỉ username, không có object User
    private String userAvatar;         // Chỉ avatar URL, không có UserProfile
    private String action;
    private String entityType;
    private String entityName;
    private String description;
    private String oldValue;
    private String newValue;
    private LocalDateTime createdAt;
}