package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Data;

import java.time.OffsetDateTime;
import java.util.UUID;

@Data
@Builder
public class UserListResponse {
    private UUID id;
    private String email;
    private String username;
    private String roleName;
    private Boolean isVerified;
    private Boolean isActive;
    private Boolean isBanned;
    private OffsetDateTime bannedAt;
    private OffsetDateTime lastLogin;
    private OffsetDateTime createdAt;
}