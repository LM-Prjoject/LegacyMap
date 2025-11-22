package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.TreeAccess;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TreeAccessResponse {
    private UUID userId;
    private String userEmail;
    private String userName;
    private String accessLevel; // "view", "edit", "admin"
    private UUID grantedBy;
    private String grantedByEmail;
    private OffsetDateTime grantedAt;

    public static TreeAccessResponse fromEntity(TreeAccess access) {
        if (access == null) {
            return null;
        }

        return TreeAccessResponse.builder()
                .userId(access.getUserId())
                .userEmail(access.getUser() != null ? access.getUser().getEmail() : "Unknown")
                .userName(access.getUser() != null ? access.getUser().getUsername() : "Unknown")
                .accessLevel(access.getAccessLevel())
                .grantedBy(access.getGrantedBy() != null ? access.getGrantedBy().getId() : null)
                .grantedByEmail(access.getGrantedBy() != null ? access.getGrantedBy().getEmail() : "Unknown")
                .grantedAt(access.getGrantedAt())
                .build();
    }
}