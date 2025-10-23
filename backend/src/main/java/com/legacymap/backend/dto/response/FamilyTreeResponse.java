// src/main/java/com/legacymap/backend/dto/response/FamilyTreeResponse.java
package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.FamilyTree;
import com.fasterxml.jackson.annotation.JsonInclude;
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
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FamilyTreeResponse {
    private UUID id;
    private String name;
    private String description;
    private UUID createdBy;
    private String createdByEmail;
    private String createdByUsername;
    private Boolean isPublic;
    private UUID shareToken;
    private String coverImageUrl;
    private OffsetDateTime createdAt;
    private OffsetDateTime updatedAt;

    // ✅ Static factory method để convert từ Entity sang DTO
    public static FamilyTreeResponse fromEntity(FamilyTree tree) {
        if (tree == null) {
            return null;
        }

        return FamilyTreeResponse.builder()
                .id(tree.getId())
                .name(tree.getName())
                .description(tree.getDescription())
                .createdBy(tree.getCreatedBy() != null ? tree.getCreatedBy().getId() : null)
                .createdByEmail(tree.getCreatedBy() != null ? tree.getCreatedBy().getEmail() : "Unknown")
                .createdByUsername(tree.getCreatedBy() != null ? tree.getCreatedBy().getUsername() : "Unknown")
                .isPublic(tree.getIsPublic())
                .shareToken(tree.getShareToken())
                .coverImageUrl(tree.getCoverImageUrl())
                .createdAt(tree.getCreatedAt())
                .updatedAt(tree.getUpdatedAt())
                .build();
    }
}