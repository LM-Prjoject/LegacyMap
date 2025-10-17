package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "family_trees")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FamilyTree {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private String name;

    @Column(columnDefinition = "text")
    private String description;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", nullable = false)
    private User createdBy;

    @Column(name = "is_public")
    private Boolean isPublic = false;

    @Column(name = "share_token")
    private UUID shareToken;

    @Column(name = "cover_image_url", columnDefinition = "text")
    private String coverImageUrl;

    @Column(name = "created_at")
    private OffsetDateTime createdAt;

    @Column(name = "updated_at")
    private OffsetDateTime updatedAt;

    @PrePersist
    void prePersist() {
        createdAt = OffsetDateTime.now();
        updatedAt = createdAt;
        if (shareToken == null) {
            shareToken = UUID.randomUUID();
        }
    }

    @PreUpdate
    void preUpdate() {
        updatedAt = OffsetDateTime.now();
    }
}
