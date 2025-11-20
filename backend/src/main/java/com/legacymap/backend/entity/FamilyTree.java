package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import com.fasterxml.jackson.annotation.JsonIgnore;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "family_trees")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString(exclude = "createdBy") // Tránh infinite loop khi log
public class FamilyTree {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private String name;

    @Column(columnDefinition = "text")
    private String description;

    // CRITICAL: Đảm bảo relationship đúng
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", nullable = false, referencedColumnName = "id")
    @JsonIgnore
    private User createdBy;

    @Column(name = "is_public")
    private Boolean isPublic = false;

    @Column(name = "share_token")
    private UUID shareToken;

    @Column(name = "cover_image_url", columnDefinition = "text")
    private String coverImageUrl;

    @CreationTimestamp
    @Column(name = "created_at", updatable = false)
    private OffsetDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private OffsetDateTime updatedAt;

    @PrePersist
    void prePersist() {
        if (shareToken == null) {
            shareToken = UUID.randomUUID();
        }
        if (isPublic == null) {
            isPublic = false;
        }
    }
}