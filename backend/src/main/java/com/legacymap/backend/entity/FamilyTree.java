package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "family_trees")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString(exclude = "createdBy")
public class FamilyTree {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private String name;

    @Column(columnDefinition = "text")
    private String description;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", nullable = false, referencedColumnName = "id")
    @JsonIgnoreProperties({"hibernateLazyInitializer", "handler"})
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

    // ✅ Thêm trường sharePermission
    @Column(name = "share_permission", length = 10)
    @Builder.Default
    private String sharePermission = "view"; // "view" hoặc "edit"

    // ✅ Thêm trường shareUrl không lưu vào DB
    @Transient
    private String shareUrl;

    @PrePersist
    void prePersist() {
        if (shareToken == null) {
            shareToken = UUID.randomUUID();
        }
        if (isPublic == null) {
            isPublic = false;
        }
        if (sharePermission == null) {
            sharePermission = "view";
        }
    }

    // ✅ Thêm method helper để tạo share URL
    public String generateShareUrl(String baseUrl) {
        if (shareToken != null) {
            return baseUrl + "/trees/shared/" + shareToken;
        }
        return null;
    }

    // ✅ THÊM: Đảm bảo có getter cho sharePermission (Lombok đã tạo nhưng explicit thêm nếu cần)
    public String getSharePermission() {
        return sharePermission;
    }

    // ✅ THÊM: Setter cho sharePermission
    public void setSharePermission(String sharePermission) {
        this.sharePermission = sharePermission;
    }
}