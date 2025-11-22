package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;

import java.io.Serializable;
import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "tree_access")
@IdClass(TreeAccessId.class)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TreeAccess {

    @Id
    @Column(name = "user_id")
    private UUID userId;

    @Id
    @Column(name = "family_tree_id")
    private UUID familyTreeId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "family_tree_id", insertable = false, updatable = false)
    private FamilyTree familyTree;

    @Column(name = "access_level", nullable = false)
    private String accessLevel; // "view", "edit", "admin"

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "granted_by")
    private User grantedBy;

    @CreationTimestamp
    @Column(name = "granted_at", updatable = false)
    private OffsetDateTime grantedAt;
}