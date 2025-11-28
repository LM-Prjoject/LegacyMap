package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "users")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class User {
        @Id
        @GeneratedValue
        private UUID id;

        @Column(name = "email", nullable = false, unique = true, length = 255)
        private String email;

        @Column(name = "password_hash", nullable = false, length = 255)
        private String passwordHash;

        @Column(name = "username", nullable = false, unique = true, length = 50)
        private String username;

        @Column(name = "role_name", nullable = false, length = 20)
        @Builder.Default
        private String roleName = "user";

        @Column(name = "is_verified", nullable = false)
        @Builder.Default
        private Boolean isVerified = false;

        @Column(name = "is_active", nullable = false)
        @Builder.Default
        private Boolean isActive = true;

        @Column(name = "is_banned", nullable = false)
        @Builder.Default
        private Boolean isBanned = false;

        @Column(name = "banned_at")
        private OffsetDateTime bannedAt;

        @Column(name = "last_login")
        private OffsetDateTime lastLogin;

        @CreationTimestamp
        @Column(name = "created_at", updatable = false)
        private OffsetDateTime createdAt;

        @UpdateTimestamp
        @Column(name = "updated_at")
        private OffsetDateTime updatedAt;

        @Column(name = "provider")
        private String provider;

        @Column(name = "failed_attempts")
        @Builder.Default
        private Integer failedAttempts = 0;

        @Column(name = "lock_until")
        private OffsetDateTime lockUntil;

        @Column(name = "password_changedat")
        private OffsetDateTime passwordChangedAt;

        @Column(name = "password_version", nullable = false)
        @Builder.Default
        private Integer passwordVersion = 0;
}