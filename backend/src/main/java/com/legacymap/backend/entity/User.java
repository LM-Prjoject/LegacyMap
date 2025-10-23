package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "users")
@Data @NoArgsConstructor
@AllArgsConstructor
@Builder
public class User {
        @Id @GeneratedValue
        private UUID id;

        @Column(name = "email", nullable = false, unique = true, length = 255)
        private String email;

        @Column(name = "password_hash", nullable = false, length = 255)
        private String passwordHash;

        @Column(name = "username", nullable = false, unique = true, length = 50)
        private String username;

        @Column(name = "role_name", nullable = false, length = 20)
        private String roleName = "user";

        @Column(name = "is_verified", nullable = false)
        private Boolean isVerified = false;

        @Column(name = "is_active", nullable = false)
        private Boolean isActive = true;

        @Column(name = "last_login")
        private OffsetDateTime lastLogin;

        @Column(name = "created_at")
        private OffsetDateTime createdAt;

        @Column(name = "updated_at")
        private OffsetDateTime updatedAt;

        @Column(name = "provider")
        private String provider;

        @Column(name = "failed_attempts")
        private Integer failedAttempts;

        @PrePersist
        void prePersist() {
                createdAt = OffsetDateTime.now();
                updatedAt = createdAt;
        }

        @PreUpdate
        void preUpdate() {
                updatedAt = OffsetDateTime.now();
        }
}
