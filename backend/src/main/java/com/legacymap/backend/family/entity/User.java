package com.legacymap.backend.family.entity;

import jakarta.persistence.*;
import lombok.Data;

import java.time.OffsetDateTime;
import java.util.UUID;

@Data
@Entity
@Table(name = "users")
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
