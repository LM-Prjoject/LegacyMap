package com.legacymap.backend.family.entity;

import jakarta.persistence.*;
import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
@Entity

@Table(name = "users")
public class User {
        @Id
        @GeneratedValue
        private UUID id;
        private String email;
        private String passwordHash;
        private String username;
        private String roleName;
        private Boolean isVerified;
        private Boolean isActive;
        private LocalDate lastLogin;
        private LocalDate createdAt;
        private LocalDate updatedAt;
}
