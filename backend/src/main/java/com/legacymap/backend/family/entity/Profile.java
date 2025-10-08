package com.legacymap.backend.family.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.Data;

import java.time.LocalDate;
@Data
@Entity
public class Profile {
        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        private String id;
        private String username;
        private String password;
        private String fullName;
        private String email;
        private String clanName;
        private String gender;
        private String phone;
        private LocalDate dob;
        private String city;
        private String ward;
        private String houseNumber;
}
