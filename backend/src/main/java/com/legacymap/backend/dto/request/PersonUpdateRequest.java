package com.legacymap.backend.dto.request;

import lombok.Data;

import java.time.LocalDate;

@Data
public class PersonUpdateRequest {
    private String fullName;
    private String gender;
    private LocalDate birthDate;
    private LocalDate deathDate;
    private String birthPlace;
    private String deathPlace;
    private String biography;
    private String avatarUrl;
    private String email;
    private String phone;
}
