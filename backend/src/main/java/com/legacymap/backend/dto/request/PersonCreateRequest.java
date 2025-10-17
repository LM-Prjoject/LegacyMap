package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.time.LocalDate;

@Data
public class PersonCreateRequest {
    @NotBlank
    private String fullName;
    private String gender;
    private LocalDate birthDate;
    private LocalDate deathDate;
    private String birthPlace;
    private String deathPlace;
    private String biography;
    private String avatarUrl;
}
