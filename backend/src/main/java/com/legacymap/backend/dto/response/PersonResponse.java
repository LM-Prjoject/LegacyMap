package com.legacymap.backend.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PersonResponse {
    private UUID id;
    private String fullName;
    private String gender;
    private LocalDate birthDate;
    private String birthPlace;
    private LocalDate deathDate;
    private String deathPlace;
    private String email;
    private String phone;
    private String avatarUrl;
    private String biography;

    // Chỉ trả về treeId, không trả toàn bộ FamilyTree object
    private UUID familyTreeId;
}