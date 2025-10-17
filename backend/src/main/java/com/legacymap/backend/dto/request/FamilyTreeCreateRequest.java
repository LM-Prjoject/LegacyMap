package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class FamilyTreeCreateRequest {
    @NotBlank
    private String name;
    private String description;
    private Boolean isPublic;
    private String coverImageUrl;
}
