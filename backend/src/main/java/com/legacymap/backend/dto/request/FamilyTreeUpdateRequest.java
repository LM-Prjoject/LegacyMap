package com.legacymap.backend.dto.request;

import lombok.Data;

@Data
public class FamilyTreeUpdateRequest {
    private String name;
    private String description;
    private Boolean isPublic;
    private String coverImageUrl;
}
