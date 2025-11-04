package com.legacymap.backend.dto.request;

import com.legacymap.backend.enums.RelationshipType;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.UUID;

@Data
public class RelationshipCreateRequest {
    @NotNull
    private UUID person1Id;
    @NotNull
    private UUID person2Id;
    @NotNull
    private RelationshipType relationshipType;
    private String notes;
}
