package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Data;

import java.time.OffsetDateTime;
import java.util.UUID;

@Data
@Builder
public class PersonClaimResponse {
    private UUID personId;
    private String personFullName;
    private UUID familyTreeId;
    private String linkType; // self|relative|manager
    private OffsetDateTime invitedAt; // linkedAt
}
