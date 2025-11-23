package com.legacymap.backend.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SharedTreeAccessInfoResponse {
    private UUID treeId;
    private String treeName;
    private Boolean canEdit;
    private Boolean canView;
    private String role; // "OWNER", "EDITOR", "VIEWER"
}