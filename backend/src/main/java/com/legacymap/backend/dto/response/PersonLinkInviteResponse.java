package com.legacymap.backend.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PersonLinkInviteResponse {
    private String status; // "APPROVED", "PENDING"
    private String message;
}
