package com.legacymap.backend.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ShareLinkResponse {
    private String shareLink;
    private String shareToken;
    private Boolean isActive;
    private Instant expiresAt;
    private String accessLevel;
}
