package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class UnbanRequestCreateRequest {

    @NotBlank
    private String identifier;

    @NotBlank
    private String reason;
}