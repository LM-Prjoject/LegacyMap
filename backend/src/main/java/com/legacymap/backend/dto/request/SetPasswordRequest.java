package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class SetPasswordRequest {
    @NotBlank
    String token;
    @NotBlank
    private String newPassword;
}
