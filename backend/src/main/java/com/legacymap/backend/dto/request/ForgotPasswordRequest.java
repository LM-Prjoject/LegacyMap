package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;

public class ForgotPasswordRequest {
    @NotBlank
    @Email
    private String email;

    @NotBlank
    private String redirectTo;

    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
    public String getRedirectTo() { return redirectTo; }
    public void setRedirectTo(String redirectTo) { this.redirectTo = redirectTo; }
}

