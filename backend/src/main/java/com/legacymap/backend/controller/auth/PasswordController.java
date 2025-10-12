package com.legacymap.backend.controller.auth;

import com.legacymap.backend.service.SupabaseAuthService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/auth")
///api/auth
public class PasswordController {

    public record ForgotPasswordRequest(
            @NotBlank @Email String email,
            @NotBlank String redirectTo
    ) {}

    public record ResetPasswordRequest(
            @NotBlank String accessToken,
            @NotBlank String newPassword
    ) {}

    private final SupabaseAuthService auth;

    public PasswordController(SupabaseAuthService auth) {
        this.auth = auth;
    }

    @PostMapping("/forgot-password")
    public ResponseEntity<?> forgotPassword(@Valid @RequestBody ForgotPasswordRequest req) {
        auth.sendPasswordResetEmail(req.email(), req.redirectTo());
        return ResponseEntity.ok().build();
    }

    @PostMapping("/reset-password")
    public ResponseEntity<?> resetPassword(@Valid @RequestBody ResetPasswordRequest req) {
        auth.updatePasswordWithAccessToken(req.accessToken(), req.newPassword());
        return ResponseEntity.ok().build();
    }
}
