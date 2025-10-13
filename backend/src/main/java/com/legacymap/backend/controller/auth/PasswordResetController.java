package com.legacymap.backend.controller.auth;

import com.legacymap.backend.dto.request.ForgotPasswordRequest;
import com.legacymap.backend.dto.request.ResetPasswordRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.service.PasswordResetService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/auth/password")
public class PasswordResetController {

    @Autowired
    private PasswordResetService passwordResetService;

    @PostMapping("/forgot")
    public ResponseEntity<ApiResponse<Void>> forgotPassword(@Valid @RequestBody ForgotPasswordRequest request,
                                                            @RequestParam(value = "redirect", required = false) String redirect) {
        String resetPage = (redirect != null && !redirect.isBlank())
                ? redirect
                : "http://localhost:5173/reset-password.html";
        ApiResponse<Void> res = passwordResetService.requestReset(request.getEmail(), resetPage);
        return ResponseEntity.ok(res);
    }

    @PostMapping("/reset")
    public ResponseEntity<ApiResponse<Void>> resetPassword(@Valid @RequestBody ResetPasswordRequest request) {
        ApiResponse<Void> res = passwordResetService.resetPassword(request.getToken(), request.getNewPassword());
        return ResponseEntity.ok(res);
    }
}
