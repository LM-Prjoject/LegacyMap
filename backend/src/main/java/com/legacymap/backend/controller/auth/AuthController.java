package com.legacymap.backend.controller.auth;

import com.legacymap.backend.dto.request.AuthenticationRequest;
import com.legacymap.backend.dto.request.SetPasswordRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.AuthenticationResponse;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.AuthTokenService;
import com.legacymap.backend.service.AuthenticationService;
import com.legacymap.backend.service.UserService;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/auth")
public class AuthController {

    @Autowired
    private AuthTokenRepository authTokenRepository;

    @Autowired
    private UserService userService;

    @Autowired
    private AuthTokenService authTokenService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AuthenticationService authenticationService;
    @Autowired
    private PasswordEncoder passwordEncoder;

    public AuthController(AuthTokenRepository authTokenRepository, UserRepository userRepository) {
        this.authTokenRepository = authTokenRepository;
        this.userRepository = userRepository;
    }

    @GetMapping("/verify")
    public ResponseEntity<ApiResponse<Map<String, String>>> verifyEmail(@RequestParam("token") String token) {
        AuthToken authToken = authTokenRepository.findByTokenAndType(token, "email_verification")
                .orElseThrow(() -> new RuntimeException("Invalid token"));

        if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
            return ResponseEntity.badRequest().body(ApiResponse.error(ErrorCode.INVALID_TOKEN, "Token expired"));
        }

        if (Boolean.TRUE.equals(authToken.getUsed())) {
            return ResponseEntity.badRequest().body(ApiResponse.error(ErrorCode.INVALID_TOKEN, "Token already used"));
        }

        User user = authToken.getUser();
        user.setIsVerified(true);
        user.setIsActive(true);
        userRepository.save(user);

        authToken.setUsed(true);
        authTokenRepository.save(authToken);

        Map<String, String> data = new HashMap<>();
        data.put("message", "Email verified successfully!");
        data.put("redirectUrl", "/"); // Chuyển hướng về trang chủ

        return ResponseEntity.ok(ApiResponse.success(data, "success"));
    }

    @PostMapping("/login")
    public ApiResponse<AuthenticationResponse> login(@RequestBody AuthenticationRequest request) {
        User user = userService.login(request.getIdentifier(), request.getPassword());

        // 🔥 Tạo session token khi đăng nhập thành công
        AuthToken sessionToken = authTokenService.createSessionToken(user);

        AuthenticationResponse response = new AuthenticationResponse(user, sessionToken.getToken());
        return ApiResponse.success(response, "success");
    }

    @PostMapping("/set-password")
    public ResponseEntity<ApiResponse<Void>> setPassword(@Valid @RequestBody SetPasswordRequest req) {
        Jws<Claims> jws = authenticationService.parse(req.getToken());
        Claims c = jws.getBody();

        if (!"SET_PASSWORD".equals(c.get("purpose"))) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(ErrorCode.INVALID_PURPOSE, "Invalid purpose"));
        }

        UUID userId = UUID.fromString(c.getSubject());
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new RuntimeException("User not found"));

        user.setPasswordHash(passwordEncoder.encode(req.getNewPassword()));
        userRepository.save(user);

        return ResponseEntity.ok(ApiResponse.success(null, "Password set successfully"));
    }

}