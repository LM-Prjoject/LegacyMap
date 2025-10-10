package com.legacymap.backend.controller.auth;

import com.legacymap.backend.dto.request.LoginRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.LoginResponse;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.AuthTokenService;
import com.legacymap.backend.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;

@RestController
@RequestMapping("/auth")
public class AuthController {

    @Autowired
    private AuthTokenRepository authTokenRepository;

    @Autowired
    private UserService userService;

    @Autowired
    private AuthTokenService authTokenService;

    @Autowired
    private UserRepository userRepository;

    public AuthController(AuthTokenRepository authTokenRepository, UserRepository userRepository) {
        this.authTokenRepository = authTokenRepository;
        this.userRepository = userRepository;
    }

    @GetMapping("/verify")
    public ResponseEntity<String> verifyEmail(@RequestParam("token") String token) {
        AuthToken authToken = authTokenRepository.findByTokenAndType(token, "email_verification")
                .orElseThrow(() -> new RuntimeException("Invalid token"));

        if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
            return ResponseEntity.badRequest().body("Token expired");
        }

        if (Boolean.TRUE.equals(authToken.getUsed())) {
            return ResponseEntity.badRequest().body("Token already used");
        }

        User user = authToken.getUser();
        user.setIsVerified(true);
        user.setIsActive(true);
        userRepository.save(user);

        authToken.setUsed(true);
        authTokenRepository.save(authToken);

        return ResponseEntity.ok("Email verified successfully!");
    }

    @PostMapping("/login")
    public ApiResponse<LoginResponse> login(@RequestBody LoginRequest request) {
        User user = userService.login(request.getIdentifier(), request.getPassword());

        // 🔥 Tạo session token khi đăng nhập thành công
        AuthToken sessionToken = authTokenService.createSessionToken(user);

        LoginResponse response = new LoginResponse(user, sessionToken.getToken());
        return ApiResponse.success(response);
    }
}
