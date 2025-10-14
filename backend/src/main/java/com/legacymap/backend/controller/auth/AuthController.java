package com.legacymap.backend.controller.auth;

import com.legacymap.backend.dto.request.AuthenticationRequest;
import com.legacymap.backend.dto.request.SetPasswordRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.AuthenticationResponse;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.service.AuthTokenService;
import com.legacymap.backend.service.AuthenticationService;
import com.legacymap.backend.service.UserService;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.*;
import jakarta.servlet.http.HttpServletResponse;

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
    private UserProfileRepository userProfileRepository;

    @Autowired
    private AuthenticationService authenticationService;
    @Autowired
    private PasswordEncoder passwordEncoder;

    @Value("${app.frontend.url:http://localhost:5173}")
    private String frontendUrl;

    public AuthController(AuthTokenRepository authTokenRepository, UserRepository userRepository) {
        this.authTokenRepository = authTokenRepository;
        this.userRepository = userRepository;
    }

    @GetMapping("/verify")
    public void verifyEmail(@RequestParam("token") String token, HttpServletResponse httpResp) throws java.io.IOException {
        AuthToken authToken = authTokenRepository.findByTokenAndType(token, "email_verification")
                .orElseThrow(() -> new RuntimeException("Invalid token"));

        if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
            httpResp.sendRedirect(frontendUrl + "?showLogin=1&err=token_expired");
            return;
        }

        if (Boolean.TRUE.equals(authToken.getUsed())) {
            httpResp.sendRedirect(frontendUrl + "?showLogin=1&err=token_used");
            return;
        }

        User user = authToken.getUser();
        user.setIsVerified(true);
        user.setIsActive(true);
        userRepository.save(user);

        authToken.setUsed(true);
        authTokenRepository.save(authToken);

        // Redirect về FE và mở modal đăng nhập
        String target = frontendUrl + "?showLogin=1";
        httpResp.sendRedirect(target);
    }

    @PostMapping("/login")
    public ApiResponse<AuthenticationResponse> login(@RequestBody AuthenticationRequest request) {
        User user = userService.login(request.getIdentifier(), request.getPassword());

        // 🔥 Tạo session token khi đăng nhập thành công
        AuthToken sessionToken = authTokenService.createSessionToken(user);

        // Build user payload with nested profile (same shape as /auth/me)
        Map<String, Object> userJson = new HashMap<>();
        userJson.put("id", user.getId());
        userJson.put("email", user.getEmail());
        userJson.put("username", user.getUsername());
        userJson.put("roleName", user.getRoleName());
        userJson.put("isActive", user.getIsActive());
        userJson.put("isVerified", user.getIsVerified());

        UserProfile profile = userProfileRepository.findById(user.getId()).orElse(null);
        if (profile != null) {
            Map<String, Object> profileJson = new HashMap<>();
            profileJson.put("fullName", profile.getFullName());
            profileJson.put("clanName", profile.getClanName());
            profileJson.put("gender", profile.getGender());
            profileJson.put("phone", profile.getPhone());
            profileJson.put("dob", profile.getDob());
            profileJson.put("address", profile.getAddress());
            profileJson.put("avatarUrl", profile.getAvatarUrl());
            userJson.put("profile", profileJson);
        }

        AuthenticationResponse response = new AuthenticationResponse(userJson, sessionToken.getToken());
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

    @GetMapping("/me")
    public ResponseEntity<Map<String, Object>> getCurrentUser(@RequestHeader(value = "Authorization", required = false) String authHeader) {
        Map<String, Object> result = new HashMap<>();
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            result.put("error", "Missing or invalid Authorization header");
            return ResponseEntity.status(401).body(result);
        }

        String token = authHeader.substring(7);
        try {
            Jws<Claims> jws = authenticationService.parse(token);
            Claims claims = jws.getBody();
            String sub = claims.getSubject();

            if (sub == null) {
                result.put("error", "Invalid token: missing subject");
                return ResponseEntity.status(401).body(result);
            }

            UUID userId = UUID.fromString(sub);
            User user = userRepository.findById(userId).orElse(null);
            if (user == null) {
                result.put("error", "User not found");
                return ResponseEntity.status(404).body(result);
            }

            UserProfile profile = userProfileRepository.findById(userId).orElse(null);

            Map<String, Object> userJson = new HashMap<>();
            userJson.put("id", user.getId());
            userJson.put("email", user.getEmail());
            userJson.put("username", user.getUsername());
            userJson.put("roleName", user.getRoleName());
            userJson.put("isActive", user.getIsActive());
            userJson.put("isVerified", user.getIsVerified());

            if (profile != null) {
                Map<String, Object> profileJson = new HashMap<>();
                profileJson.put("fullName", profile.getFullName());
                profileJson.put("clanName", profile.getClanName());
                profileJson.put("gender", profile.getGender());
                profileJson.put("phone", profile.getPhone());
                profileJson.put("dob", profile.getDob());
                profileJson.put("address", profile.getAddress());
                profileJson.put("avatarUrl", profile.getAvatarUrl());
                userJson.put("profile", profileJson);
            }

            return ResponseEntity.ok(userJson);
        } catch (Exception ex) {
            result.put("error", "Invalid or expired token");
            return ResponseEntity.status(401).body(result);
        }
    }

}