package com.legacymap.backend.controller.auth;

import com.legacymap.backend.dto.request.AuthenticationRequest;
import com.legacymap.backend.dto.request.ChangePasswordRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.AuthenticationResponse;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.service.AuthenticationService;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import jakarta.servlet.http.HttpServletResponse;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {

    private final AuthTokenRepository authTokenRepository;
    private final UserRepository userRepository;
    private final UserProfileRepository userProfileRepository;
    private final AuthenticationService authenticationService;

    @Value("${app.frontend.url}")
    private String frontendUrl;

    @GetMapping("/verify")
    public void verifyEmail(@RequestParam("token") String token, HttpServletResponse httpResp) throws java.io.IOException {
        log.info("Email verification attempt for token: {}", token);

        var optToken = authTokenRepository.findByTokenAndType(token, "email_verification");
        if (optToken.isEmpty()) {
            log.warn("Verification token not found: {}", token);
            httpResp.sendRedirect(frontendUrl + "?showLogin=1&err=invalid_token");
            return;
        }
        AuthToken authToken = optToken.get();

        if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
            log.warn("Verification token expired: {}", token);
            httpResp.sendRedirect(frontendUrl + "?showLogin=1&err=token_expired");
            return;
        }

        if (Boolean.TRUE.equals(authToken.getUsed())) {
            log.warn("Verification token already used: {}", token);
            httpResp.sendRedirect(frontendUrl + "?showLogin=1&err=token_used");
            return;
        }

        User user = authToken.getUser();
        user.setIsVerified(true);
        user.setIsActive(true);
        userRepository.save(user);

        authToken.setUsed(true);
        authTokenRepository.save(authToken);

        log.info("Email verified successfully for user: {}", user.getEmail());

        // Redirect về FE và mở modal đăng nhập
        String target = frontendUrl + "?showLogin=1";
        httpResp.sendRedirect(target);
    }

    @PostMapping("/login")
    public ApiResponse<AuthenticationResponse> login(@RequestBody AuthenticationRequest request) {
        try {
            AuthenticationResponse response = authenticationService.login(
                    request.getIdentifier(),
                    request.getPassword()
            );
            return ApiResponse.success(response, "Login successful");

        } catch (Exception e) {
            log.error(request.getIdentifier(), e.getMessage());
            throw e;
        }
    }

    private Optional<User> findUserByIdentifier(String identifier) {
        // Thử tìm bằng email trước
        Optional<User> userOpt = userRepository.findByEmail(identifier);
        if (userOpt.isPresent()) {
            return userOpt;
        }
        // Nếu không tìm thấy, thử bằng username
        return userRepository.findByUsername(identifier);
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

            userJson.put("provider", user.getProvider());

            OffsetDateTime changedAt = user.getPasswordChangedAt();
            userJson.put(
                    "passwordChangedAt",
                    changedAt != null ? changedAt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME) : null
            );

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

    @PostMapping(value = "/change-password", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ApiResponse<Void> changePassword(@Valid @RequestBody ChangePasswordRequest request) {
        return authenticationService.changePassword(request);
    }
}