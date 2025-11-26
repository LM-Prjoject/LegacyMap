package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.ChangePasswordRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.AuthenticationResponse;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.repository.UserRepository;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.crypto.SecretKey;
import jakarta.servlet.http.HttpServletRequest;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.*;
import lombok.extern.slf4j.Slf4j;  // ✅ THÊM IMPORT NÀY

@Slf4j
@Service
public class AuthenticationService {

    private final UserRepository userRepository;
    private final UserProfileRepository userProfileRepository;
    private final PasswordEncoder passwordEncoder;
    private final UserSessionService userSessionService;  // ✅ THÊM DÒNG NÀY
    private final JwtUtil jwtUtil;  // ✅ THÊM DÒNG NÀY
    private final SecretKey key;
    private final long accessTtlMillis;

    public AuthenticationService(
            UserRepository userRepository,
            UserProfileRepository userProfileRepository,
            PasswordEncoder passwordEncoder,
            UserSessionService userSessionService,  // ✅ THÊM THAM SỐ NÀY
            JwtUtil jwtUtil,  // ✅ THÊM THAM SỐ NÀY
            @Value("${app.jwt.secret}") String secret,
            @Value("${app.jwt.access-ttl-ms:604800000}") long accessTtlMillis
    ) {
        this.userRepository = userRepository;
        this.userProfileRepository = userProfileRepository;
        this.passwordEncoder = passwordEncoder;
        this.userSessionService = userSessionService;  // ✅ THÊM DÒNG NÀY
        this.jwtUtil = jwtUtil;  // ✅ THÊM DÒNG NÀY
        this.key = Keys.hmacShaKeyFor(secret.getBytes());
        this.accessTtlMillis = accessTtlMillis;
    }

    public AuthenticationResponse login(String identifier, String password, HttpServletRequest request) {
        try {
            if (password == null || password.trim().isEmpty()) {
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            Optional<User> userOpt = findUserByIdentifier(identifier);
            if (userOpt.isEmpty()) {
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            User user = userOpt.get();

            if ("google".equalsIgnoreCase(user.getProvider())) {
                throw new AppException(ErrorCode.OAUTH_GOOGLE_ONLY);
            }

            if (user.getPasswordHash() == null || user.getPasswordHash().trim().isEmpty()) {
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            boolean passwordMatches = passwordEncoder.matches(password, user.getPasswordHash());
            if (!passwordMatches) {
                boolean lockedNow = increaseFailedAttempts(user);
                if (lockedNow) {
                    throw new AppException(ErrorCode.ACCOUNT_DISABLED);
                }
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            if (Boolean.TRUE.equals(user.getIsBanned())) {
                throw new AppException(ErrorCode.USER_BANNED);
            }
            if (Boolean.FALSE.equals(user.getIsActive())) {
                throw new AppException(ErrorCode.ACCOUNT_DISABLED);
            }
            if (Boolean.FALSE.equals(user.getIsVerified())) {
                throw new AppException(ErrorCode.ACCOUNT_NOT_VERIFIED);
            }

            resetFailedAttempts(user);
            user.setLastLogin(java.time.OffsetDateTime.now());
            userRepository.save(user);

            String token = generateAccessToken(user);

            // ✅ Tạo session trong database
            try {
                userSessionService.createSession(user.getId(), token, request);
                log.info("✅ Session created for user: {}", user.getEmail());
            } catch (Exception e) {
                log.error("❌ Failed to create session: {}", e.getMessage());
            }

            Map<String, Object> userJson = new HashMap<>();
            userJson.put("id", user.getId());
            userJson.put("email", user.getEmail());
            userJson.put("username", user.getUsername());
            userJson.put("role", user.getRoleName());
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

            return new AuthenticationResponse(userJson, token);

        } catch (AppException e) {
            throw e;
        } catch (Exception e) {
            throw new AppException(ErrorCode.UNAUTHENTICATED);
        }
    }

    private Optional<User> findUserByIdentifier(String identifier) {
        Optional<User> userOpt = userRepository.findByEmail(identifier);
        if (userOpt.isPresent()) return userOpt;
        return userRepository.findByUsername(identifier);
    }

    public String generateAccessToken(User user) {
        Integer pwdv = user.getPasswordVersion() != null ? user.getPasswordVersion() : 0;
        return jwtUtil.generateToken(user.getId(), user.getEmail(), user.getRoleName(), pwdv); // ✅ Thêm pwdv
    }

    public Jws<Claims> parse(String jwt) {
        return Jwts.parserBuilder().setSigningKey(key).build().parseClaimsJws(jwt);
    }

    public boolean increaseFailedAttempts(User user) {
        int newAttempts = (user.getFailedAttempts() == null ? 0 : user.getFailedAttempts()) + 1;
        user.setFailedAttempts(newAttempts);

        boolean lockedNow = newAttempts >= 3;
        if (lockedNow) {
            user.setIsActive(false);
        }
        userRepository.save(user);
        return lockedNow;
    }

    public void resetFailedAttempts(User user) {
        user.setFailedAttempts(0);
        userRepository.save(user);
    }

    @Transactional
    public ApiResponse<Void> changePassword(ChangePasswordRequest request) {
        UUID currentUserId = getCurrentUserIdFromContext();
        User user = userRepository.findById(currentUserId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        if (Boolean.TRUE.equals(user.getIsBanned())) {
            throw new AppException(ErrorCode.USER_BANNED);
        }
        if (Boolean.FALSE.equals(user.getIsActive())) {
            throw new AppException(ErrorCode.ACCOUNT_DISABLED);
        }

        if (user.getPasswordHash() == null ||
                !passwordEncoder.matches(request.getCurrentPassword(), user.getPasswordHash())) {
            return ApiResponse.error(ErrorCode.INVALID_PASSWORD);
        }

        if (passwordEncoder.matches(request.getNewPassword(), user.getPasswordHash())) {
            return ApiResponse.error(ErrorCode.BAD_REQUEST, "Mật khẩu mới không được trùng với mật khẩu cũ");
        }

        user.setPasswordHash(passwordEncoder.encode(request.getNewPassword()));
        user.setFailedAttempts(0);
        user.setIsActive(true);
        user.setPasswordChangedAt(OffsetDateTime.now());
        user.setPasswordVersion((user.getPasswordVersion() == null ? 0 : user.getPasswordVersion()) + 1);

        userRepository.save(user);
        return ApiResponse.success(null, "Đổi mật khẩu thành công");
    }

    private UUID getCurrentUserIdFromContext() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }
        try {
            return UUID.fromString(auth.getName());
        } catch (IllegalArgumentException e) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }
    }

    @Transactional
    public void updateUserActivity(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        user.setLastLogin(OffsetDateTime.now());
        userRepository.save(user);

        log.debug("🔄 Updated lastLogin for user: {}", email);
    }
}