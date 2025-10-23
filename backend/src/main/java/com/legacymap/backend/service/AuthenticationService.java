package com.legacymap.backend.service;

import com.legacymap.backend.dto.response.AuthenticationResponse;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.entity.UserProfile;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@Slf4j
@Service
public class AuthenticationService {

    private final UserRepository userRepository;
    private final UserProfileRepository userProfileRepository;
    private final PasswordEncoder passwordEncoder;
    private final SecretKey key;
    private final long accessTtlMillis;

    public AuthenticationService(
            UserRepository userRepository,
            UserProfileRepository userProfileRepository,
            PasswordEncoder passwordEncoder,
            @Value("${app.jwt.secret}") String secret,
            @Value("${app.jwt.access-ttl-ms:604800000}") long accessTtlMillis
    ) {
        this.userRepository = userRepository;
        this.userProfileRepository = userProfileRepository;
        this.passwordEncoder = passwordEncoder;
        this.key = Keys.hmacShaKeyFor(secret.getBytes());
        this.accessTtlMillis = accessTtlMillis;

        log.info("✅ AuthenticationService initialized with JWT configuration");
    }

    /**
     * Method login xử lý đăng nhập - hỗ trợ cả email và username
     * ✅ RETURN: AuthenticationResponse thay vì String
     */
    public AuthenticationResponse login(String identifier, String password) {
        log.info("=== 🔐 LOGIN ATTEMPT ===");
        log.info("Identifier: {}", identifier);

        try {
            // Kiểm tra password null hoặc empty
            if (password == null || password.trim().isEmpty()) {
                log.error("❌ PASSWORD IS NULL OR EMPTY");
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            // 1. Tìm user bằng email hoặc username
            Optional<User> userOpt = findUserByIdentifier(identifier);
            log.info("User found in DB: {}", userOpt.isPresent());

            if (userOpt.isEmpty()) {
                log.error("❌ USER NOT FOUND IN DATABASE: {}", identifier);
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            User user = userOpt.get();
            log.info("✅ User details - ID: {}, Active: {}, Verified: {}, Banned: {}, Role: {}",
                    user.getId(), user.getIsActive(), user.getIsVerified(), user.getIsBanned(), user.getRoleName());

            // Kiểm tra password hash tồn tại
            if (user.getPasswordHash() == null || user.getPasswordHash().trim().isEmpty()) {
                log.error("❌ NO PASSWORD HASH IN DATABASE FOR USER: {}", identifier);
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            // 2. Kiểm tra password
            log.info("🔑 Starting password check...");
            boolean passwordMatches = passwordEncoder.matches(password, user.getPasswordHash());
            log.info("🔑 PasswordEncoder.matches() result: {}", passwordMatches);

            if (!passwordMatches) {
                log.error("❌ PASSWORD MISMATCH - Check failed");
                throw new AppException(ErrorCode.INVALID_CREDENTIALS);
            }

            // 3. Kiểm tra trạng thái tài khoản
            log.info("📋 Checking account status...");

            // 🔥 CHECK BANNED - THÊM MỚI
            if (Boolean.TRUE.equals(user.getIsBanned())) {
                log.error("🚫 ACCOUNT IS BANNED - Login denied for user: {}", identifier);
                throw new AppException(ErrorCode.USER_BANNED);
            }

            if (Boolean.FALSE.equals(user.getIsActive())) {
                log.error("❌ ACCOUNT INACTIVE");
                throw new AppException(ErrorCode.ACCOUNT_DISABLED);
            }

            if (Boolean.FALSE.equals(user.getIsVerified())) {
                log.error("❌ ACCOUNT NOT VERIFIED");
                throw new AppException(ErrorCode.ACCOUNT_NOT_VERIFIED);
            }

            log.info("✅ All checks passed - Generating token...");
            String token = generateAccessToken(user);

            // ✅ Tạo user map với đầy đủ thông tin
            Map<String, Object> userJson = new HashMap<>();
            userJson.put("id", user.getId());
            userJson.put("email", user.getEmail());
            userJson.put("username", user.getUsername());
            userJson.put("role", user.getRoleName());
            userJson.put("roleName", user.getRoleName());
            userJson.put("isActive", user.getIsActive());
            userJson.put("isVerified", user.getIsVerified());

            // Thêm profile nếu có
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

            AuthenticationResponse response = new AuthenticationResponse(userJson, token);

            log.info("✅ LOGIN SUCCESSFUL - Token generated for role: {}", user.getRoleName());
            log.info("=== 🔐 LOGIN DEBUG END ===");

            return response;

        } catch (AppException e) {
            log.error("💥 LOGIN FAILED: {}", e.getErrorCode().getMessage());
            throw e;
        } catch (Exception e) {
            log.error("💥 UNEXPECTED LOGIN EXCEPTION: {}", e.getMessage());
            log.error("💥 Exception type: {}", e.getClass().getName());
            log.info("=== 🔐 LOGIN DEBUG END WITH ERROR ===");
            throw new AppException(ErrorCode.UNAUTHENTICATED);
        }
    }

    /**
     * Tìm user bằng email hoặc username
     */
    private Optional<User> findUserByIdentifier(String identifier) {
        // Thử tìm bằng email trước
        Optional<User> userOpt = userRepository.findByEmail(identifier);
        if (userOpt.isPresent()) {
            log.info("🔍 User found by email: {}", identifier);
            return userOpt;
        }
        // Nếu không tìm thấy, thử bằng username
        userOpt = userRepository.findByUsername(identifier);
        if (userOpt.isPresent()) {
            log.info("🔍 User found by username: {}", identifier);
        } else {
            log.info("🔍 User not found by email or username: {}", identifier);
        }
        return userOpt;
    }

    /**
     * Generate access token for authenticated user
     */
    public String generateAccessToken(User user) {
        Instant now = Instant.now();
        String token = Jwts.builder()
                .setSubject(user.getId().toString())
                .claim("email", user.getEmail())
                .claim("role", user.getRoleName())
                .claim("purpose", "ACCESS")
                .setIssuedAt(Date.from(now))
                .setExpiration(Date.from(now.plusMillis(accessTtlMillis)))
                .signWith(key, SignatureAlgorithm.HS256)
                .compact();

        log.info("🎫 Generated JWT for user: {}, role: {}", user.getEmail(), user.getRoleName());
        return token;
    }

    public Jws<Claims> parse(String jwt) {
        return Jwts.parserBuilder().setSigningKey(key).build().parseClaimsJws(jwt);
    }
}