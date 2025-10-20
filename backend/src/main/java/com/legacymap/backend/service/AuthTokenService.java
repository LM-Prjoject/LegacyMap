package com.legacymap.backend.service;

import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.AuthTokenRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthTokenService {

    private final AuthTokenRepository authTokenRepository;

    /**
     * Tạo session token cho user (dùng cho login)
     */
    public AuthToken createSessionToken(User user) {
        String token = generateRandomToken();
        OffsetDateTime expiresAt = OffsetDateTime.now().plusDays(7);

        AuthToken authToken = AuthToken.builder()
                .user(user)
                .token(token)
                .type("session")  // ✅ GIỮ NGUYÊN "session" - match với DB constraint
                .expiresAt(expiresAt)
                .used(false)
                .build();

        log.info("✅ Created session token for user: {}", user.getId());
        return authTokenRepository.save(authToken);
    }

    /**
     * Tạo email verification token
     */
    public AuthToken createEmailVerificationToken(User user) {
        AuthToken token = AuthToken.builder()
                .user(user)
                .type("email_verification")  // ✅ GIỮ NGUYÊN - match với DB constraint
                .token(UUID.randomUUID().toString().replace("-", ""))
                .expiresAt(OffsetDateTime.now().plusMinutes(5))
                .used(false)
                .build();

        log.info("📧 Created email verification token for user: {}", user.getEmail());
        return authTokenRepository.save(token);
    }

    /**
     * 🔥 Validate session/access token và trả về userId
     */
    @Transactional(readOnly = true)
    public UUID validateAccessToken(String token) {
        try {
            log.debug("🔍 Validating token: {}...", token.substring(0, Math.min(20, token.length())));

            // Tìm token trong database
            Optional<AuthToken> authTokenOpt = authTokenRepository.findByToken(token);

            if (authTokenOpt.isEmpty()) {
                log.warn("❌ Token not found in database");
                return null;
            }

            AuthToken authToken = authTokenOpt.get();

            // ✅ Kiểm tra loại token - chấp nhận "session" (đây là access token)
            if (!"session".equalsIgnoreCase(authToken.getType())) {
                log.warn("❌ Invalid token type: {} (expected session)", authToken.getType());
                return null;
            }

            // Kiểm tra expired
            if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
                log.warn("❌ Token expired at: {}", authToken.getExpiresAt());
                return null;
            }

            // Kiểm tra used/revoked
            if (Boolean.TRUE.equals(authToken.getUsed())) {
                log.warn("❌ Token has been used/revoked");
                return null;
            }

            // Token hợp lệ, trả về userId
            UUID userId = authToken.getUser().getId();
            log.info("✅ Token valid for user: {}", userId);
            return userId;

        } catch (Exception e) {
            log.error("💥 Error validating token: {}", e.getMessage(), e);
            return null;
        }
    }

    /**
     * Generate random token string
     */
    private String generateRandomToken() {
        return UUID.randomUUID().toString().replace("-", "") +
                Long.toHexString(ThreadLocalRandom.current().nextLong());
    }

    /**
     * 🔥 Revoke token (cho logout)
     */
    @Transactional
    public void revokeToken(String token) {
        authTokenRepository.findByToken(token).ifPresent(authToken -> {
            authToken.setUsed(true);
            authTokenRepository.save(authToken);
            log.info("🔒 Token revoked for user: {}", authToken.getUser().getId());
        });
    }

    /**
     * 🔥 Tạo password reset token
     */
    public AuthToken createPasswordResetToken(User user) {
        AuthToken token = AuthToken.builder()
                .user(user)
                .type("password_reset")  // ✅ Match với DB constraint
                .token(UUID.randomUUID().toString().replace("-", ""))
                .expiresAt(OffsetDateTime.now().plusHours(1))
                .used(false)
                .build();

        log.info("🔑 Created password reset token for user: {}", user.getEmail());
        return authTokenRepository.save(token);
    }
}