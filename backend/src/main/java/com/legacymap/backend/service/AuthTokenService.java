package com.legacymap.backend.service;

import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthTokenService {

    private final AuthTokenRepository authTokenRepository;
    private final UserRepository userRepository;

    /**
     * Tạo session token cho user (dùng cho login)
     */
    @Transactional
    public AuthToken createSessionToken(User user) {
        // 🔥 THÊM MỚI: Clean up expired tokens trước khi tạo token mới
        cleanupExpiredTokens(user.getId());

        String token = generateRandomToken();
        OffsetDateTime expiresAt = OffsetDateTime.now().plusDays(7);

        AuthToken authToken = AuthToken.builder()
                .user(user)
                .token(token)
                .type("session")
                .expiresAt(expiresAt)
                .used(false)
                .build();

        AuthToken savedToken = authTokenRepository.save(authToken);
        log.info("✅ Created session token for user: {}, role: {}, expires: {}",
                user.getId(), user.getRoleName(), expiresAt);
        return savedToken;
    }

    /**
     * Tạo email verification token
     */
    @Transactional
    public AuthToken createEmailVerificationToken(User user) {
        // 🔥 THÊM MỚI: Xóa các verification token cũ của user
        revokeTokensByType(user.getId(), "email_verification");

        AuthToken token = AuthToken.builder()
                .user(user)
                .type("email_verification")
                .token(UUID.randomUUID().toString().replace("-", ""))
                .expiresAt(OffsetDateTime.now().plusMinutes(5))
                .used(false)
                .build();

        AuthToken savedToken = authTokenRepository.save(token);
        log.info("📧 Created email verification token for user: {}, expires: {}",
                user.getEmail(), token.getExpiresAt());
        return savedToken;
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

            // Kiểm tra loại token - chấp nhận "session" (đây là access token)
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

            // 🔥 THÊM MỚI: Kiểm tra nếu user bị banned
            User user = authToken.getUser();
            if (Boolean.TRUE.equals(user.getIsBanned())) {
                log.warn("❌ User is banned: {}", user.getId());
                return null;
            }

            // Token hợp lệ, trả về userId
            UUID userId = user.getId();
            log.info("✅ Token valid for user: {}, role: {}", userId, user.getRoleName());
            return userId;

        } catch (Exception e) {
            log.error("💥 Error validating token: {}", e.getMessage(), e);
            return null;
        }
    }

    /**
     * 🔥 Validate token và trả về User object đầy đủ (dùng cho admin check)
     */
    @Transactional(readOnly = true)
    public User validateAccessTokenAndGetUser(String token) {
        try {
            log.debug("🔍 Validating token and getting user: {}...", token.substring(0, Math.min(20, token.length())));

            Optional<AuthToken> authTokenOpt = authTokenRepository.findByToken(token);

            if (authTokenOpt.isEmpty()) {
                log.warn("❌ Token not found in database");
                return null;
            }

            AuthToken authToken = authTokenOpt.get();

            if (!"session".equalsIgnoreCase(authToken.getType())) {
                log.warn("❌ Invalid token type: {} (expected session)", authToken.getType());
                return null;
            }

            if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
                log.warn("❌ Token expired at: {}", authToken.getExpiresAt());
                return null;
            }

            if (Boolean.TRUE.equals(authToken.getUsed())) {
                log.warn("❌ Token has been used/revoked");
                return null;
            }

            User user = authToken.getUser();

            // 🔥 THÊM MỚI: Kiểm tra nếu user bị banned
            if (Boolean.TRUE.equals(user.getIsBanned())) {
                log.warn("❌ User is banned: {}", user.getId());
                return null;
            }

            log.info("✅ Token valid for user: {}, role: {}", user.getId(), user.getRoleName());
            return user;

        } catch (Exception e) {
            log.error("💥 Error validating token and getting user: {}", e.getMessage(), e);
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
    @Transactional
    public AuthToken createPasswordResetToken(User user) {
        // 🔥 THÊM MỚI: Xóa các password reset token cũ của user
        revokeTokensByType(user.getId(), "password_reset");

        AuthToken token = AuthToken.builder()
                .user(user)
                .type("password_reset")
                .token(UUID.randomUUID().toString().replace("-", ""))
                .expiresAt(OffsetDateTime.now().plusHours(1))
                .used(false)
                .build();

        AuthToken savedToken = authTokenRepository.save(token);
        log.info("🔑 Created password reset token for user: {}, expires: {}",
                user.getEmail(), token.getExpiresAt());
        return savedToken;
    }

    /**
     * 🔥 Revoke all tokens của user (dùng cho ban user)
     */
    @Transactional
    public void revokeAllUserTokens(UUID userId) {
        try {
            int revokedCount = authTokenRepository.revokeAllUserTokens(userId);
            log.info("🔒 Revoked all {} tokens for user: {}", revokedCount, userId);
        } catch (Exception e) {
            log.error("❌ Failed to revoke tokens for user {}: {}", userId, e.getMessage());
            throw e; // Re-throw để Spring rollback transaction nếu cần
        }
    }

    /**
     * 🔥 THÊM MỚI: Revoke tokens theo type
     */
    @Transactional
    public void revokeTokensByType(UUID userId, String tokenType) {
        try {
            // Tìm tất cả active tokens của type cụ thể
            List<AuthToken> activeTokens = authTokenRepository.findActiveTokensByUserId(userId)
                    .stream()
                    .filter(token -> tokenType.equals(token.getType()))
                    .toList();

            // Revoke từng token
            for (AuthToken token : activeTokens) {
                token.setUsed(true);
                authTokenRepository.save(token);
            }

            log.info("🔒 Revoked {} {} tokens for user: {}", activeTokens.size(), tokenType, userId);
        } catch (Exception e) {
            log.error("❌ Failed to revoke {} tokens for user {}: {}", tokenType, userId, e.getMessage());
        }
    }

    /**
     * 🔥 THÊM MỚI: Clean up expired tokens
     */
    @Transactional
    public void cleanupExpiredTokens(UUID userId) {
        try {
            List<AuthToken> expiredTokens = authTokenRepository.findActiveTokensByUserId(userId)
                    .stream()
                    .filter(token -> token.getExpiresAt().isBefore(OffsetDateTime.now()))
                    .toList();

            for (AuthToken token : expiredTokens) {
                token.setUsed(true);
                authTokenRepository.save(token);
            }

            if (!expiredTokens.isEmpty()) {
                log.info("🧹 Cleaned up {} expired tokens for user: {}", expiredTokens.size(), userId);
            }
        } catch (Exception e) {
            log.error("❌ Failed to cleanup expired tokens for user {}: {}", userId, e.getMessage());
        }
    }

    /**
     * 🔥 THÊM MỚI: Lấy thông tin user từ token (không validate)
     */
    @Transactional(readOnly = true)
    public User getUserFromToken(String token) {
        return authTokenRepository.findByToken(token)
                .map(AuthToken::getUser)
                .orElse(null);
    }

    /**
     * 🔥 THÊM MỚI: Kiểm tra xem token có hợp lệ không (chỉ kiểm tra tồn tại)
     */
    @Transactional(readOnly = true)
    public boolean isTokenValid(String token) {
        return authTokenRepository.findByToken(token)
                .filter(authToken -> "session".equals(authToken.getType()))
                .filter(authToken -> !Boolean.TRUE.equals(authToken.getUsed()))
                .filter(authToken -> authToken.getExpiresAt().isAfter(OffsetDateTime.now()))
                .filter(authToken -> !Boolean.TRUE.equals(authToken.getUser().getIsBanned()))
                .isPresent();
    }

    /**
     * 🔥 THÊM MỚI: Lấy tất cả active tokens của user (dùng cho admin)
     */
    @Transactional(readOnly = true)
    public List<AuthToken> getActiveUserTokens(UUID userId) {
        return authTokenRepository.findActiveTokensByUserId(userId);
    }

    /**
     * 🔥 THÊM MỚI: Kiểm tra và xử lý khi user bị banned
     */
    @Transactional
    public void handleUserBan(UUID userId) {
        try {
            // Revoke all tokens
            revokeAllUserTokens(userId);
            log.info("🚫 All tokens revoked for banned user: {}", userId);
        } catch (Exception e) {
            log.error("❌ Failed to handle user ban for {}: {}", userId, e.getMessage());
            throw e;
        }
    }

    /**
     * 🔥 THÊM MỚI: Kiểm tra và xử lý khi user được unbanned
     */
    @Transactional
    public void handleUserUnban(UUID userId) {
        try {
            // Clean up expired tokens nhưng không tạo token mới
            // User cần login lại để tạo token mới
            cleanupExpiredTokens(userId);
            log.info("✅ Token cleanup completed for unbanned user: {}", userId);
        } catch (Exception e) {
            log.error("❌ Failed to handle user unban for {}: {}", userId, e.getMessage());
            throw e;
        }
    }
}