package com.legacymap.backend.service;

import com.legacymap.backend.entity.UserSession;
import com.legacymap.backend.repository.UserSessionRepository;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserSessionService {

    private final UserSessionRepository sessionRepository;

    // ✅ Thời gian timeout (5 phút không activity = offline)
    private static final int ACTIVITY_TIMEOUT_MINUTES = 5;

    /**
     * Tạo session mới khi user đăng nhập
     */
    @Transactional
    public UserSession createSession(UUID userId, String token, HttpServletRequest request) {
        log.info("Creating new session for user: {}", userId);

        UserSession session = UserSession.builder()
                .userId(userId)
                .sessionToken(token)
                .lastActivity(OffsetDateTime.now())
                .isActive(true)
                .userAgent(request.getHeader("User-Agent"))
                .ipAddress(getClientIp(request))
                .expiresAt(OffsetDateTime.now().plusDays(30)) // Session expires in 30 days
                .build();

        return sessionRepository.save(session);
    }

    /**
     * Cập nhật activity của session (gọi mỗi khi user tương tác)
     */
    @Transactional
    public void updateActivity(String sessionToken) {
        try {
            int updated = sessionRepository.updateLastActivity(sessionToken, OffsetDateTime.now());
            if (updated == 0) {
                log.warn("No session found to update: {}", sessionToken);
            }
        } catch (Exception e) {
            log.error("Error updating session activity: {}", e.getMessage());
        }
    }

    /**
     * Deactivate session khi user logout
     */
    @Transactional
    public void deactivateSession(String sessionToken) {
        log.info("Deactivating session: {}", sessionToken);
        sessionRepository.findBySessionToken(sessionToken)
                .ifPresent(session -> {
                    session.setIsActive(false);
                    sessionRepository.save(session);
                });
    }

    /**
     * Deactivate tất cả sessions của user
     */
    @Transactional
    public void deactivateAllUserSessions(UUID userId) {
        log.info("Deactivating all sessions for user: {}", userId);
        sessionRepository.deactivateUserSessions(userId);
    }

    /**
     * Đếm số user đang online
     */
    public long countOnlineUsers() {
        OffsetDateTime cutoff = OffsetDateTime.now().minusMinutes(ACTIVITY_TIMEOUT_MINUTES);
        return sessionRepository.countOnlineUsers(cutoff);
    }

    /**
     * Lấy danh sách user IDs đang online
     */
    public List<UUID> getOnlineUserIds() {
        OffsetDateTime cutoff = OffsetDateTime.now().minusMinutes(ACTIVITY_TIMEOUT_MINUTES);
        return sessionRepository.findOnlineUserIds(cutoff);
    }

    /**
     * Kiểm tra user có đang online không
     */
    public boolean isUserOnline(UUID userId) {
        OffsetDateTime cutoff = OffsetDateTime.now().minusMinutes(ACTIVITY_TIMEOUT_MINUTES);
        List<UserSession> activeSessions = sessionRepository.findByUserIdAndIsActiveTrue(userId);

        return activeSessions.stream()
                .anyMatch(session -> session.getLastActivity().isAfter(cutoff));
    }

    /**
     * ✅ Scheduled task: Tự động cleanup sessions cũ (chạy mỗi 15 phút)
     */
    @Scheduled(fixedDelay = 900000) // 15 minutes
    @Transactional
    public void cleanupInactiveSessions() {
        try {
            log.info("🧹 Starting session cleanup...");

            // Mark sessions inactive after 5 minutes
            OffsetDateTime inactivityCutoff = OffsetDateTime.now().minusMinutes(ACTIVITY_TIMEOUT_MINUTES);
            int markedInactive = sessionRepository.markInactiveSessions(inactivityCutoff);

            // Delete sessions older than 30 days
            OffsetDateTime deletionCutoff = OffsetDateTime.now().minusDays(30);
            int deleted = sessionRepository.deleteOldSessions(deletionCutoff);

            log.info("✅ Session cleanup complete: {} marked inactive, {} deleted",
                    markedInactive, deleted);
        } catch (Exception e) {
            log.error("❌ Error during session cleanup: {}", e.getMessage(), e);
        }
    }

    /**
     * Lấy client IP address từ request
     */
    private String getClientIp(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("X-Real-IP");
        }
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }
}