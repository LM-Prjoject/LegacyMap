package com.legacymap.backend.repository;

import com.legacymap.backend.entity.UserSession;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface UserSessionRepository extends JpaRepository<UserSession, UUID> {

    /**
     * Tìm session theo token
     */
    Optional<UserSession> findBySessionToken(String sessionToken);

    /**
     * Tìm tất cả sessions active của user
     */
    List<UserSession> findByUserIdAndIsActiveTrue(UUID userId);

    /**
     * Đếm số user đang online (có session active trong X phút gần đây)
     */
    @Query("SELECT COUNT(DISTINCT s.userId) FROM UserSession s " +
            "WHERE s.isActive = true " +
            "AND s.lastActivity > :cutoffTime")
    long countOnlineUsers(@Param("cutoffTime") OffsetDateTime cutoffTime);

    /**
     * Lấy danh sách user IDs đang online
     */
    @Query("SELECT DISTINCT s.userId FROM UserSession s " +
            "WHERE s.isActive = true " +
            "AND s.lastActivity > :cutoffTime")
    List<UUID> findOnlineUserIds(@Param("cutoffTime") OffsetDateTime cutoffTime);

    /**
     * Đánh dấu sessions cũ là inactive
     */
    @Modifying
    @Query("UPDATE UserSession s SET s.isActive = false " +
            "WHERE s.isActive = true " +
            "AND s.lastActivity < :cutoffTime")
    int markInactiveSessions(@Param("cutoffTime") OffsetDateTime cutoffTime);

    /**
     * Xóa sessions cũ
     */
    @Modifying
    @Query("DELETE FROM UserSession s WHERE s.lastActivity < :cutoffTime")
    int deleteOldSessions(@Param("cutoffTime") OffsetDateTime cutoffTime);

    /**
     * Cập nhật last_activity của session
     */
    @Modifying
    @Query("UPDATE UserSession s SET s.lastActivity = :now " +
            "WHERE s.sessionToken = :token AND s.isActive = true")
    int updateLastActivity(@Param("token") String token, @Param("now") OffsetDateTime now);

    /**
     * Deactivate tất cả sessions của user (khi logout)
     */
    @Modifying
    @Query("UPDATE UserSession s SET s.isActive = false " +
            "WHERE s.userId = :userId AND s.isActive = true")
    int deactivateUserSessions(@Param("userId") UUID userId);
}