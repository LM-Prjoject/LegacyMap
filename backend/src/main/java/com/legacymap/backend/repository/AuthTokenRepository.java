package com.legacymap.backend.repository;

import com.legacymap.backend.entity.AuthToken;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AuthTokenRepository extends JpaRepository<AuthToken, UUID> {
    Optional<AuthToken> findByTokenAndType(String token, String type);
    Optional<AuthToken> findByToken(String token);

    // 🔥 Revoke all tokens của user
    @Modifying
    @Query("UPDATE AuthToken a SET a.used = true WHERE a.user.id = :userId AND a.used = false")
    int revokeAllUserTokens(@Param("userId") UUID userId);

    // 🔥 Tìm active tokens của user
    @Query("SELECT a FROM AuthToken a WHERE a.user.id = :userId AND a.used = false AND a.expiresAt > CURRENT_TIMESTAMP")
    List<AuthToken> findActiveTokensByUserId(@Param("userId") UUID userId);

    // 🔥 THÊM MỚI: Tìm tokens theo type và user
    @Query("SELECT a FROM AuthToken a WHERE a.user.id = :userId AND a.type = :type AND a.used = false AND a.expiresAt > CURRENT_TIMESTAMP")
    List<AuthToken> findActiveTokensByUserIdAndType(@Param("userId") UUID userId, @Param("type") String type);

    // 🔥 THÊM MỚI: Đếm số active tokens của user
    @Query("SELECT COUNT(a) FROM AuthToken a WHERE a.user.id = :userId AND a.used = false AND a.expiresAt > CURRENT_TIMESTAMP")
    long countActiveTokensByUserId(@Param("userId") UUID userId);
}