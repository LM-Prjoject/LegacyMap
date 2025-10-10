package com.legacymap.backend.repository;

import com.legacymap.backend.entity.AuthToken;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface AuthTokenRepository extends JpaRepository<AuthToken, UUID> {
    Optional<AuthToken> findByTokenAndType(String token, String type);
    Optional<AuthToken> findByToken(String token);

}
