package com.legacymap.backend.family.repository;

import com.legacymap.backend.family.entity.AuthToken;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface AuthTokenRepository extends JpaRepository<AuthToken, UUID> {
    Optional<AuthToken> findByTokenAndType(String token, String type);
}
