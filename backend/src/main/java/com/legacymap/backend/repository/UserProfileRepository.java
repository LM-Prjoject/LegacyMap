package com.legacymap.backend.repository;

import com.legacymap.backend.entity.UserProfile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface UserProfileRepository extends JpaRepository<UserProfile, UUID> {

    // 🟢 (THÊM MỚI) tìm hồ sơ theo user_id
    Optional<UserProfile> findByUserId(UUID userId);
}