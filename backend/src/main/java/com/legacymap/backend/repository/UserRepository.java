package com.legacymap.backend.repository;

import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface UserRepository extends JpaRepository<User, UUID> {
    boolean existsByUsername(String username);
    Optional<User> findByEmail(String email);
    Optional<User> findByUsername(String username);
    boolean existsByEmail(String email);

    // 🔥 THÊM MỚI - Admin queries
    List<User> findAllByOrderByCreatedAtDesc();
    List<User> findByIsBannedTrue();
    List<User> findByRoleName(String roleName);
}