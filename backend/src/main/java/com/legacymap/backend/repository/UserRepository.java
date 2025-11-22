package com.legacymap.backend.repository;

import com.legacymap.backend.entity.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
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

    // ✅ NEW: Tìm tất cả users có cùng email (cho ban by email)
    List<User> findAllByEmail(String email);

    @Query("""
            SELECT DISTINCT u FROM User u
            LEFT JOIN UserProfile p ON p.user = u
            WHERE LOWER(u.email) LIKE LOWER(CONCAT('%', :keyword, '%'))
               OR LOWER(u.username) LIKE LOWER(CONCAT('%', :keyword, '%'))
               OR (p.fullName IS NOT NULL AND LOWER(p.fullName) LIKE LOWER(CONCAT('%', :keyword, '%')))
               OR (p.phone IS NOT NULL AND p.phone LIKE CONCAT('%', :keyword, '%'))
            """)
    Page<User> searchUsers(@Param("keyword") String keyword, Pageable pageable);
}