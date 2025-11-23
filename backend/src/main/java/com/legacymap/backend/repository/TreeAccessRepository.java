package com.legacymap.backend.repository;

import com.legacymap.backend.entity.TreeAccess;
import com.legacymap.backend.entity.TreeAccessId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface TreeAccessRepository extends JpaRepository<TreeAccess, TreeAccessId> {

    // Lấy tất cả người có quyền truy cập tree
    @Query("SELECT ta FROM TreeAccess ta " +
            "LEFT JOIN FETCH ta.user " +
            "LEFT JOIN FETCH ta.grantedBy " +
            "WHERE ta.familyTreeId = :familyTreeId")
    List<TreeAccess> findAllByFamilyTreeId(@Param("familyTreeId") UUID familyTreeId);

    // Method mới với JOIN FETCH để load user và grantedBy
    @Query("SELECT ta FROM TreeAccess ta " +
            "JOIN FETCH ta.user " +
            "LEFT JOIN FETCH ta.grantedBy " +
            "WHERE ta.familyTreeId = :treeId")
    List<TreeAccess> findAllByFamilyTreeIdWithUsers(@Param("treeId") UUID treeId);

    // Kiểm tra user có quyền truy cập tree không
    Optional<TreeAccess> findByUserIdAndFamilyTreeId(UUID userId, UUID familyTreeId);

    // Xóa tất cả quyền truy cập của tree
    void deleteAllByFamilyTreeId(UUID familyTreeId);

    // Xóa quyền truy cập của 1 user
    void deleteByUserIdAndFamilyTreeId(UUID userId, UUID familyTreeId);

    // Đếm số người có quyền truy cập
    long countByFamilyTreeId(UUID familyTreeId);

    // Lấy danh sách tree mà user có quyền truy cập
    @Query("SELECT ta FROM TreeAccess ta WHERE ta.userId = :userId")
    List<TreeAccess> findAllByUserId(UUID userId);
}