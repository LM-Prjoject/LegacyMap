package com.legacymap.backend.repository;

import com.legacymap.backend.entity.AuditLog;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface AuditLogRepository extends JpaRepository<AuditLog, UUID> {

    // ✅ FIXED: Sử dụng JOIN FETCH thay vì EntityGraph
    @Query(value = "SELECT al FROM AuditLog al " +
            "LEFT JOIN FETCH al.user u " +
            "LEFT JOIN FETCH u.userProfile " +
            "WHERE al.tree.id = :treeId " +
            "ORDER BY al.createdAt DESC",
            countQuery = "SELECT COUNT(al) FROM AuditLog al WHERE al.tree.id = :treeId")
    Page<AuditLog> findByTreeIdWithUserOrderByCreatedAtDesc(@Param("treeId") UUID treeId, Pageable pageable);

    // ✅ FIXED: Method 2 với JOIN FETCH
    @Query(value = "SELECT al FROM AuditLog al " +
            "LEFT JOIN FETCH al.user u " +
            "LEFT JOIN FETCH u.userProfile " +
            "WHERE al.tree.id = :treeId " +
            "ORDER BY al.createdAt DESC",
            countQuery = "SELECT COUNT(al) FROM AuditLog al WHERE al.tree.id = :treeId")
    Page<AuditLog> findByTreeIdOrderByCreatedAtDesc(@Param("treeId") UUID treeId, Pageable pageable);

    // ✅ FIXED: Method 3 với JOIN FETCH
    @Query(value = "SELECT al FROM AuditLog al " +
            "LEFT JOIN FETCH al.user u " +
            "LEFT JOIN FETCH u.userProfile " +
            "ORDER BY al.createdAt DESC",
            countQuery = "SELECT COUNT(al) FROM AuditLog al")
    Page<AuditLog> findAllWithUserOrderByCreatedAtDesc(Pageable pageable);

    // ✅ FIXED: Method 4 với JOIN FETCH
    @Query(value = "SELECT al FROM AuditLog al " +
            "LEFT JOIN FETCH al.user u " +
            "LEFT JOIN FETCH u.userProfile " +
            "WHERE al.user.id = :userId " +
            "ORDER BY al.createdAt DESC",
            countQuery = "SELECT COUNT(al) FROM AuditLog al WHERE al.user.id = :userId")
    Page<AuditLog> findByUserIdWithUserOrderByCreatedAtDesc(@Param("userId") UUID userId, Pageable pageable);

    void deleteByTree_Id(UUID treeId);
}