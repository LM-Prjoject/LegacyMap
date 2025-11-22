package com.legacymap.backend.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;

@Repository
public interface FamilyTreeRepository extends JpaRepository<FamilyTree, UUID> {

    // MAIN: Eager fetch User để tránh LazyInitializationException
    @Query("SELECT DISTINCT ft FROM FamilyTree ft LEFT JOIN FETCH ft.createdBy ORDER BY ft.createdAt DESC")
    List<FamilyTree> findAllWithUserOrderByCreatedAtDesc();

    // BACKUP: Simple query without JOIN
    @Query("SELECT ft FROM FamilyTree ft ORDER BY ft.createdAt DESC")
    List<FamilyTree> findAllOrderByCreatedAtDescSimple();

    // ALTERNATIVE: Native query
    @Query(value = "SELECT * FROM family_trees ORDER BY created_at DESC", nativeQuery = true)
    List<FamilyTree> findAllNative();

    // Giữ nguyên các method cũ
    List<FamilyTree> findAllByOrderByCreatedAtDesc();

    List<FamilyTree> findAllByCreatedBy(User user);

    Optional<FamilyTree> findByIdAndCreatedBy(UUID id, User user);

    long count();

    // ✅ Thêm method tìm tree bằng shareToken
    Optional<FamilyTree> findByShareToken(UUID shareToken);

    // ✅ THÊM MỚI: Query tất cả cây user có quyền truy cập (sở hữu + được chia sẻ)
    @Query("""
        SELECT DISTINCT ft FROM FamilyTree ft
        LEFT JOIN FETCH ft.createdBy
        LEFT JOIN TreeAccess ta ON ta.familyTreeId = ft.id
        WHERE ft.createdBy.id = :userId 
           OR ta.userId = :userId
        ORDER BY ft.createdAt DESC
    """)
    List<FamilyTree> findAllAccessibleByUser(@Param("userId") UUID userId);

    // ✅ THÊM MỚI: Query bằng user ID (thay vì User entity)
    @Query("SELECT ft FROM FamilyTree ft WHERE ft.createdBy.id = :userId")
    List<FamilyTree> findAllByCreatedBy_Id(@Param("userId") UUID userId);
}