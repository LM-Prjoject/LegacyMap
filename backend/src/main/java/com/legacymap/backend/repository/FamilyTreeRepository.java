package com.legacymap.backend.repository;

import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface FamilyTreeRepository extends JpaRepository<FamilyTree, UUID> {

    // ✅ MAIN: Eager fetch User để tránh LazyInitializationException
    @Query("SELECT DISTINCT ft FROM FamilyTree ft LEFT JOIN FETCH ft.createdBy ORDER BY ft.createdAt DESC")
    List<FamilyTree> findAllWithUserOrderByCreatedAtDesc();

    // ✅ BACKUP: Simple query without JOIN
    @Query("SELECT ft FROM FamilyTree ft ORDER BY ft.createdAt DESC")
    List<FamilyTree> findAllOrderByCreatedAtDescSimple();

    // ✅ ALTERNATIVE: Native query
    @Query(value = "SELECT * FROM family_trees ORDER BY created_at DESC", nativeQuery = true)
    List<FamilyTree> findAllNative();

    // Giữ nguyên các method cũ
    List<FamilyTree> findAllByOrderByCreatedAtDesc();

    List<FamilyTree> findAllByCreatedBy(User user);

    Optional<FamilyTree> findByIdAndCreatedBy(UUID id, User user);

    long count();
}