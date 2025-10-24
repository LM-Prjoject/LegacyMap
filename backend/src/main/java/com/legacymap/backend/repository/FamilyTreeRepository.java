package com.legacymap.backend.repository;

import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface FamilyTreeRepository extends JpaRepository<FamilyTree, UUID> {

    // ✅ Tìm family trees theo thứ tự mới nhất (cho Admin)
    List<FamilyTree> findAllByOrderByCreatedAtDesc();

    // ✅ Tìm family trees theo User object (cho FamilyTreeService)
    List<FamilyTree> findAllByCreatedBy(User user);

    // ✅ Tìm 1 family tree theo ID và User owner (cho FamilyTreeService & RelationshipService)
    Optional<FamilyTree> findByIdAndCreatedBy(UUID id, User user);

    // ✅ Đếm số lượng family trees
    long count();
}