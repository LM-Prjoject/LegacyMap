package com.legacymap.backend.repository;

import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface FamilyTreeRepository extends JpaRepository<FamilyTree, UUID> {
    Optional<FamilyTree> findByIdAndCreatedBy(UUID id, User createdBy);
    List<FamilyTree> findAllByCreatedBy(User createdBy);
}
