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

    List<FamilyTree> findAllByOrderByCreatedAtDesc();

    List<FamilyTree> findAllByCreatedBy(User user);

    Optional<FamilyTree> findByIdAndCreatedBy(UUID id, User user);

    long count();
}