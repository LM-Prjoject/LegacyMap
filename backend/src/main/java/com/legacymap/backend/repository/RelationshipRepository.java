package com.legacymap.backend.repository;

import com.legacymap.backend.entity.Relationship;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface RelationshipRepository extends JpaRepository<Relationship, UUID> {
    boolean existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(UUID treeId, UUID p1, UUID p2, String relationshipType);
    Optional<Relationship> findByIdAndFamilyTree_Id(UUID id, UUID treeId);
    Optional<Relationship> findByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(UUID treeId, UUID p1, UUID p2, String relationshipType);
}
