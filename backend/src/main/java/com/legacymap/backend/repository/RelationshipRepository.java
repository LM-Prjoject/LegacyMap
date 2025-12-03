package com.legacymap.backend.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;

import com.legacymap.backend.entity.Relationship;

public interface RelationshipRepository extends JpaRepository<Relationship, UUID> {
    boolean existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(UUID treeId, UUID p1, UUID p2, String relationshipType);
    Optional<Relationship> findByIdAndFamilyTree_Id(UUID id, UUID treeId);
    Optional<Relationship> findByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(UUID treeId, UUID p1, UUID p2, String relationshipType);
    List<Relationship> findAllByFamilyTree_Id(UUID treeId);
    List<Relationship> findByFamilyTreeId(UUID treeId);
    List<Relationship> findByPerson1IdOrPerson2Id(UUID person1Id, UUID person2Id);

    @org.springframework.data.jpa.repository.Query("SELECT r.person2.id FROM Relationship r WHERE r.person1.id = :personId AND r.relationshipType = 'parent'")
    List<UUID> findChildIdsByPersonId(@org.springframework.data.repository.query.Param("personId") UUID personId);

    @org.springframework.data.jpa.repository.Query("SELECT r.person1.id FROM Relationship r WHERE r.person2.id = :personId AND r.relationshipType = 'parent'")
    List<UUID> findParentIdsByPersonId(@org.springframework.data.repository.query.Param("personId") UUID personId);
}
