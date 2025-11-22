package com.legacymap.backend.repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.legacymap.backend.entity.PersonUserLink;
import com.legacymap.backend.entity.PersonUserLinkId;

public interface PersonUserLinkRepository extends JpaRepository<PersonUserLink, PersonUserLinkId> {
    Optional<PersonUserLink> findByPersonIdAndLinkType(UUID personId, PersonUserLink.LinkType linkType);

    Optional<PersonUserLink> findByPerson_IdAndUser_Id(UUID personId, UUID userId);

    boolean existsByPerson_IdAndLinkTypeAndStatus(UUID personId, PersonUserLink.LinkType linkType, PersonUserLink.Status status);

    List<PersonUserLink> findByUser_IdAndStatus(UUID userId, PersonUserLink.Status status);

    // Check if a user has a verified self link in a specific family tree
    boolean existsByUser_IdAndPerson_FamilyTree_IdAndLinkTypeAndStatus(
            UUID userId,
            UUID familyTreeId,
            PersonUserLink.LinkType linkType,
            PersonUserLink.Status status
    );

    // List verified links by user and type
    List<PersonUserLink> findByUser_IdAndLinkTypeAndStatus(UUID userId, PersonUserLink.LinkType linkType, PersonUserLink.Status status);

    boolean existsByPersonIdAndUserId(UUID personId, UUID userId);

    Optional<PersonUserLink> findByPersonIdAndUserId(UUID personId, UUID userId);

    void deleteByPersonIdAndUserId(UUID personId, UUID userId);

    @Query("SELECT pul.user.id FROM PersonUserLink pul WHERE pul.person.id IN :personIds AND pul.status = com.legacymap.backend.entity.PersonUserLink.Status.approved")
    Set<UUID> findUserIdsByPersonIds(@Param("personIds") Set<UUID> personIds);

    @Query("SELECT pul FROM PersonUserLink pul " +
            "JOIN FETCH pul.person p " +
            "JOIN FETCH pul.user u " +
            "WHERE p.familyTree.id = :familyTreeId " +
            "AND pul.status = com.legacymap.backend.entity.PersonUserLink.Status.approved")
    List<PersonUserLink> findApprovedLinksByFamilyTreeId(@Param("familyTreeId") UUID familyTreeId);
}
