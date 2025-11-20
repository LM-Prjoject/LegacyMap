package com.legacymap.backend.repository;

import com.legacymap.backend.entity.PersonUserLink;
import com.legacymap.backend.entity.PersonUserLinkId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface PersonUserLinkRepository extends JpaRepository<PersonUserLink, PersonUserLinkId> {
    Optional<PersonUserLink> findByPersonIdAndLinkType(UUID personId, PersonUserLink.LinkType linkType);

    Optional<PersonUserLink> findByPerson_IdAndUser_Id(UUID personId, UUID userId);

    boolean existsByPerson_IdAndLinkTypeAndVerifiedIsTrue(UUID personId, PersonUserLink.LinkType linkType);

    List<PersonUserLink> findByUser_IdAndVerifiedIsFalse(UUID userId);

    // Check if a user has a verified self link in a specific family tree
    boolean existsByUser_IdAndPerson_FamilyTree_IdAndLinkTypeAndVerifiedIsTrue(
            UUID userId,
            UUID familyTreeId,
            PersonUserLink.LinkType linkType
    );

    // List verified links by user and type
    List<PersonUserLink> findByUser_IdAndLinkTypeAndVerifiedIsTrue(UUID userId, PersonUserLink.LinkType linkType);
}
