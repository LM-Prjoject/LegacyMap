package com.legacymap.backend.repository;

import com.legacymap.backend.entity.PersonUserLink;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface PersonUserLinkRepository extends JpaRepository<PersonUserLink, UUID> {
    Optional<PersonUserLink> findByPersonIdAndLinkType(UUID personId, PersonUserLink.LinkType linkType);
}
