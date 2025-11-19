package com.legacymap.backend.repository;

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

    boolean existsByPersonIdAndUserId(UUID personId, UUID userId);
    
    Optional<PersonUserLink> findByPersonIdAndUserId(UUID personId, UUID userId);
    
    void deleteByPersonIdAndUserId(UUID personId, UUID userId);
    
    @Query("SELECT pul.user.id FROM PersonUserLink pul WHERE pul.person.id IN :personIds AND pul.verified = true")
    Set<UUID> findUserIdsByPersonIds(@Param("personIds") Set<UUID> personIds);
}
