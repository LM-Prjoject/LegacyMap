package com.legacymap.backend.repository;

import com.legacymap.backend.entity.UnbanRequest;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.enums.UnbanRequestStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface UnbanRequestRepository extends JpaRepository<UnbanRequest, UUID> {

    Optional<UnbanRequest> findFirstByUserAndStatusOrderByCreatedAtDesc(
            User user,
            UnbanRequestStatus status
    );
}
