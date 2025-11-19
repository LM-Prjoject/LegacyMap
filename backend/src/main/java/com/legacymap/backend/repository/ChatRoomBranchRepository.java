package com.legacymap.backend.repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;

import com.legacymap.backend.entity.ChatRoomBranch;
import com.legacymap.backend.entity.ChatRoomBranchId;

public interface ChatRoomBranchRepository extends JpaRepository<ChatRoomBranch, ChatRoomBranchId> {
    Optional<ChatRoomBranch> findByBranchPersonId(UUID branchPersonId);
    
    List<ChatRoomBranch> findByBranchPersonIdIn(Set<UUID> branchPersonIds);
}

