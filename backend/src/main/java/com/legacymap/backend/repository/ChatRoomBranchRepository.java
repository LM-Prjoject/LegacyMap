package com.legacymap.backend.repository;

import com.legacymap.backend.entity.ChatRoomBranch;
import com.legacymap.backend.entity.ChatRoomBranchId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ChatRoomBranchRepository extends JpaRepository<ChatRoomBranch, ChatRoomBranchId> {
}

