package com.legacymap.backend.repository;

import com.legacymap.backend.entity.ChatRoomMember;
import com.legacymap.backend.entity.ChatRoomMemberId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ChatRoomMemberRepository extends JpaRepository<ChatRoomMember, ChatRoomMemberId> {

    List<ChatRoomMember> findByRoom_Id(UUID roomId);

    List<ChatRoomMember> findByUser_Id(UUID userId);

    Optional<ChatRoomMember> findByRoom_IdAndUser_Id(UUID roomId, UUID userId);

    boolean existsByRoom_IdAndUser_Id(UUID roomId, UUID userId);
}

