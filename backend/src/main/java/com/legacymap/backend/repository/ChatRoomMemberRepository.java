package com.legacymap.backend.repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.legacymap.backend.entity.ChatRoomMember;
import com.legacymap.backend.entity.ChatRoomMemberId;

public interface ChatRoomMemberRepository extends JpaRepository<ChatRoomMember, ChatRoomMemberId> {

    List<ChatRoomMember> findByRoom_Id(UUID roomId);

    List<ChatRoomMember> findByUser_Id(UUID userId);

    Optional<ChatRoomMember> findByRoom_IdAndUser_Id(UUID roomId, UUID userId);

    Optional<ChatRoomMember> findByRoom_IdAndUser_IdNot(@Param("roomId") UUID roomId, @Param("userId") UUID userId);

    @Query("""
        SELECT CASE WHEN EXISTS (
            SELECT 1 FROM ChatRoomMember m
            WHERE m.room.id = :roomId
              AND m.user.id = :userId
              AND m.role = 'admin'
        ) THEN true ELSE false END
    """)
    boolean isAdmin(@Param("roomId") UUID roomId, @Param("userId") UUID userId);
    
    boolean existsByRoomIdAndUserId(UUID roomId, UUID userId);
}

