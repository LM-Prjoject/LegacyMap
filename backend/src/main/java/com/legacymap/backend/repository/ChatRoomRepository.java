package com.legacymap.backend.repository;

import com.legacymap.backend.entity.ChatRoom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ChatRoomRepository extends JpaRepository<ChatRoom, UUID> {

    List<ChatRoom> findByCreatedBy_Id(UUID userId);

    @Query("""
            select r
            from ChatRoom r
            where r.roomType = :roomType
              and r.active = true
              and exists (
                  select 1 from ChatRoomMember m
                  where m.room = r and m.user.id = :userA
              )
              and exists (
                  select 1 from ChatRoomMember m
                  where m.room = r and m.user.id = :userB
              )
              and (select count(m) from ChatRoomMember m where m.room = r) = :memberCount
            """)
    Optional<ChatRoom> findPrivateRoomWithMembers(@Param("roomType") ChatRoom.ChatRoomType roomType,
                                                  @Param("userA") UUID userA,
                                                  @Param("userB") UUID userB,
                                                  @Param("memberCount") long memberCount);
    
    Optional<ChatRoom> findByFamilyTreeIdAndRoomType(UUID familyTreeId, ChatRoom.ChatRoomType roomType);
}

