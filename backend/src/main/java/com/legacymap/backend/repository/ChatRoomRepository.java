package com.legacymap.backend.repository;

import com.legacymap.backend.entity.ChatRoom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ChatRoomRepository extends JpaRepository<ChatRoom, UUID> {

    List<ChatRoom> findByCreatedBy_Id(UUID userId);
    
    @Query("SELECT r FROM ChatRoom r " +
           "JOIN ChatRoomMember m ON m.room.id = r.id " +
           "WHERE m.user.id = :userId AND r.active = true")
    List<ChatRoom> findActiveRoomsByMemberId(@Param("userId") UUID userId);
    
    @Query("SELECT r FROM ChatRoom r " +
           "WHERE r.id = :roomId AND r.active = true")
    Optional<ChatRoom> findActiveById(@Param("roomId") UUID roomId);
    
    @Modifying
    @Query("UPDATE ChatRoom r SET r.active = false, r.updatedAt = CURRENT_TIMESTAMP " +
           "WHERE r.id = :roomId AND r.createdBy.id = :userId")
    int deactivateRoom(@Param("roomId") UUID roomId, @Param("userId") UUID userId);

    @Modifying
    @Query("""
        UPDATE ChatRoom r 
        SET r.name = :name, 
            r.description = :description, 
            r.updatedAt = CURRENT_TIMESTAMP
        WHERE r.id = :roomId
    """)
    int updateRoomInfo(
            @Param("roomId") UUID roomId,
            @Param("name") String name,
            @Param("description") String description
    );
    
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

