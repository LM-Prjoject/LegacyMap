package com.legacymap.backend.repository;

import com.legacymap.backend.entity.ChatMessage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.UUID;

public interface ChatMessageRepository extends JpaRepository<ChatMessage, UUID> {

    Page<ChatMessage> findByRoom_IdOrderByCreatedAtDesc(UUID roomId, Pageable pageable);
    
    @Query("SELECT m FROM ChatMessage m " +
           "JOIN FETCH m.sender " +
           "JOIN FETCH m.room " +
           "LEFT JOIN FETCH m.replyTo " +
           "WHERE m.id = :messageId AND m.room.id = :roomId")
    Optional<ChatMessage> findByIdAndRoomIdWithRelations(@Param("messageId") UUID messageId, 
                                                       @Param("roomId") UUID roomId);
    
    @Modifying
    @Query("UPDATE ChatMessage m SET m.messageText = :messageText, m.updatedAt = :now, m.edited = true " +
           "WHERE m.id = :messageId AND m.sender.id = :senderId AND m.room.id = :roomId")
    int updateMessageText(@Param("messageId") UUID messageId, 
                         @Param("roomId") UUID roomId, 
                         @Param("senderId") UUID senderId,
                         @Param("messageText") String messageText,
                         @Param("now") OffsetDateTime now);

    @Modifying
    @Query("UPDATE ChatMessage m " + "SET m.deleted = true, " + "    m.deletedAt = :now, " + "    m.deletedBy.id = :userId " +
            "WHERE m.id = :messageId " + "  AND m.room.id = :roomId " + "  AND (m.sender.id = :userId OR :isAdmin = true)")
    int markAsDeleted(@Param("messageId") UUID messageId,
                      @Param("roomId") UUID roomId,
                      @Param("userId") UUID userId,
                      @Param("isAdmin") boolean isAdmin,
                      @Param("now") OffsetDateTime now);
}

