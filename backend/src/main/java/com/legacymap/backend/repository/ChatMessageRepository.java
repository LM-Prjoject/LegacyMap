package com.legacymap.backend.repository;

import com.legacymap.backend.entity.ChatMessage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ChatMessageRepository extends JpaRepository<ChatMessage, UUID> {

    Page<ChatMessage> findByRoom_IdOrderByCreatedAtDesc(UUID roomId, Pageable pageable);
}

