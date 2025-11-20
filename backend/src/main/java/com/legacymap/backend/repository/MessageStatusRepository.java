package com.legacymap.backend.repository;

import com.legacymap.backend.entity.MessageStatus;
import com.legacymap.backend.entity.MessageStatusId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface MessageStatusRepository extends JpaRepository<MessageStatus, MessageStatusId> {

    List<MessageStatus> findByMessage_Id(UUID messageId);

    List<MessageStatus> findByUser_IdAndMessage_Room_Id(UUID userId, UUID roomId);
}

