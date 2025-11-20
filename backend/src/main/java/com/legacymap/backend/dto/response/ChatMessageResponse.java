package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.ChatMessage;
import lombok.Builder;
import lombok.Value;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Value
@Builder
public class ChatMessageResponse {
    UUID id;
    UUID roomId;
    UUID senderId;
    String senderName;
    String messageText;
    ChatMessage.ChatMessageType messageType;
    String fileUrl;
    String fileName;
    Long fileSize;
    String fileType;
    UUID replyToId;
    Boolean edited;
    Boolean deleted;
    OffsetDateTime createdAt;
    OffsetDateTime updatedAt;
    List<MessageRecipientStatus> recipients;

    @Value
    @Builder
    public static class MessageRecipientStatus {
        UUID userId;
        Boolean read;
        OffsetDateTime readAt;
    }
}

