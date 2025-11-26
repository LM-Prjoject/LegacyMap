package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.ChatMessage;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.UUID;

@Data
public class ChatMessageSendRequest {
    @NotNull
    private UUID roomId;
    private String messageText;
    private ChatMessage.ChatMessageType messageType = ChatMessage.ChatMessageType.text;
    private UUID replyToId;
    private String fileUrl;
    private String fileName;
    private Long fileSize;
    private String fileType;
}

