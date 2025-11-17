package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.ChatMessageSendRequest;
import com.legacymap.backend.service.ChatMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Controller;

import java.security.Principal;
import java.util.UUID;

@Controller
@RequiredArgsConstructor
public class ChatWebSocketController {

    private final ChatMessageService chatMessageService;

    @MessageMapping("/chat/send")
    public void sendMessage(@Payload ChatMessageSendRequest payload, Principal principal) {
        UUID senderId = extractUserId(principal);
        chatMessageService.sendMessage(senderId, payload);
    }

    private UUID extractUserId(Principal principal) {
        if (principal == null || principal.getName() == null) {
            throw new IllegalStateException("Không xác định được người dùng WebSocket");
        }
        return UUID.fromString(principal.getName());
    }
}

