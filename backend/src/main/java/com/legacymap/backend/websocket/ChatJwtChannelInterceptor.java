package com.legacymap.backend.websocket;

import java.util.Collections;
import java.util.UUID;

import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.messaging.support.MessageHeaderAccessor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.JwtUtil;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Component
@RequiredArgsConstructor
@Slf4j
public class ChatJwtChannelInterceptor implements ChannelInterceptor {

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;

    @Override
    public Message<?> preSend(Message<?> message, MessageChannel channel) {
        StompHeaderAccessor accessor = MessageHeaderAccessor.getAccessor(message, StompHeaderAccessor.class);
        if (accessor != null && StompCommand.CONNECT.equals(accessor.getCommand())) {
            String token = resolveToken(accessor);
            UUID userId = jwtUtil.validateToken(token);
            if (userId == null || userRepository.findById(userId).isEmpty()) {
                throw new IllegalArgumentException("Token websocket không hợp lệ");
            }
            UsernamePasswordAuthenticationToken authentication =
                    new UsernamePasswordAuthenticationToken(userId.toString(), null, Collections.emptyList());
            accessor.setUser(authentication);
            SecurityContextHolder.getContext().setAuthentication(authentication);
        }
        return message;
    }

    private String resolveToken(StompHeaderAccessor accessor) {
        String header = accessor.getFirstNativeHeader("Authorization");
        if (header == null) {
            header = accessor.getFirstNativeHeader("authorization");
        }
        if (header == null) {
            header = accessor.getFirstNativeHeader("token");
        }
        if (header == null) {
            throw new IllegalArgumentException("Thiếu Authorization header cho WebSocket");
        }
        if (header.startsWith("Bearer ")) {
            return header.substring(7);
        }
        return header;
    }
}

