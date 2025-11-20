package com.legacymap.backend.controller;

import java.util.List;
import java.util.UUID;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.legacymap.backend.dto.request.BranchRoomCreateRequest;
import com.legacymap.backend.dto.request.ChatRoomCreateRequest;
import com.legacymap.backend.dto.request.DirectRoomCreateRequest;
import com.legacymap.backend.dto.request.JoinRoomRequest;
import com.legacymap.backend.dto.response.ChatRoomResponse;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.ChatRoomService;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/api/chat/rooms")
@RequiredArgsConstructor
public class ChatRoomController {

    private final ChatRoomService chatRoomService;
    private final UserRepository userRepository;

    @PostMapping
    public ResponseEntity<ChatRoomResponse> createRoom(@Valid @RequestBody ChatRoomCreateRequest request) {
        return ResponseEntity.ok(chatRoomService.createRoom(getCurrentUserId(), request));
    }

    @PostMapping("/private")
    public ResponseEntity<ChatRoomResponse> createDirectRoom(@Valid @RequestBody DirectRoomCreateRequest request) {
        return ResponseEntity.ok(chatRoomService.createDirectRoom(getCurrentUserId(), request));
    }

    @PostMapping("/{roomId}/join")
    public ResponseEntity<ChatRoomResponse> joinRoom(@PathVariable UUID roomId,
                                                     @Valid @RequestBody JoinRoomRequest request) {
        return ResponseEntity.ok(chatRoomService.joinRoom(getCurrentUserId(), roomId, request));
    }

    @PostMapping("/branch")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ChatRoomResponse> createBranchRoom(@Valid @RequestBody BranchRoomCreateRequest request) {
        UUID userId = getCurrentUserId();
        return ResponseEntity.ok(chatRoomService.createBranchRoom(userId, request));
    }

    @GetMapping
    public ResponseEntity<List<ChatRoomResponse>> getMyRooms() {
        return ResponseEntity.ok(chatRoomService.findRoomsForUser(getCurrentUserId()));
    }

    private UUID getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new RuntimeException("User is not authenticated");
        }

        Object principal = authentication.getPrincipal();
        if (principal instanceof String str) {
            try {
                return UUID.fromString(str);
            } catch (IllegalArgumentException ignored) {
            }
        }

        String email = authentication.getName();
        return userRepository.findByEmail(email)
                .map(user -> user.getId())
                .orElseThrow(() -> new RuntimeException("User not found"));
    }
}

