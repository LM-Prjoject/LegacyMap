package com.legacymap.backend.controller;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.legacymap.backend.dto.request.*;
import com.legacymap.backend.entity.ChatRoom;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

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

    @PostMapping("/branch")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ChatRoomResponse> createBranchRoom(@Valid @RequestBody BranchRoomCreateRequest request) {
        UUID userId = getCurrentUserId();
        return ResponseEntity.ok(chatRoomService.createBranchRoom(userId, request));
    }

    @PostMapping("/{roomId}/members/{userId}/role")
    public ResponseEntity<ChatRoomResponse> updateMemberRole(
            @PathVariable UUID roomId,
            @PathVariable UUID userId,
            @RequestBody @Valid UpdateMemberRoleRequest request) {

        UUID actorId = getCurrentUserId();

        ChatRoomResponse response = chatRoomService.updateMemberRole(
                actorId, roomId, userId, request.getRole());
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{roomId}/members/me")
    public ResponseEntity<Void> leaveRoom(@PathVariable UUID roomId) {
        UUID userId = getCurrentUserId();
        ChatRoom room = chatRoomService.getRoomOrThrow(roomId);

        if (room.getRoomType() == ChatRoom.ChatRoomType.family ||
                room.getRoomType() == ChatRoom.ChatRoomType.private_chat) {
            throw new AppException(ErrorCode.BAD_REQUEST, "You cannot leave this room");
        }

        chatRoomService.leaveRoom(userId, roomId);
        return ResponseEntity.noContent().build();
    }

    @PutMapping("/{roomId}/members/me")
    public ResponseEntity<ChatRoomResponse> updateMyMembership(
            @PathVariable UUID roomId,
            @RequestBody Map<String, Object> payload) {

        return ResponseEntity.ok(
                chatRoomService.updateMyMembership(roomId, getCurrentUserId(), payload)
        );
    }

    @PutMapping("/{roomId}")
    public ResponseEntity<ChatRoomResponse> updateRoom(
            @PathVariable UUID roomId,
            @Valid @RequestBody UpdateRoomRequest request) {

        ChatRoomResponse updated = chatRoomService.updateRoom(getCurrentUserId(), roomId, request);
        return ResponseEntity.ok(updated);
    }

    @DeleteMapping("/{roomId}")
    public ResponseEntity<Void> deleteRoom(@PathVariable UUID roomId) {
        chatRoomService.deactivateRoom(getCurrentUserId(), roomId);
        return ResponseEntity.noContent().build();
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

