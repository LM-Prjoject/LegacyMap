package com.legacymap.backend.controller;

import java.io.IOException;
import java.util.UUID;

import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.legacymap.backend.dto.request.MarkMessagesReadRequest;
import com.legacymap.backend.dto.request.UpdateMessageRequest;
import com.legacymap.backend.dto.response.AttachmentUploadResponse;
import com.legacymap.backend.dto.response.ChatMessagePageResponse;
import com.legacymap.backend.dto.response.ChatMessageResponse;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.ChatMessageService;
import com.legacymap.backend.service.SupabaseStorageService;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping("/api/chat")
@RequiredArgsConstructor
@Slf4j
public class ChatMessageController {

    private final ChatMessageService chatMessageService;
    private final SupabaseStorageService supabaseStorageService;
    private final UserRepository userRepository;

    @GetMapping("/rooms/{roomId}/messages")
    public ResponseEntity<ChatMessagePageResponse> getMessages(@PathVariable UUID roomId,
                                                               @RequestParam(defaultValue = "0") int page,
                                                               @RequestParam(defaultValue = "50") int size) {
        return ResponseEntity.ok(chatMessageService.getMessages(getCurrentUserId(), roomId, page, size));
    }

    @PostMapping("/rooms/{roomId}/read")
    public ResponseEntity<Void> markRead(@PathVariable UUID roomId,
                                         @Valid @RequestBody MarkMessagesReadRequest request) {
        chatMessageService.markMessagesRead(getCurrentUserId(), roomId, request);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/rooms/{roomId}/attachments", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<AttachmentUploadResponse> uploadAttachment(
            @PathVariable UUID roomId,
            @RequestPart("file") MultipartFile file,
            @RequestPart(value = "caption", required = false) String caption) {

        SupabaseStorageService.StoredFile storedFile;
        try {
            storedFile = supabaseStorageService.uploadChatFile(file, roomId);
        } catch (IOException e) {
            log.error("Chat file upload failed", e);
            throw new AppException(ErrorCode.INTERNAL_ERROR, "Cannot upload file");
        }

        ChatMessageResponse message = chatMessageService.createAttachmentMessage(
                getCurrentUserId(), roomId, storedFile, caption);

        return ResponseEntity.ok(AttachmentUploadResponse.builder()
                .fileUrl(storedFile.url())
                .originalName(storedFile.originalName())
                .size(storedFile.size())
                .contentType(storedFile.contentType())
                .message(message)
                .build());
    }

    @PatchMapping("/rooms/{roomId}/messages/{messageId}")
    public ResponseEntity<ChatMessageResponse> updateMessage(
            @PathVariable UUID roomId,
            @PathVariable UUID messageId,
            @Valid @RequestBody UpdateMessageRequest request) {

        ChatMessageResponse response = chatMessageService.updateMessage(
                getCurrentUserId(), roomId, messageId, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/rooms/{roomId}/messages/{messageId}")
    public ResponseEntity<Void> deleteMessage(
            @PathVariable UUID roomId,
            @PathVariable UUID messageId,
            @RequestParam(defaultValue = "false") boolean isAdmin) {

        chatMessageService.deleteMessage(getCurrentUserId(), roomId, messageId, isAdmin);
        return ResponseEntity.noContent().build();
    }

    private UUID getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new SecurityException("User not authenticated");
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
                .orElseThrow(() -> new SecurityException("User not found"));
    }
}
