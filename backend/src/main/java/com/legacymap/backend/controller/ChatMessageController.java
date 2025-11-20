package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.MarkMessagesReadRequest;
import com.legacymap.backend.dto.response.AttachmentUploadResponse;
import com.legacymap.backend.dto.response.ChatMessagePageResponse;
import com.legacymap.backend.dto.response.ChatMessageResponse;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.ChatFileStorageService;
import com.legacymap.backend.service.ChatMessageService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.UUID;

@RestController
@RequestMapping("/api/chat")
@RequiredArgsConstructor
public class ChatMessageController {

    private final ChatMessageService chatMessageService;
    private final ChatFileStorageService chatFileStorageService;
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
    public ResponseEntity<AttachmentUploadResponse> uploadAttachment(@PathVariable UUID roomId,
                                                                     @RequestPart("file") MultipartFile file,
                                                                     @RequestPart(value = "caption", required = false) String caption) {
        ChatFileStorageService.StoredFile storedFile = chatFileStorageService.store(file);
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

