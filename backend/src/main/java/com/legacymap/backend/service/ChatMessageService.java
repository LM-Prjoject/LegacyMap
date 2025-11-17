package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.ChatMessageSendRequest;
import com.legacymap.backend.dto.request.MarkMessagesReadRequest;
import com.legacymap.backend.dto.response.ChatMessagePageResponse;
import com.legacymap.backend.dto.response.ChatMessageResponse;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.ChatMessage;
import com.legacymap.backend.entity.ChatRoom;
import com.legacymap.backend.entity.ChatRoomMember;
import com.legacymap.backend.entity.MessageStatus;
import com.legacymap.backend.entity.MessageStatusId;
import com.legacymap.backend.service.ChatFileStorageService.StoredFile;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.ChatMessageRepository;
import com.legacymap.backend.repository.ChatRoomMemberRepository;
import com.legacymap.backend.repository.MessageStatusRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ChatMessageService {

    private final ChatRoomService chatRoomService;
    private final ChatMessageRepository chatMessageRepository;
    private final MessageStatusRepository messageStatusRepository;
    private final ChatRoomMemberRepository chatRoomMemberRepository;
    private final SimpMessagingTemplate messagingTemplate;

    @Transactional
    public ChatMessageResponse sendMessage(UUID senderId, ChatMessageSendRequest request) {
        ChatRoomMember membership = chatRoomService.ensureMembership(request.getRoomId(), senderId);
        ChatRoom room = membership.getRoom();
        ChatMessage message = buildMessage(room, membership.getUser(), request);
        ChatMessage saved = chatMessageRepository.save(message);

        List<MessageStatus> statuses = createStatuses(saved, membership.getUser());
        ChatMessageResponse response = toResponse(saved, statuses);
        broadcast(room.getId(), response);
        return response;
    }

    @Transactional
    public ChatMessageResponse createAttachmentMessage(UUID senderId, UUID roomId, StoredFile storedFile, String caption) {
        ChatMessageSendRequest request = new ChatMessageSendRequest();
        request.setRoomId(roomId);
        request.setMessageText(caption);
        request.setMessageType(storedFile.image()
                ? ChatMessage.ChatMessageType.image
                : ChatMessage.ChatMessageType.file);
        request.setFileUrl(storedFile.url());
        request.setFileName(storedFile.originalName());
        request.setFileSize(storedFile.size());
        request.setFileType(storedFile.contentType());
        return sendMessage(senderId, request);
    }

    @Transactional(readOnly = true)
    public ChatMessagePageResponse getMessages(UUID userId, UUID roomId, int page, int size) {
        chatRoomService.ensureMembership(roomId, userId);
        Page<ChatMessage> messagePage = chatMessageRepository
                .findByRoom_IdOrderByCreatedAtDesc(roomId, PageRequest.of(page, size));

        List<ChatMessageResponse> responses = messagePage.stream()
                .map(message -> {
                    List<MessageStatus> statuses = messageStatusRepository.findByMessage_Id(message.getId());
                    return toResponse(message, statuses);
                })
                .toList();

        return ChatMessagePageResponse.builder()
                .messages(responses)
                .hasMore(messagePage.hasNext())
                .build();
    }

    @Transactional
    public void markMessagesRead(UUID userId, UUID roomId, MarkMessagesReadRequest request) {
        ChatRoomMember member = chatRoomService.ensureMembership(roomId, userId);
        ChatMessage lastMessage = chatMessageRepository.findById(request.getLastMessageId())
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));

        if (!lastMessage.getRoom().getId().equals(roomId)) {
            throw new AppException(ErrorCode.BAD_REQUEST);
        }

        List<MessageStatus> statuses = messageStatusRepository.findByUser_IdAndMessage_Room_Id(userId, roomId);
        for (MessageStatus status : statuses) {
            if (!Boolean.TRUE.equals(status.getRead())
                    && !status.getMessage().getCreatedAt().isAfter(lastMessage.getCreatedAt())) {
                status.markAsRead();
            }
        }
        messageStatusRepository.saveAll(statuses);

        member.setLastReadAt(OffsetDateTime.now());
        chatRoomMemberRepository.save(member);
    }

    private ChatMessage buildMessage(ChatRoom room, User sender, ChatMessageSendRequest request) {
        ChatMessage.ChatMessageType messageType = request.getMessageType() != null
                ? request.getMessageType()
                : ChatMessage.ChatMessageType.text;
        ChatMessage.ChatMessageBuilder builder = ChatMessage.builder()
                .room(room)
                .sender(sender)
                .messageText(request.getMessageText())
                .messageType(messageType)
                .fileUrl(request.getFileUrl())
                .fileName(request.getFileName())
                .fileSize(request.getFileSize())
                .fileType(request.getFileType());

        if (request.getReplyToId() != null) {
            ChatMessage replied = chatMessageRepository.findById(request.getReplyToId())
                    .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));
            builder.replyTo(replied);
        }
        return builder.build();
    }

    private List<MessageStatus> createStatuses(ChatMessage message, User sender) {
        List<ChatRoomMember> members = chatRoomMemberRepository.findByRoom_Id(message.getRoom().getId());
        OffsetDateTime now = OffsetDateTime.now();
        List<MessageStatus> statuses = members.stream()
                .map(member -> {
                    MessageStatus status = MessageStatus.builder()
                            .id(new MessageStatusId(
                                    message.getId(), member.getUser().getId()))
                            .message(message)
                            .user(member.getUser())
                            .build();
                    if (member.getUser().getId().equals(sender.getId())) {
                        status.markAsRead();
                        member.setLastReadAt(now);
                        chatRoomMemberRepository.save(member);
                    }
                    return status;
                })
                .collect(Collectors.toList());
        return messageStatusRepository.saveAll(statuses);
    }

    private ChatMessageResponse toResponse(ChatMessage message, List<MessageStatus> statuses) {
        List<ChatMessageResponse.MessageRecipientStatus> recipients = statuses.stream()
                .map(status -> ChatMessageResponse.MessageRecipientStatus.builder()
                        .userId(status.getUser().getId())
                        .read(status.getRead())
                        .readAt(status.getReadAt())
                        .build())
                .toList();

        return ChatMessageResponse.builder()
                .id(message.getId())
                .roomId(message.getRoom().getId())
                .senderId(message.getSender().getId())
                .senderName(message.getSender().getUsername())
                .messageText(message.getMessageText())
                .messageType(message.getMessageType())
                .fileUrl(message.getFileUrl())
                .fileName(message.getFileName())
                .fileSize(message.getFileSize())
                .fileType(message.getFileType())
                .replyToId(message.getReplyTo() != null ? message.getReplyTo().getId() : null)
                .edited(message.getEdited())
                .deleted(message.getDeleted())
                .createdAt(message.getCreatedAt())
                .updatedAt(message.getUpdatedAt())
                .recipients(recipients)
                .build();
    }

    private void broadcast(UUID roomId, ChatMessageResponse response) {
        messagingTemplate.convertAndSend("/topic/chat/" + roomId, response);
    }
}

