package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.ChatRoomCreateRequest;
import com.legacymap.backend.dto.request.DirectRoomCreateRequest;
import com.legacymap.backend.dto.request.JoinRoomRequest;
import com.legacymap.backend.dto.response.ChatRoomMemberResponse;
import com.legacymap.backend.dto.response.ChatRoomResponse;
import com.legacymap.backend.entity.*;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ChatRoomService {

    private final ChatRoomRepository chatRoomRepository;
    private final ChatRoomMemberRepository chatRoomMemberRepository;
    private final ChatRoomBranchRepository chatRoomBranchRepository;
    private final UserRepository userRepository;
    private final FamilyTreeRepository familyTreeRepository;
    private final PersonRepository personRepository;

    @Transactional
    public ChatRoomResponse createRoom(UUID creatorId, ChatRoomCreateRequest request) {
        User creator = getUserOrThrow(creatorId);
        validateCreateRequest(request);

        ChatRoom room = ChatRoom.builder()
                .roomType(request.getRoomType())
                .name(request.getName())
                .description(request.getDescription())
                .createdBy(creator)
                .familyTree(resolveFamilyTree(request))
                .build();

        ChatRoom savedRoom = chatRoomRepository.save(room);
        addMember(savedRoom, creator, null, ChatRoomMember.ChatMemberRole.admin);

        if (request.getMemberUserIds() != null) {
            for (UUID memberId : request.getMemberUserIds()) {
                if (memberId.equals(creatorId)) {
                    continue;
                }
                User member = getUserOrThrow(memberId);
                addMember(savedRoom, member, null, ChatRoomMember.ChatMemberRole.member);
            }
        }

        if (request.getRoomType() == ChatRoom.ChatRoomType.branch) {
            List<UUID> branchIds = Optional.ofNullable(request.getBranchPersonIds())
                    .filter(list -> !list.isEmpty())
                    .orElseGet(() -> request.getBranchPersonId() != null
                            ? List.of(request.getBranchPersonId())
                            : List.of());
            for (UUID branchPersonId : branchIds) {
                Person person = getPersonOrThrow(branchPersonId);
                ChatRoomBranch branch = ChatRoomBranch.builder()
                        .id(new ChatRoomBranchId(savedRoom.getId(), person.getId()))
                        .room(savedRoom)
                        .branchPerson(person)
                        .createdBy(creator)
                        .build();
                chatRoomBranchRepository.save(branch);
            }
        }

        return toResponse(savedRoom);
    }

    @Transactional
    public ChatRoomResponse createDirectRoom(UUID creatorId, DirectRoomCreateRequest request) {
        if (creatorId.equals(request.getTargetUserId())) {
            throw new AppException(ErrorCode.BAD_REQUEST);
        }

        User creator = getUserOrThrow(creatorId);
        User target = getUserOrThrow(request.getTargetUserId());

        Optional<ChatRoom> existing = chatRoomRepository.findPrivateRoomWithMembers(
                ChatRoom.ChatRoomType.private_chat,
                creatorId,
                target.getId(),
                2
        );
        if (existing.isPresent()) {
            return toResponse(existing.get());
        }

        String roomName = Optional.ofNullable(request.getName())
                .filter(name -> !name.isBlank())
                .orElseGet(() -> target.getUsername() + " & " + creator.getUsername());

        ChatRoom room = ChatRoom.builder()
                .roomType(ChatRoom.ChatRoomType.private_chat)
                .name(roomName)
                .createdBy(creator)
                .description("Phòng chat riêng tư 1-1")
                .build();

        ChatRoom savedRoom = chatRoomRepository.save(room);
        addMember(savedRoom, creator, null, ChatRoomMember.ChatMemberRole.admin);
        addMember(savedRoom, target, null, ChatRoomMember.ChatMemberRole.member);

        return toResponse(savedRoom);
    }

    @Transactional
    public ChatRoomResponse joinRoom(UUID userId, UUID roomId, JoinRoomRequest request) {
        ChatRoom room = getRoomOrThrow(roomId);
        if (!Boolean.TRUE.equals(room.getActive())) {
            throw new AppException(ErrorCode.BAD_REQUEST);
        }
        if (chatRoomMemberRepository.existsByRoom_IdAndUser_Id(roomId, userId)) {
            return toResponse(room);
        }

        User user = getUserOrThrow(userId);
        Person person = request.getPersonId() != null ? getPersonOrThrow(request.getPersonId()) : null;
        addMember(room, user, person, Optional.ofNullable(request.getRole()).orElse(ChatRoomMember.ChatMemberRole.member));
        return toResponse(room);
    }

    @Transactional(readOnly = true)
    public List<ChatRoomResponse> findRoomsForUser(UUID userId) {
        List<ChatRoomMember> memberships = chatRoomMemberRepository.findByUser_Id(userId);
        return memberships.stream()
                .map(ChatRoomMember::getRoom)
                .distinct()
                .map(this::toResponse)
                .toList();
    }

    @Transactional(readOnly = true)
    public ChatRoom getRoomOrThrow(UUID roomId) {
        return chatRoomRepository.findById(roomId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));
    }

    @Transactional(readOnly = true)
    public ChatRoomMember ensureMembership(UUID roomId, UUID userId) {
        return chatRoomMemberRepository.findByRoom_IdAndUser_Id(roomId, userId)
                .orElseThrow(() -> new AppException(ErrorCode.UNAUTHORIZED));
    }

    private void validateCreateRequest(ChatRoomCreateRequest request) {
        if (request.getRoomType() == ChatRoom.ChatRoomType.family || request.getRoomType() == ChatRoom.ChatRoomType.branch) {
            if (request.getFamilyTreeId() == null) {
                throw new AppException(ErrorCode.BAD_REQUEST);
            }
        }
        if (request.getRoomType() == ChatRoom.ChatRoomType.private_chat && request.getFamilyTreeId() != null) {
            throw new AppException(ErrorCode.BAD_REQUEST);
        }
    }

    private FamilyTree resolveFamilyTree(ChatRoomCreateRequest request) {
        if (request.getFamilyTreeId() == null) {
            return null;
        }
        return familyTreeRepository.findById(request.getFamilyTreeId())
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
    }

    private void addMember(ChatRoom room, User user, Person person, ChatRoomMember.ChatMemberRole role) {
        ChatRoomMember member = ChatRoomMember.builder()
                .id(new ChatRoomMemberId(room.getId(), user.getId()))
                .room(room)
                .user(user)
                .person(person)
                .role(role)
                .build();
        chatRoomMemberRepository.save(member);
    }

    private User getUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    private Person getPersonOrThrow(UUID personId) {
        return personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
    }

    private ChatRoomResponse toResponse(ChatRoom room) {
        List<ChatRoomMemberResponse> members = chatRoomMemberRepository.findByRoom_Id(room.getId())
                .stream()
                .map(member -> ChatRoomMemberResponse.builder()
                        .userId(member.getUser().getId())
                        .username(member.getUser().getUsername())
                        .personId(member.getPerson() != null ? member.getPerson().getId() : null)
                        .role(member.getRole())
                        .joinedAt(member.getJoinedAt())
                        .lastReadAt(member.getLastReadAt())
                        .build())
                .collect(Collectors.toList());

        return ChatRoomResponse.builder()
                .id(room.getId())
                .name(room.getName())
                .description(room.getDescription())
                .roomType(room.getRoomType())
                .active(room.getActive())
                .familyTreeId(room.getFamilyTree() != null ? room.getFamilyTree().getId() : null)
                .createdAt(room.getCreatedAt())
                .updatedAt(room.getUpdatedAt())
                .members(members)
                .build();
    }
}

