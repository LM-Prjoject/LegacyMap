package com.legacymap.backend.service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

import com.legacymap.backend.dto.request.UpdateRoomRequest;
import com.legacymap.backend.dto.response.ChatMessageResponse;
import com.legacymap.backend.entity.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.messaging.simp.SimpMessagingTemplate;

import com.legacymap.backend.dto.request.BranchRoomCreateRequest;
import com.legacymap.backend.dto.request.ChatRoomCreateRequest;
import com.legacymap.backend.dto.request.DirectRoomCreateRequest;
import com.legacymap.backend.dto.response.ChatRoomMemberResponse;
import com.legacymap.backend.dto.response.ChatRoomResponse;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.ChatRoomBranchRepository;
import com.legacymap.backend.repository.ChatRoomMemberRepository;
import com.legacymap.backend.repository.ChatRoomRepository;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.PersonUserLinkRepository;
import com.legacymap.backend.repository.RelationshipRepository;
import com.legacymap.backend.repository.UserRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ChatRoomService {

    private final ChatRoomRepository chatRoomRepository;
    private final ChatRoomMemberRepository chatRoomMemberRepository;
    private final ChatRoomBranchRepository chatRoomBranchRepository;
    private final UserRepository userRepository;
    private final FamilyTreeRepository familyTreeRepository;
    private final PersonRepository personRepository;
    private final PersonUserLinkRepository personUserLinkRepository;
    private final RelationshipRepository relationshipRepository;
    private final SimpMessagingTemplate messagingTemplate;

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
    public ChatRoomResponse createBranchRoom(UUID creatorId, BranchRoomCreateRequest request) {
        // Validate creator and request
        User creator = getUserOrThrow(creatorId);
        Person branchPerson = personRepository.findById(request.getBranchPersonId())
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));

        FamilyTree familyTree = branchPerson.getFamilyTree();
        
        // Check if user has access to the family tree
        if (!familyTreeRepository.existsByIdAndUserHasAccess(familyTree.getId(), creatorId)) {
            throw new AppException(ErrorCode.ACCESS_DENIED);
        }
        
        // Check if branch room already exists
        chatRoomBranchRepository.findByBranchPersonId(branchPerson.getId())
                .ifPresent(room -> {
                    // SỬA: Thay constant không tồn tại
                    throw new AppException(ErrorCode.RELATIONSHIP_ALREADY_EXISTS, "Branch room already exists");
                });
        
        // Fetch all relationships and person_user_links for the tree
        List<Relationship> allRelationships = relationshipRepository.findAllByFamilyTree_Id(familyTree.getId());
        
        // Build graph in memory
        GraphData graphData = buildGraph(allRelationships);
        
        // Traverse using BFS to find all descendants
        Set<UUID> descendantPersonIds = findDescendantsWithGraph(branchPerson.getId(), graphData);
        
        // Map person IDs to user IDs (only verified links)
        Set<UUID> userIds = personUserLinkRepository.findUserIdsByPersonIds(descendantPersonIds);
        
        // Create the chat room
        ChatRoom room = ChatRoom.builder()
                .name(request.getName() != null ? request.getName() : "Nhánh " + branchPerson.getFullName())
                .description(request.getDescription())
                .roomType(ChatRoom.ChatRoomType.branch)
                .familyTree(familyTree)
                .createdBy(creator)
                .build();

        ChatRoom savedRoom = chatRoomRepository.save(room);
        
        // Add branch person to chat_room_branches
        ChatRoomBranch branch = ChatRoomBranch.builder()
                .id(new ChatRoomBranchId(savedRoom.getId(), branchPerson.getId()))
                .room(savedRoom)
                .branchPerson(branchPerson)
                .createdBy(creator)
                .build();
        chatRoomBranchRepository.save(branch);
        
        // Add all users to the room
        addMembersToRoom(savedRoom, userIds, creatorId);
        
        // Add creator as admin if not already a member
        if (!userIds.contains(creatorId)) {
            addMember(savedRoom, creator, null, ChatRoomMember.ChatMemberRole.admin);
        }

        return mapToChatRoomResponse(savedRoom, creatorId);
    }

    /**
     * Kiểm tra user có quyền truy cập tree không
     */
    private boolean canUserAccessTree(UUID treeId, UUID userId) {
        Optional<FamilyTree> tree = familyTreeRepository.findById(treeId);
        if (tree.isPresent() && tree.get().getCreatedBy().getId().equals(userId)) {
            return true;
        }

        return true;
    }

    /**
     * Build graph structure from relationships
     * - parentToChildren: Map parent ID -> List of child IDs
     * - personToSpouses: Map person ID -> List of spouse IDs
     */
    private GraphData buildGraph(List<Relationship> relationships) {
        java.util.Map<UUID, List<UUID>> parentToChildren = new java.util.HashMap<>();
        java.util.Map<UUID, List<UUID>> personToSpouses = new java.util.HashMap<>();

        for (Relationship rel : relationships) {
            UUID p1Id = rel.getPerson1().getId();
            UUID p2Id = rel.getPerson2().getId();
            String type = rel.getRelationshipType();

            if ("parent".equals(type)) {
                // person1 is parent of person2
                parentToChildren.computeIfAbsent(p1Id, k -> new java.util.ArrayList<>()).add(p2Id);
            } else if ("spouse".equals(type)) {
                // Bidirectional relationship
                personToSpouses.computeIfAbsent(p1Id, k -> new java.util.ArrayList<>()).add(p2Id);
                personToSpouses.computeIfAbsent(p2Id, k -> new java.util.ArrayList<>()).add(p1Id);
            }
        }

        return new GraphData(parentToChildren, personToSpouses);
    }

    /**
     * Find all descendants using BFS starting from root person
     * Includes spouses of descendants
     */
    private Set<UUID> findDescendantsWithGraph(UUID rootPersonId, GraphData graphData) {
        Set<UUID> result = new HashSet<>();
        Queue<UUID> queue = new LinkedList<>();
        queue.add(rootPersonId);
        result.add(rootPersonId);

        while (!queue.isEmpty()) {
            UUID currentId = queue.poll();

            // Add children
            List<UUID> children = graphData.parentToChildren.getOrDefault(currentId, java.util.Collections.emptyList());
            for (UUID childId : children) {
                if (!result.contains(childId)) {
                    result.add(childId);
                    queue.add(childId);

                    // Add spouse of child
                    List<UUID> spouses = graphData.personToSpouses.getOrDefault(childId, java.util.Collections.emptyList());
                    for (UUID spouseId : spouses) {
                        if (!result.contains(spouseId)) {
                            result.add(spouseId);
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Helper class to hold graph data
     */
    private static class GraphData {
        final java.util.Map<UUID, List<UUID>> parentToChildren;
        final java.util.Map<UUID, List<UUID>> personToSpouses;

        GraphData(java.util.Map<UUID, List<UUID>> parentToChildren,
                  java.util.Map<UUID, List<UUID>> personToSpouses) {
            this.parentToChildren = parentToChildren;
            this.personToSpouses = personToSpouses;
        }
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
                .description("1-1 private chat room")
                .build();

        ChatRoom savedRoom = chatRoomRepository.save(room);
        addMember(savedRoom, creator, null, ChatRoomMember.ChatMemberRole.admin);
        addMember(savedRoom, target, null, ChatRoomMember.ChatMemberRole.member);

        return toResponse(savedRoom);
    }

    private void broadcastSystemMessage(UUID roomId, String messageText) {
        ChatMessageResponse systemMessage = ChatMessageResponse.builder()
                .id(UUID.randomUUID())
                .roomId(roomId)
                .senderId(null)
                .senderName("System")
                .messageText(messageText)
                .messageType(ChatMessage.ChatMessageType.system)
                .fileUrl(null)
                .fileName(null)
                .fileSize(null)
                .fileType(null)
                .replyToId(null)
                .edited(false)
                .deleted(false)
                .createdAt(OffsetDateTime.now())
                .updatedAt(OffsetDateTime.now())
                .recipients(null)
                .build();

        messagingTemplate.convertAndSend("/topic/chat/" + roomId, systemMessage);
    }

    @Transactional
    public void leaveRoom(UUID userId, UUID roomId) {
        ChatRoomMember member = chatRoomMemberRepository.findByRoom_IdAndUser_Id(roomId, userId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));

        ChatRoom room = member.getRoom();

        if (room.getCreatedBy().getId().equals(userId)) {
            long adminCount = chatRoomMemberRepository.findByRoom_Id(roomId).stream()
                    .filter(m -> "admin".equals(m.getRole()))
                    .count();
            if (adminCount <= 1) {
                throw new AppException(ErrorCode.BAD_REQUEST, "Creator cannot leave room without another admin");
            }
        }

        chatRoomMemberRepository.delete(member);

        String leaveMessage = member.getUser().getUsername() + " đã rời khời phòng";
        broadcastSystemMessage(roomId, leaveMessage);
    }

    @Transactional
    public ChatRoomResponse updateRoom(UUID userId, UUID roomId, UpdateRoomRequest request) {
        ChatRoom room = chatRoomRepository.findActiveById(roomId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND, "The room does not exist or has been deleted"));

        if (room.getRoomType() == ChatRoom.ChatRoomType.family ||
                room.getRoomType() == ChatRoom.ChatRoomType.private_chat) {
            if (request.getName() != null && !request.getName().trim().equals(room.getName())) {
                throw new AppException(ErrorCode.ACCESS_DENIED, "You are not allowed to rename this room");
            }
        }

        if (room.getRoomType() == ChatRoom.ChatRoomType.branch) {
            boolean isAdmin = chatRoomMemberRepository.isAdmin(roomId, userId);
            ChatRoomMember member = chatRoomMemberRepository.findByRoom_IdAndUser_Id(roomId, userId).orElse(null);
            boolean isModerator = member != null && member.getRole() == ChatRoomMember.ChatMemberRole.moderator;

            if (!isAdmin && !isModerator) {
                throw new AppException(ErrorCode.ACCESS_DENIED, "Only admins or moderators are allowed to rename the room");
            }
        }

        String oldName = room.getName();
        String newName = request.getName() != null ? request.getName().trim() : room.getName();

        if (request.getName() != null) {
            room.setName(newName);
        }
        if (request.getDescription() != null) {
            room.setDescription(request.getDescription());
        }

        ChatRoom savedRoom = chatRoomRepository.saveAndFlush(room);

        if (request.getName() != null && !newName.equals(oldName)) {
            String actorName = userRepository.findById(userId)
                    .map(User::getUsername)
                    .orElse("Someone");

            broadcastSystemMessage(roomId, actorName + " đã đổi tên phòng thành \"" + savedRoom.getName() + "\"");
        }

        return toResponse(savedRoom);
    }

    @Transactional
    public void deactivateRoom(UUID userId, UUID roomId) {
        ChatRoom room = chatRoomRepository.findActiveById(roomId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));

        if (room.getRoomType() != ChatRoom.ChatRoomType.branch &&
                room.getRoomType() != ChatRoom.ChatRoomType.group) {
            throw new AppException(ErrorCode.ACCESS_DENIED, "Only branch or group rooms can be deleted");
        }

        boolean isCreator = room.getCreatedBy().getId().equals(userId);
        boolean isAdmin = chatRoomMemberRepository.isAdmin(roomId, userId);

        if (!isCreator && !isAdmin) {
            throw new AppException(ErrorCode.ACCESS_DENIED, "Only the creator or an admin is allowed to delete a branch room");
        }

        chatRoomRepository.delete(room);

        broadcastSystemMessage(roomId, "Phòng chat đã bị xóa bởi quản trị viên");
    }

    @Transactional
    public ChatRoomResponse updateMyMembership(UUID roomId, UUID userId, Map<String, Object> payload) {
        ChatRoomMember myMember = chatRoomMemberRepository.findByRoom_IdAndUser_Id(roomId, userId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));

        boolean changed = false;

        if (payload.containsKey("nickname")) {
            if (myMember.getRoom().getRoomType() != ChatRoom.ChatRoomType.private_chat) {
                throw new AppException(ErrorCode.BAD_REQUEST, "Nicknames can only be set in private chats");
            }

            String nick = payload.get("nickname") instanceof String s ? s.trim() : null;
            if (nick != null && nick.isEmpty()) nick = null;

            ChatRoomMember otherMember = chatRoomMemberRepository.findByRoom_IdAndUser_IdNot(roomId, userId)
                    .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND, "Other member not found"));

            if (!Objects.equals(otherMember.getNickname(), nick)) {
                otherMember.setNickname(nick);
                changed = true;

                String actorName = userRepository.findById(userId)
                        .map(User::getUsername)
                        .orElse("Someone");

                messagingTemplate.convertAndSend("/topic/chat/" + roomId,
                        ChatMessageResponse.builder()
                                .messageType(ChatMessage.ChatMessageType.system)
                                .messageText(actorName + " đã đổi biệt danh")
                                .createdAt(OffsetDateTime.now())
                                .build());
            }
        }

        if (payload.containsKey("muted")) {
            boolean muted = Boolean.TRUE.equals(payload.get("muted"));
            boolean currentMuted = myMember.getMuted() != null && myMember.getMuted();

            if (currentMuted != muted) {
                myMember.setMuted(muted);
                changed = true;
            }
        }

        if (changed) {
            chatRoomMemberRepository.save(myMember);
            if (payload.containsKey("nickname")) {
                chatRoomMemberRepository.save(
                        chatRoomMemberRepository.findByRoom_IdAndUser_IdNot(roomId, userId)
                                .orElse(myMember)
                );
            }
        }

        return toResponse(myMember.getRoom());
    }

    @Transactional(readOnly = true)
    public List<ChatRoomResponse> findRoomsForUser(UUID userId) {
        List<ChatRoomMember> memberships = chatRoomMemberRepository.findByUser_Id(userId);
        return memberships.stream()
                .map(ChatRoomMember::getRoom)
                .filter(room -> room.getActive())
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

    public ChatRoomResponse toResponse(ChatRoom room) {
        List<ChatRoomMemberResponse> members = chatRoomMemberRepository.findByRoom_Id(room.getId())
                .stream()
                .map(member -> ChatRoomMemberResponse.builder()
                        .userId(member.getUser().getId())
                        .username(member.getUser().getUsername())
                        .personId(member.getPerson() != null ? member.getPerson().getId() : null)
                        .role(member.getRole())
                        .joinedAt(member.getJoinedAt())
                        .lastReadAt(member.getLastReadAt())
                        .isMuted(member.getMuted())
                        .nickname(member.getNickname())
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

    private void addMembersToRoom(ChatRoom room, Set<UUID> userIds, UUID creatorId) {
        for (UUID userId : userIds) {
            if (userId.equals(creatorId)) {
                continue;
            }
            User user = getUserOrThrow(userId);
            addMember(room, user, null, ChatRoomMember.ChatMemberRole.member);
        }
    }

    private ChatRoomResponse mapToChatRoomResponse(ChatRoom room, UUID userId) {
        return toResponse(room);
    }
}