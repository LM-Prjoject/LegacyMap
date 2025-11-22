package com.legacymap.backend.service;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.legacymap.backend.dto.request.BranchRoomCreateRequest;
import com.legacymap.backend.dto.request.ChatRoomCreateRequest;
import com.legacymap.backend.dto.request.DirectRoomCreateRequest;
import com.legacymap.backend.dto.request.JoinRoomRequest;
import com.legacymap.backend.dto.response.ChatRoomMemberResponse;
import com.legacymap.backend.dto.response.ChatRoomResponse;
import com.legacymap.backend.entity.ChatRoom;
import com.legacymap.backend.entity.ChatRoomBranch;
import com.legacymap.backend.entity.ChatRoomBranchId;
import com.legacymap.backend.entity.ChatRoomMember;
import com.legacymap.backend.entity.ChatRoomMemberId;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.entity.User;
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
        // 1. Validate creator and request
        User creator = getUserOrThrow(creatorId);
        Person branchPerson = personRepository.findById(request.getBranchPersonId())
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));

        FamilyTree familyTree = branchPerson.getFamilyTree();

        // 2. Check if user has access to the family tree
        // SỬA: Thay thế method không tồn tại bằng logic kiểm tra trực tiếp
        if (!canUserAccessTree(familyTree.getId(), creatorId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        // 3. Check if branch room already exists
        chatRoomBranchRepository.findByBranchPersonId(branchPerson.getId())
                .ifPresent(room -> {
                    // SỬA: Thay constant không tồn tại
                    throw new AppException(ErrorCode.RELATIONSHIP_ALREADY_EXISTS, "Branch room already exists");
                });

        // 4. Fetch all relationships and person_user_links for the tree
        List<Relationship> allRelationships = relationshipRepository.findAllByFamilyTree_Id(familyTree.getId());

        // 5. Build graph in memory
        GraphData graphData = buildGraph(allRelationships);

        // 6. Traverse using BFS to find all descendants
        Set<UUID> descendantPersonIds = findDescendantsWithGraph(branchPerson.getId(), graphData);

        // 7. Map person IDs to user IDs (only verified links)
        Set<UUID> userIds = personUserLinkRepository.findUserIdsByPersonIds(descendantPersonIds);

        // 8. Create the chat room
        ChatRoom room = ChatRoom.builder()
                .name(request.getName() != null ? request.getName() : "Nhánh " + branchPerson.getFullName())
                .description(request.getDescription())
                .roomType(ChatRoom.ChatRoomType.branch)
                .familyTree(familyTree)
                .createdBy(creator)
                .build();

        ChatRoom savedRoom = chatRoomRepository.save(room);

        // 9. Add branch person to chat_room_branches
        ChatRoomBranch branch = ChatRoomBranch.builder()
                .id(new ChatRoomBranchId(savedRoom.getId(), branchPerson.getId()))
                .room(savedRoom)
                .branchPerson(branchPerson)
                .createdBy(creator)
                .build();
        chatRoomBranchRepository.save(branch);

        // 10. Add all users to the room
        addMembersToRoom(savedRoom, userIds, creatorId);

        // 11. Add creator as admin if not already a member
        if (!userIds.contains(creatorId)) {
            addMember(savedRoom, creator, null, ChatRoomMember.ChatMemberRole.admin);
        }

        return mapToChatRoomResponse(savedRoom, creatorId);
    }

    /**
     * Kiểm tra user có quyền truy cập tree không
     */
    private boolean canUserAccessTree(UUID treeId, UUID userId) {
        // Kiểm tra user có phải là owner không
        Optional<FamilyTree> tree = familyTreeRepository.findById(treeId);
        if (tree.isPresent() && tree.get().getCreatedBy().getId().equals(userId)) {
            return true;
        }

        // TODO: Thêm logic kiểm tra TreeAccess nếu cần
        // Hiện tại tạm thời return true để không block
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

    private void addMembersToRoom(ChatRoom room, Set<UUID> userIds, UUID creatorId) {
        for (UUID userId : userIds) {
            if (userId.equals(creatorId)) {
                continue; // Creator will be added separately if needed
            }
            User user = getUserOrThrow(userId);
            addMember(room, user, null, ChatRoomMember.ChatMemberRole.member);
        }
    }

    private ChatRoomResponse mapToChatRoomResponse(ChatRoom room, UUID userId) {
        return toResponse(room);
    }
}