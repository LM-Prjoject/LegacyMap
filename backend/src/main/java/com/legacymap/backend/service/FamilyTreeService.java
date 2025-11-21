package com.legacymap.backend.service;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.dto.request.PersonUpdateRequest;
import com.legacymap.backend.entity.ChatRoom;
import com.legacymap.backend.entity.ChatRoomMember;
import com.legacymap.backend.entity.ChatRoomMemberId;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.PersonUserLink;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.ChatRoomMemberRepository;
import com.legacymap.backend.repository.ChatRoomRepository;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.PersonUserLinkRepository;
import com.legacymap.backend.repository.UserRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class FamilyTreeService {

    private final FamilyTreeRepository familyTreeRepository;
    private final PersonRepository personRepository;
    private final UserRepository userRepository;
    private final ChatRoomRepository chatRoomRepository;
    private final ChatRoomMemberRepository chatRoomMemberRepository;
    private final PersonUserLinkRepository personUserLinkRepository;

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    @Transactional(readOnly = true)
    public UUID getOwnerId(UUID treeId) {
        return familyTreeRepository.findById(treeId)
                .map(t -> t.getCreatedBy() != null ? t.getCreatedBy().getId() : null)
                .orElse(null);
    }

    @Transactional
    public FamilyTree create(UUID userId, FamilyTreeCreateRequest req) {
        User creator = loadUserOrThrow(userId);
        FamilyTree tree = FamilyTree.builder()
                .name(req.getName())
                .description(req.getDescription())
                .isPublic(req.getIsPublic() != null ? req.getIsPublic() : false)
                .coverImageUrl(req.getCoverImageUrl())
                .createdBy(creator)
                .build();
        FamilyTree savedTree = familyTreeRepository.save(tree);
        
        // Tự động tạo Family Room khi tạo cây mới
        createFamilyRoomForTree(savedTree, creator);
        
        return savedTree;
    }
    
    private void createFamilyRoomForTree(FamilyTree tree, User creator) {
        ChatRoom familyRoom = ChatRoom.builder()
                .familyTree(tree)
                .name("Phòng chat gia đình: " + tree.getName())
                .description("Phòng chat chung cho toàn bộ thành viên trong cây gia phả")
                .roomType(ChatRoom.ChatRoomType.family)
                .createdBy(creator)
                .active(true)
                .build();
        ChatRoom savedRoom = chatRoomRepository.save(familyRoom);
        
        // Thêm creator làm admin
        ChatRoomMember creatorMember = ChatRoomMember.builder()
                .id(new ChatRoomMemberId(savedRoom.getId(), creator.getId()))
                .room(savedRoom)
                .user(creator)
                .person(null)
                .role(ChatRoomMember.ChatMemberRole.admin)
                .build();
        chatRoomMemberRepository.save(creatorMember);
    }

    private FamilyTree findOwnedTreeOrThrow(UUID treeId, UUID userId) {
        User user = loadUserOrThrow(userId);
        return familyTreeRepository.findByIdAndCreatedBy(treeId, user)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
    }

    @Transactional
    public FamilyTree update(UUID treeId, UUID userId, FamilyTreeUpdateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        if (req.getName() != null) tree.setName(req.getName());
        if (req.getDescription() != null) tree.setDescription(req.getDescription());
        if (req.getIsPublic() != null) tree.setIsPublic(req.getIsPublic());
        if (req.getCoverImageUrl() != null) tree.setCoverImageUrl(req.getCoverImageUrl());
        return familyTreeRepository.save(tree);
    }

    @Transactional
    public void delete(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        familyTreeRepository.delete(tree);
    }

    @Transactional
    public Person addMember(UUID treeId, UUID userId, PersonCreateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        User creator = loadUserOrThrow(userId);
        // Validate unique email within the same tree (if provided)
        String email = req.getEmail() != null ? req.getEmail().trim().toLowerCase() : null;
        if (email != null && !email.isBlank()) {
            boolean exists = personRepository.existsByFamilyTree_IdAndEmailIgnoreCase(tree.getId(), email);
            if (exists) {
                throw new AppException(ErrorCode.PERSON_EMAIL_EXISTS_IN_TREE);
            }
        }
        Person p = Person.builder()
                .familyTree(tree)
                .fullName(req.getFullName())
                .gender(req.getGender())
                .birthDate(req.getBirthDate())
                .deathDate(req.getDeathDate())
                .birthPlace(req.getBirthPlace())
                .deathPlace(req.getDeathPlace())
                .biography(req.getBiography())
                .avatarUrl(req.getAvatarUrl())
                .email(email)
                .phone(req.getPhone())
                .createdBy(creator)
                .build();
        return personRepository.save(p);
    }

    @Transactional(readOnly = true)
    public List<FamilyTree> listByUser(UUID userId) {
        User user = loadUserOrThrow(userId);
        return familyTreeRepository.findAllByCreatedBy(user);
    }

    @Transactional(readOnly = true)
    public List<Person> listMembers(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        return personRepository.findAllByFamilyTree_Id(tree.getId());
    }

    @Transactional(readOnly = true)
    public boolean hasViewerAccess(UUID treeId, UUID userId) {
        // ensure user exists
        loadUserOrThrow(userId);
        // a user can view if they have a verified self-link to any person in the tree
        return personUserLinkRepository
                .existsByUser_IdAndPerson_FamilyTree_IdAndLinkTypeAndStatus(
                        userId, treeId, PersonUserLink.LinkType.self, PersonUserLink.Status.approved);
    }

    @Transactional(readOnly = true)
    public List<Person> listMembersForViewer(UUID treeId, UUID userId) {
        if (!hasViewerAccess(treeId, userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }
        return personRepository.findAllByFamilyTree_Id(treeId);
    }

    @Transactional(readOnly = true)
    public List<FamilyTree> listViewableTrees(UUID userId) {
        loadUserOrThrow(userId);
        List<PersonUserLink> links = personUserLinkRepository
                .findByUser_IdAndLinkTypeAndStatus(userId, PersonUserLink.LinkType.self, PersonUserLink.Status.approved);
        // Collect distinct tree IDs in order
        List<UUID> treeIds = links.stream()
                .map(l -> l.getPerson())
                .filter(p -> p != null && p.getFamilyTree() != null && p.getFamilyTree().getId() != null)
                .map(p -> p.getFamilyTree().getId())
                .collect(Collectors.toList());
        // preserve insertion order and uniqueness
        Map<UUID, Boolean> seen = new java.util.LinkedHashMap<>();
        List<UUID> distinctIds = new java.util.ArrayList<>();
        for (UUID id : treeIds) {
            if (!seen.containsKey(id)) {
                seen.put(id, Boolean.TRUE);
                distinctIds.add(id);
            }
        }
        // Fetch managed entities from repository
        List<FamilyTree> fetched = familyTreeRepository.findAllById(distinctIds);
        // Keep original order of distinctIds
        Map<UUID, FamilyTree> map = fetched.stream().collect(Collectors.toMap(FamilyTree::getId, t -> t));
        List<FamilyTree> ordered = new java.util.ArrayList<>();
        for (UUID id : distinctIds) {
            FamilyTree t = map.get(id);
            if (t != null) ordered.add(t);
        }
        return ordered;
    }

    @Transactional
    public Person updateMember(UUID treeId, UUID userId, UUID personId, PersonUpdateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }
        if (req.getFullName() != null) p.setFullName(req.getFullName());
        if (req.getGender() != null) p.setGender(req.getGender());
        if (req.getBirthDate() != null) p.setBirthDate(req.getBirthDate());
        if (req.getDeathDate() != null) p.setDeathDate(req.getDeathDate());
        if (req.getBirthPlace() != null) p.setBirthPlace(req.getBirthPlace());
        if (req.getDeathPlace() != null) p.setDeathPlace(req.getDeathPlace());
        if (req.getBiography() != null) p.setBiography(req.getBiography());
        if (req.getAvatarUrl() != null) p.setAvatarUrl(req.getAvatarUrl());
        if (req.getEmail() != null) {
            String newEmail = req.getEmail() != null ? req.getEmail().trim().toLowerCase() : null;
            String currentEmail = p.getEmail() != null ? p.getEmail().trim().toLowerCase() : null;
            boolean changed = (newEmail == null && currentEmail != null) || (newEmail != null && !newEmail.equals(currentEmail));
            if (changed && newEmail != null && !newEmail.isBlank()) {
                boolean exists = personRepository.existsByFamilyTree_IdAndEmailIgnoreCase(tree.getId(), newEmail);
                if (exists) throw new AppException(ErrorCode.PERSON_EMAIL_EXISTS_IN_TREE);
            }
            p.setEmail(newEmail);
        }
        if (req.getPhone() != null) p.setPhone(req.getPhone());
        return personRepository.save(p);
    }

    @Transactional
    public void deleteMember(UUID treeId, UUID userId, UUID personId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }
        personRepository.delete(p);
    }
}
