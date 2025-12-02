package com.legacymap.backend.service;

import java.util.*;
import java.util.stream.Collectors;

import com.legacymap.backend.entity.*;
import com.legacymap.backend.repository.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.dto.request.PersonUpdateRequest;
import com.legacymap.backend.dto.request.NotificationCreateRequest;
import com.legacymap.backend.dto.response.TreeShareResponse;
import com.legacymap.backend.dto.response.SharedTreeAccessInfoResponse;
import com.legacymap.backend.dto.response.NotificationResponse;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;

@Slf4j
@Service
@RequiredArgsConstructor
public class FamilyTreeService {

    private final FamilyTreeRepository familyTreeRepository;
    private final PersonRepository personRepository;
    private final UserRepository userRepository;
    private final ChatRoomRepository chatRoomRepository;
    private final ChatRoomMemberRepository chatRoomMemberRepository;
    private final PersonUserLinkRepository personUserLinkRepository;
    private final AvatarGenerationService avatarGenerationService;
    private final RelationshipRepository relationshipRepository;

    @Autowired
    private TreeAccessRepository treeAccessRepository;

    @Autowired
    private EmailService emailService;

    @Autowired
    private AuditLogService historyService;

    @Autowired
    private NotificationService notificationService;
    @Autowired
    private com.legacymap.backend.repository.NotificationRepository notificationRepository;
    @Autowired
    private ObjectMapper objectMapper;

    @Value("${app.frontend.url}")
    private String frontendUrl;

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

    @Transactional(readOnly = true)
    public FamilyTree getTreeById(UUID treeId) {
        return familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
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

    private FamilyTree findEditableTreeOrThrow(UUID treeId, UUID userId) {
        FamilyTree tree = familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
        
        // Check if user is owner or has edit access
        if (tree.getCreatedBy().getId().equals(userId)) {
            return tree;
        }
        
        Optional<TreeAccess> access = treeAccessRepository.findByUserIdAndFamilyTreeId(userId, treeId);
        if (access.isPresent() && "edit".equals(access.get().getAccessLevel())) {
            return tree;
        }
        
        throw new AppException(ErrorCode.PERMISSION_DENIED);
    }

    @Transactional
    public FamilyTree update(UUID treeId, UUID userId, FamilyTreeUpdateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        boolean nameChanged = req.getName() != null && !req.getName().equals(tree.getName());

        if (req.getName() != null) tree.setName(req.getName());
        if (req.getDescription() != null) tree.setDescription(req.getDescription());
        if (req.getIsPublic() != null) tree.setIsPublic(req.getIsPublic());
        if (req.getCoverImageUrl() != null) tree.setCoverImageUrl(req.getCoverImageUrl());
        FamilyTree saved = familyTreeRepository.save(tree);

        if (nameChanged) {
            chatRoomRepository.findByFamilyTreeIdAndRoomType(treeId, ChatRoom.ChatRoomType.family)
                    .ifPresent(room -> {
                        room.setName("Phòng chat gia đình: " + saved.getName());
                        chatRoomRepository.save(room);
                    });
        }

        return saved;
    }

    @Transactional
    public void delete(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        familyTreeRepository.delete(tree);
    }

    @Transactional
    public Person addMember(UUID treeId, UUID userId, PersonCreateRequest req) {

        FamilyTree tree = findEditableTreeOrThrow(treeId, userId);
        User creator = loadUserOrThrow(userId);

        String email = req.getEmail() != null ? req.getEmail().trim().toLowerCase() : null;
        if (email != null && !email.isBlank()) {
            boolean exists = personRepository.existsByFamilyTree_IdAndEmailIgnoreCase(tree.getId(), email);
            if (exists) {
                throw new AppException(ErrorCode.EMAIL_EXISTED, "Email already exists in this family tree");
            }
        }

        String avatarUrl = req.getAvatarUrl();
        if (avatarUrl == null || avatarUrl.isBlank()) {
            avatarUrl = avatarGenerationService.generateAvatar(
                    req.getFullName(),
                    req.getGender(),
                    req.getBirthDate(),
                    req.getDeathDate()
            );
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
                .avatarUrl(avatarUrl)
                .email(email)
                .phone(req.getPhone())
                .createdBy(creator)
                .build();

        Person savedPerson = personRepository.save(p);
        historyService.logMemberCreated(treeId, userId, savedPerson.getId(), savedPerson);
        return savedPerson;
    }

    @Transactional(readOnly = true)
    public List<FamilyTree> listByUser(UUID userId) {
        return getUserTrees(userId);
    }

    @Transactional(readOnly = true)
    public List<Person> listMembers(UUID treeId, UUID userId) {
        FamilyTree tree = familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        boolean hasAccess = tree.getCreatedBy().getId().equals(userId)
                || canView(treeId, userId);

        if (!hasAccess) {
            throw new AppException(ErrorCode.PERMISSION_DENIED);
        }

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
        FamilyTree tree = findEditableTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }

        // Clone person trước khi update
        Person oldPerson = Person.builder()
                .id(p.getId())
                .familyTree(p.getFamilyTree())
                .fullName(p.getFullName())
                .gender(p.getGender())
                .birthDate(p.getBirthDate())
                .deathDate(p.getDeathDate())
                .birthPlace(p.getBirthPlace())
                .deathPlace(p.getDeathPlace())
                .biography(p.getBiography())
                .avatarUrl(p.getAvatarUrl())
                .email(p.getEmail())
                .phone(p.getPhone())
                .createdBy(p.getCreatedBy())
                .build();

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
                if (exists) {
                    throw new AppException(ErrorCode.EMAIL_EXISTED, "Email already exists in this family tree");
                }
            }
            p.setEmail(newEmail);
        }
        if (req.getPhone() != null) p.setPhone(req.getPhone());

        Person updated = personRepository.save(p);
        historyService.logMemberUpdated(treeId, userId, p.getId(), oldPerson, updated);
        return updated;
    }

    @Transactional
    public void deleteMember(UUID treeId, UUID userId, UUID personId) {
        FamilyTree tree = findEditableTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }

        historyService.logMemberDeleted(treeId, userId, p.getId(), p);
        personRepository.delete(p);
    }

    @Transactional
    public void deleteMemberSafe(UUID treeId, UUID userId, UUID personId) {
        FamilyTree tree = findEditableTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }

        java.util.Set<UUID> parentIds = new java.util.HashSet<>(relationshipRepository.findParentIdsByPersonId(personId));
        java.util.Set<UUID> anchorRoots = computeAncestorRoots(tree.getId(), parentIds);

        List<Relationship> links = relationshipRepository.findByPerson1IdOrPerson2Id(personId, personId);
        // Seed set: children + spouses
        java.util.Set<UUID> seedIds = new java.util.HashSet<>(relationshipRepository.findChildIdsByPersonId(personId));
        if (!links.isEmpty()) {
            for (Relationship r : links) {
                String t = r.getRelationshipType();
                if (t != null && t.trim().equalsIgnoreCase("spouse")) {
                    UUID a = r.getPerson1() != null ? r.getPerson1().getId() : null;
                    UUID b = r.getPerson2() != null ? r.getPerson2().getId() : null;
                    UUID spouse = personId.equals(a) ? b : a;
                    if (spouse != null) seedIds.add(spouse);
                }
            }
            relationshipRepository.deleteAll(links);
        }
        personRepository.delete(p);

        // Always prune using KEEP roots: ancestor roots if available, otherwise global roots
        if (anchorRoots == null || anchorRoots.isEmpty()) {
            pruneComponentFromSeeds(tree.getId(), seedIds, /*keepRoots*/ null);
        } else {
            pruneComponentFromSeeds(tree.getId(), seedIds, anchorRoots);
        }
    }

    private java.util.Set<UUID> computeAncestorRoots(UUID treeId, java.util.Set<UUID> startParents) {
        // Walk upwards via parent edges to any nodes with no parent; return that root set
        java.util.Set<UUID> roots = new java.util.HashSet<>();
        if (startParents == null || startParents.isEmpty()) return roots;
        java.util.ArrayDeque<UUID> dq = new java.util.ArrayDeque<>(startParents);
        java.util.Set<UUID> visited = new java.util.HashSet<>();
        while (!dq.isEmpty()) {
            UUID u = dq.pollFirst();
            if (!visited.add(u)) continue;
            java.util.List<UUID> uParents = relationshipRepository.findParentIdsByPersonId(u);
            if (uParents == null || uParents.isEmpty()) {
                roots.add(u);
            } else {
                dq.addAll(uParents);
            }
        }
        return roots;
    }

    private void pruneComponentFromSeeds(UUID treeId, java.util.Set<UUID> seedIds, java.util.Set<UUID> keepRoots) {
        pruneComponentFromSeedsOpt(treeId, seedIds, keepRoots, true);
    }

    private void pruneComponentFromSeedsOpt(UUID treeId, java.util.Set<UUID> seedIds, java.util.Set<UUID> keepRoots, boolean useGlobalRootsIfKeepEmpty) {
        if (seedIds == null || seedIds.isEmpty()) return;

        // Build graph from current persons and relationships (bloodline-only)
        java.util.List<Person> persons = personRepository.findAllByFamilyTree_Id(treeId);
        if (persons.isEmpty()) return;
        java.util.List<Relationship> rels = relationshipRepository.findByFamilyTreeId(treeId);

        java.util.Map<UUID, java.util.Set<UUID>> graph = new java.util.HashMap<>();
        java.util.Set<UUID> allIds = new java.util.HashSet<>();
        for (Person person : persons) {
            allIds.add(person.getId());
            graph.put(person.getId(), new java.util.HashSet<>());
        }
        for (Relationship r : rels) {
            String type = r.getRelationshipType();
            if (type == null) continue;
            String t = type.trim().toLowerCase();
            if ("parent".equals(t)) {
                UUID a = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID b = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (a != null && b != null && allIds.contains(a) && allIds.contains(b)) {
                    graph.get(a).add(b);
                    graph.get(b).add(a);
                }
            } else if ("child".equals(t)) {
                UUID b = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID a = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (a != null && b != null && allIds.contains(a) && allIds.contains(b)) {
                    graph.get(a).add(b);
                    graph.get(b).add(a);
                }
            }
        }

        // KEEP set
        java.util.Set<UUID> keep = new java.util.HashSet<>();
        java.util.ArrayDeque<UUID> dq = new java.util.ArrayDeque<>();
        if (keepRoots != null && !keepRoots.isEmpty()) {
            for (UUID r : keepRoots) { if (graph.containsKey(r) && keep.add(r)) dq.add(r); }
        } else if (useGlobalRootsIfKeepEmpty) {
            // global roots: nodes without parent
            java.util.Set<UUID> hasParent = new java.util.HashSet<>();
            for (Relationship r : rels) {
                String t = r.getRelationshipType();
                if (t != null && t.trim().equalsIgnoreCase("parent")) {
                    UUID child = r.getPerson2() != null ? r.getPerson2().getId() : null;
                    if (child != null) hasParent.add(child);
                } else if (t != null && t.trim().equalsIgnoreCase("child")) {
                    UUID child = r.getPerson1() != null ? r.getPerson1().getId() : null;
                    if (child != null) hasParent.add(child);
                }
            }
            for (UUID id : allIds) {
                if (!hasParent.contains(id)) { if (keep.add(id)) dq.add(id); }
            }
        }
        while (!dq.isEmpty()) {
            UUID u = dq.pollFirst();
            for (UUID v : graph.getOrDefault(u, java.util.Collections.emptySet())) {
                if (keep.add(v)) dq.addLast(v);
            }
        }

        // AFFECTED set = component reachable from seeds
        java.util.Set<UUID> affected = new java.util.HashSet<>();
        java.util.ArrayDeque<UUID> dq2 = new java.util.ArrayDeque<>();
        for (UUID s : seedIds) { if (graph.containsKey(s) && affected.add(s)) dq2.add(s); }
        while (!dq2.isEmpty()) {
            UUID u = dq2.pollFirst();
            for (UUID v : graph.getOrDefault(u, java.util.Collections.emptySet())) {
                if (affected.add(v)) dq2.addLast(v);
            }
        }

        // toDelete = affected - keep
        java.util.List<UUID> toDelete = new java.util.ArrayList<>();
        for (UUID id : affected) {
            if (!keep.contains(id)) toDelete.add(id);
        }

        // Also include spouses of toDelete if those spouses are not in KEEP
        if (!toDelete.isEmpty()) {
            java.util.Set<UUID> extraSpouses = new java.util.HashSet<>();
            for (UUID id : new java.util.ArrayList<>(toDelete)) {
                java.util.List<Relationship> relLinks = relationshipRepository.findByPerson1IdOrPerson2Id(id, id);
                for (Relationship r : relLinks) {
                    String t = r.getRelationshipType();
                    if (t != null && t.trim().equalsIgnoreCase("spouse")) {
                        UUID a = r.getPerson1() != null ? r.getPerson1().getId() : null;
                        UUID b = r.getPerson2() != null ? r.getPerson2().getId() : null;
                        UUID sp = id.equals(a) ? b : a;
                        if (sp != null && !keep.contains(sp)) extraSpouses.add(sp);
                    }
                }
            }
            for (UUID sp : extraSpouses) {
                if (!toDelete.contains(sp)) toDelete.add(sp);
            }
        }

        if (!toDelete.isEmpty()) {
            for (UUID id : toDelete) {
                java.util.List<Relationship> rm = relationshipRepository.findByPerson1IdOrPerson2Id(id, id);
                if (!rm.isEmpty()) relationshipRepository.deleteAll(rm);
                personRepository.findById(id).ifPresent(personRepository::delete);
            }
        }
    }

    private void pruneFromRoots(UUID treeId, java.util.Set<UUID> anchorRoots) {
        if (anchorRoots == null || anchorRoots.isEmpty()) return;
        java.util.List<Person> persons = personRepository.findAllByFamilyTree_Id(treeId);
        if (persons.isEmpty()) return;
        java.util.List<Relationship> rels = relationshipRepository.findByFamilyTreeId(treeId);

        java.util.Map<UUID, java.util.Set<UUID>> graph = new java.util.HashMap<>();
        java.util.Set<UUID> allIds = new java.util.HashSet<>();
        for (Person person : persons) {
            allIds.add(person.getId());
            graph.put(person.getId(), new java.util.HashSet<>());
        }
        for (Relationship r : rels) {
            String type = r.getRelationshipType();
            if (type == null) continue;
            String t = type.trim().toLowerCase();
            if ("parent".equals(t)) {
                UUID a = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID b = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (a != null && b != null && allIds.contains(a) && allIds.contains(b)) {
                    graph.get(a).add(b);
                    graph.get(b).add(a);
                }
            } else if ("child".equals(t)) {
                UUID b = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID a = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (a != null && b != null && allIds.contains(a) && allIds.contains(b)) {
                    graph.get(a).add(b);
                    graph.get(b).add(a);
                }
            }
        }

        java.util.Set<UUID> reachable = new java.util.HashSet<>();
        java.util.ArrayDeque<UUID> dq = new java.util.ArrayDeque<>();
        for (UUID r : anchorRoots) {
            if (graph.containsKey(r) && reachable.add(r)) dq.add(r);
        }
        while (!dq.isEmpty()) {
            UUID u = dq.pollFirst();
            for (UUID v : graph.getOrDefault(u, java.util.Collections.emptySet())) {
                if (reachable.add(v)) dq.addLast(v);
            }
        }

        java.util.List<UUID> toDelete = persons.stream()
                .map(Person::getId)
                .filter(id -> !reachable.contains(id))
                .toList();
        if (!toDelete.isEmpty()) {
            for (UUID id : toDelete) {
                java.util.List<Relationship> links = relationshipRepository.findByPerson1IdOrPerson2Id(id, id);
                if (!links.isEmpty()) relationshipRepository.deleteAll(links);
                personRepository.findById(id).ifPresent(personRepository::delete);
            }
        }
    }

    private void pruneDisconnectedFromAncestors(UUID treeId, UUID personId) {
        List<Person> persons = personRepository.findAllByFamilyTree_Id(treeId);
        if (persons.isEmpty()) return;

        List<Relationship> rels = relationshipRepository.findByFamilyTreeId(treeId);

        java.util.Map<UUID, java.util.Set<UUID>> graph = new java.util.HashMap<>();
        java.util.Set<UUID> allIds = new java.util.HashSet<>();
        for (Person person : persons) {
            allIds.add(person.getId());
            graph.put(person.getId(), new java.util.HashSet<>());
        }

        java.util.Set<UUID> hasParent = new java.util.HashSet<>();
        for (Relationship r : rels) {
            String type = r.getRelationshipType();
            if (type == null) continue;
            String t = type.trim().toLowerCase();
            if ("parent".equals(t)) {
                UUID parent = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID child = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (parent != null && child != null && allIds.contains(parent) && allIds.contains(child)) {
                    graph.get(parent).add(child);
                    graph.get(child).add(parent);
                    hasParent.add(child);
                }
            } else if ("child".equals(t)) {
                UUID child = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID parent = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (parent != null && child != null && allIds.contains(parent) && allIds.contains(child)) {
                    graph.get(parent).add(child);
                    graph.get(child).add(parent);
                    hasParent.add(child);
                }
            }
        }

        // Global roots: nodes without any parent in the current tree
        java.util.List<UUID> roots = persons.stream()
                .map(Person::getId)
                .filter(id -> !hasParent.contains(id))
                .toList();
        if (roots.isEmpty()) return;

        java.util.Set<UUID> reachable = new java.util.HashSet<>();
        java.util.ArrayDeque<UUID> dq = new java.util.ArrayDeque<>();
        for (UUID r : roots) { if (reachable.add(r)) dq.add(r); }
        while (!dq.isEmpty()) {
            UUID u = dq.pollFirst();
            for (UUID v : graph.getOrDefault(u, java.util.Collections.emptySet())) {
                if (reachable.add(v)) dq.addLast(v);
            }
        }

        java.util.List<UUID> toDelete = persons.stream()
                .map(Person::getId)
                .filter(id -> !reachable.contains(id))
                .toList();

        if (!toDelete.isEmpty()) {
            for (UUID id : toDelete) {
                // Remove their relationships first to avoid FK issues
                java.util.List<Relationship> links = relationshipRepository.findByPerson1IdOrPerson2Id(id, id);
                if (!links.isEmpty()) relationshipRepository.deleteAll(links);
                personRepository.findById(id).ifPresent(personRepository::delete);
            }
        }
    }

    // ==================== SHARING LOGIC ====================

    @Transactional
    public TreeShareResponse generatePublicShareLink(UUID treeId, UUID userId, String permission) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);

        if (!permission.equals("view") && !permission.equals("edit")) {
            throw new AppException(ErrorCode.VALIDATION_FAILED);
        }

        tree.setIsPublic(true);
        tree.setSharePermission(permission);
        familyTreeRepository.save(tree);

        long sharedCount = treeAccessRepository.countByFamilyTreeId(treeId);
        String publicUrl = frontendUrl + "/trees/shared/" + tree.getShareToken();

        return TreeShareResponse.builder()
                .treeId(tree.getId())
                .treeName(tree.getName())
                .shareToken(tree.getShareToken())
                .shareUrl(publicUrl)
                .publicShareUrl(publicUrl)
                .sharedWithCount((int) sharedCount)
                .sharePermission(tree.getSharePermission())
                .build();
    }

    @Transactional
    public TreeAccess shareWithUser(UUID treeId, UUID ownerId, String targetEmail, String accessLevel) {
        log.info("START: shareWithUser - treeId: {}, ownerId: {}, targetEmail: {}, accessLevel: {}",
                treeId, ownerId, targetEmail, accessLevel);

        FamilyTree tree = findOwnedTreeOrThrow(treeId, ownerId);
        User owner = loadUserOrThrow(ownerId);
        User targetUser = userRepository.findByEmail(targetEmail)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        if (targetUser.getId().equals(ownerId)) {
            throw new AppException(ErrorCode.CANNOT_SHARE_TO_SELF);
        }

        Optional<TreeAccess> existing = treeAccessRepository.findByUserIdAndFamilyTreeId(
                targetUser.getId(), treeId);

        TreeAccess access;
        if (existing.isPresent()) {
            access = existing.get();
            access.setAccessLevel(accessLevel);
            access = treeAccessRepository.save(access);
            log.info("UPDATED: Existing TreeAccess updated for user: {}", targetEmail);
        } else {
            access = TreeAccess.builder()
                    .userId(targetUser.getId())
                    .familyTreeId(tree.getId())
                    .accessLevel(accessLevel)
                    .grantedBy(owner)
                    .build();
            access = treeAccessRepository.save(access);
            log.info("CREATED: New TreeAccess created for user: {}", targetEmail);
        }

        try {
            String shareUrl = frontendUrl + "/trees/shared/" + tree.getShareToken();
            emailService.sendTreeShareNotification(
                    targetEmail,
                    targetUser.getUsername(),
                    tree.getName(),
                    owner.getUsername(),
                    accessLevel,
                    shareUrl
            );
            log.info("SUCCESS: Sent share notification email to {}", targetEmail);
        } catch (Exception e) {
            log.error("FAILED: Error sending email to {}: {}", targetEmail, e.getMessage(), e);
        }

        log.info("COMPLETED: shareWithUser finished for user: {}", targetEmail);
        return access;
    }

    @Transactional(readOnly = true)
    public List<TreeAccess> getSharedUsers(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        return treeAccessRepository.findAllByFamilyTreeIdWithUsers(tree.getId());
    }

    @Transactional
    public void revokeAccess(UUID treeId, UUID ownerId, UUID targetUserId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, ownerId);
        treeAccessRepository.deleteByUserIdAndFamilyTreeId(targetUserId, tree.getId());
    }

    @Transactional
    public void disablePublicSharing(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        tree.setIsPublic(false);
        familyTreeRepository.save(tree);
    }

    @Transactional(readOnly = true)
    public FamilyTree getTreeByShareToken(UUID shareToken) {
        FamilyTree tree = familyTreeRepository.findByShareToken(shareToken)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        if (tree.getCreatedBy() != null) {
            tree.getCreatedBy().getEmail();
        }
        return tree;
    }

    @Transactional(readOnly = true)
    public boolean canEdit(UUID treeId, UUID userId) {
        Optional<FamilyTree> ownedTree = familyTreeRepository.findByIdAndCreatedBy(
                treeId, loadUserOrThrow(userId));
        if (ownedTree.isPresent()) {
            return true;
        }

        Optional<FamilyTree> tree = familyTreeRepository.findById(treeId);
        if (tree.isPresent() && Boolean.TRUE.equals(tree.get().getIsPublic())
                && "edit".equals(tree.get().getSharePermission())) {
            return true;
        }

        Optional<TreeAccess> access = treeAccessRepository.findByUserIdAndFamilyTreeId(userId, treeId);
        return access.isPresent() &&
                ("edit".equals(access.get().getAccessLevel()) || "admin".equals(access.get().getAccessLevel()));
    }

    @Transactional(readOnly = true)
    public boolean canView(UUID treeId, UUID userId) {
        Optional<FamilyTree> ownedTree = familyTreeRepository.findByIdAndCreatedBy(
                treeId, loadUserOrThrow(userId));
        if (ownedTree.isPresent()) {
            return true;
        }

        Optional<TreeAccess> access = treeAccessRepository.findByUserIdAndFamilyTreeId(userId, treeId);
        return access.isPresent();
    }

    @Transactional(readOnly = true)
    public FamilyTree getSharedTree(UUID shareToken, UUID userId) {
        FamilyTree tree = getTreeByShareToken(shareToken);

        if (tree.getCreatedBy() != null) {
            tree.getCreatedBy().getId();
        }

        if (Boolean.TRUE.equals(tree.getIsPublic())) {
            return tree;
        }

        if (userId == null) {
            throw new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND);
        }

        if (tree.getCreatedBy().getId().equals(userId) || canView(tree.getId(), userId)) {
            return tree;
        }

        throw new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND);
    }

    // ==================== MỚI: THÔNG TIN ACCESS QUA SHARE TOKEN ====================

    /**
     * Lấy thông tin access từ shareToken
     * Dùng cho frontend khi mở link shared để biết:
     * - Có phải owner không?
     * - Có quyền edit không?
     * - Vai trò hiện tại (OWNER / EDITOR / VIEWER)
     */
    @Transactional(readOnly = true)
    public SharedTreeAccessInfoResponse getSharedTreeAccessInfo(UUID shareToken, UUID userId) {
        FamilyTree tree = getTreeByShareToken(shareToken);

        boolean canEdit = false;
        boolean canView = false;
        String role = "VIEWER";

        if (userId != null) {
            // Kiểm tra owner
            if (tree.getCreatedBy().getId().equals(userId)) {
                canEdit = true;
                canView = true;
                role = "OWNER";
            } else {
                // Kiểm tra tree_access (chia sẻ cá nhân)
                Optional<TreeAccess> access = treeAccessRepository
                        .findByUserIdAndFamilyTreeId(userId, tree.getId());

                if (access.isPresent()) {
                    String accessLevel = access.get().getAccessLevel();
                    canView = true;

                    if ("edit".equals(accessLevel) || "admin".equals(accessLevel)) {
                        canEdit = true;
                        role = "EDITOR";
                    }
                } else if (Boolean.TRUE.equals(tree.getIsPublic())) {
                    // Public link
                    canView = true;
                    if ("edit".equals(tree.getSharePermission())) {
                        canEdit = true;
                        role = "EDITOR";
                    }
                }
            }
        } else {
            // Chưa đăng nhập → chỉ có thể xem nếu public
            if (Boolean.TRUE.equals(tree.getIsPublic())) {
                canView = true;
                if ("edit".equals(tree.getSharePermission())) {
                    canEdit = true;
                }
            }
        }

        return SharedTreeAccessInfoResponse.builder()
                .treeId(tree.getId())
                .treeName(tree.getName())
                .canEdit(canEdit)
                .canView(canView)
                .role(role)
                .build();
    }

    // ==================== MỚI: LẤY CẢ CÂY SỞ HỮU VÀ CÂY ĐƯỢC CHIA SẺ ====================

    /**
     * MỚI: Lấy tất cả cây gia phả của user (bao gồm cả cây sở hữu và cây được chia sẻ)
     * Sử dụng JPQL query hiệu quả thay vì merge thủ công
     */
    @Transactional(readOnly = true)
    public List<FamilyTree> getUserTrees(UUID userId) {
        log.info("Getting all accessible trees for user: {}", userId);

        List<FamilyTree> allTrees = familyTreeRepository.findAllAccessibleByUser(userId);

        log.info("Total accessible trees for user {}: {}", userId, allTrees.size());
        return allTrees;
    }

    // ==================== EDIT ACCESS REQUEST METHODS ====================

    @Transactional
    public void requestEditAccess(UUID treeId, UUID requesterId) {
        FamilyTree tree = familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        User requester = userRepository.findById(requesterId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        User owner = tree.getCreatedBy();
        if (owner == null) {
            throw new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND);
        }

        Optional<TreeAccess> existingAccess = treeAccessRepository
                .findByUserIdAndFamilyTreeId(requesterId, treeId);

        if (existingAccess.isPresent() && "edit".equals(existingAccess.get().getAccessLevel())) {
            throw new AppException(ErrorCode.ALREADY_HAS_EDIT_ACCESS);
        }

        // ✅ Tạo JsonNode thay vì String
        ObjectNode relatedEntityJson = objectMapper.createObjectNode();
        relatedEntityJson.put("treeId", treeId.toString());
        relatedEntityJson.put("requesterId", requesterId.toString());

        Notification notification = Notification.builder()
                .user(owner)
                .type(Notification.NotificationType.edit_request)
                .title("Yêu cầu quyền chỉnh sửa")
                .message(String.format(
                        "%s (%s) yêu cầu quyền chỉnh sửa cây gia phả '%s'",
                        requester.getUserProfile() != null ? requester.getUserProfile().getFullName() : requester.getUsername(),
                        requester.getEmail(),
                        tree.getName()
                ))
                .relatedEntity(relatedEntityJson)  // ← Dùng JsonNode
                .isRead(false)
                .createdAt(OffsetDateTime.now(ZoneOffset.UTC))
                .build();

        Notification savedNotification = notificationRepository.save(notification);
        
        // Gửi SSE notification đến owner
        notificationService.sendNotificationToUser(owner.getId(), mapNotificationToResponse(savedNotification));

        log.info("Edit request sent: Tree={}, Requester={}, Owner={}",
                treeId, requesterId, owner.getId());
    }
    
    @Transactional
    public void approveEditRequest(UUID treeId, UUID ownerId, UUID requesterId) {
        FamilyTree tree = findEditableTreeOrThrow(treeId, ownerId);
        User requester = loadUserOrThrow(requesterId);

        // Cập nhật hoặc tạo TreeAccess với quyền edit
        Optional<TreeAccess> existing = treeAccessRepository
                .findByUserIdAndFamilyTreeId(requesterId, treeId);

        TreeAccess access;
        if (existing.isPresent()) {
            access = existing.get();
            access.setAccessLevel("edit");
        } else {
            access = TreeAccess.builder()
                    .userId(requesterId)
                    .familyTreeId(treeId)
                    .accessLevel("edit")
                    .grantedBy(loadUserOrThrow(ownerId))
                    .build();
        }

        treeAccessRepository.save(access);

        // Gửi notification cho requester
        NotificationCreateRequest notifRequest = new NotificationCreateRequest();
        notifRequest.setTitle("Yêu cầu được chấp nhận");
        notifRequest.setMessage(String.format(
                "Bạn đã được cấp quyền chỉnh sửa cây gia phả \"%s\"",
                tree.getName()
        ));
        notifRequest.setType(Notification.NotificationType.access_granted);
        notifRequest.setRelatedEntity(Map.of(
                "type", "edit_granted",
                "treeId", treeId.toString()
        ));

        notificationService.createNotification(requesterId, notifRequest);

        log.info("Edit access approved: Tree={}, Requester={}", treeId, requesterId);
    }

    @Transactional
    public void rejectEditRequest(UUID treeId, UUID ownerId, UUID requesterId) {
        FamilyTree tree = findEditableTreeOrThrow(treeId, ownerId);

        // Gửi notification từ chối
        NotificationCreateRequest notifRequest = new NotificationCreateRequest();
        notifRequest.setTitle("Yêu cầu bị từ chối");
        notifRequest.setMessage(String.format(
                "Yêu cầu chỉnh sửa cây gia phả \"%s\" đã bị từ chối",
                tree.getName()
        ));
        notifRequest.setType(Notification.NotificationType.system);

        notificationService.createNotification(requesterId, notifRequest);

        log.info("Edit access rejected: Tree={}, Requester={}", treeId, requesterId);
    }

    private NotificationResponse mapNotificationToResponse(Notification notification) {
        NotificationResponse response = new NotificationResponse();
        response.setId(notification.getId());
        response.setUserId(notification.getUser().getId());
        response.setTitle(notification.getTitle());
        response.setMessage(notification.getMessage());
        response.setType(notification.getType());
        response.setIsRead(notification.getIsRead());

        OffsetDateTime createdAt = notification.getCreatedAt() != null
                ? notification.getCreatedAt()
                : OffsetDateTime.now(ZoneOffset.UTC);

        ZoneId vnZone = ZoneId.of("Asia/Ho_Chi_Minh");
        response.setCreatedAt(createdAt.atZoneSameInstant(vnZone).toOffsetDateTime());

        if (notification.getRelatedEntity() != null) {
            response.setRelatedEntity(objectMapper.convertValue(
                    notification.getRelatedEntity(),
                    objectMapper.getTypeFactory().constructMapType(Map.class, String.class, Object.class)
            ));
        }

        return response;
    }

    @Transactional
    public void pruneDisconnectedBloodlineAutoRoot(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        List<Person> persons = personRepository.findAllByFamilyTree_Id(tree.getId());
        if (persons.isEmpty()) return;

        List<Relationship> rels = relationshipRepository.findByFamilyTreeId(tree.getId());

        Map<UUID, Set<UUID>> graph = new java.util.HashMap<>();
        Set<UUID> allIds = new java.util.HashSet<>();
        for (Person person : persons) {
            allIds.add(person.getId());
            graph.put(person.getId(), new java.util.HashSet<>());
        }

        Set<UUID> hasParent = new java.util.HashSet<>();

        for (Relationship r : rels) {
            String type = r.getRelationshipType();
            if (type == null) continue;
            String t = type.trim().toLowerCase();
            if ("parent".equals(t)) {
                UUID parent = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID child = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (parent != null && child != null && allIds.contains(parent) && allIds.contains(child)) {
                    graph.get(parent).add(child);
                    graph.get(child).add(parent);
                    hasParent.add(child);
                }
            } else if ("child".equals(t)) {
                UUID child = r.getPerson1() != null ? r.getPerson1().getId() : null;
                UUID parent = r.getPerson2() != null ? r.getPerson2().getId() : null;
                if (parent != null && child != null && allIds.contains(parent) && allIds.contains(child)) {
                    graph.get(parent).add(child);
                    graph.get(child).add(parent);
                    hasParent.add(child);
                }
            }
        }

        List<UUID> roots = persons.stream()
                .map(Person::getId)
                .filter(id -> !hasParent.contains(id))
                .collect(Collectors.toList());
        if (roots.isEmpty()) return;

        Set<UUID> reachable = new java.util.HashSet<>();
        java.util.ArrayDeque<UUID> dq = new java.util.ArrayDeque<>();
        for (UUID r : roots) {
            if (reachable.add(r)) dq.add(r);
        }
        while (!dq.isEmpty()) {
            UUID cur = dq.pollFirst();
            for (UUID nb : graph.getOrDefault(cur, java.util.Collections.emptySet())) {
                if (reachable.add(nb)) dq.addLast(nb);
            }
        }

        List<UUID> toDelete = persons.stream()
                .map(Person::getId)
                .filter(id -> !reachable.contains(id))
                .collect(Collectors.toList());

        if (toDelete.isEmpty()) return;

        for (UUID pid : toDelete) {
            List<Relationship> links = relationshipRepository.findByPerson1IdOrPerson2Id(pid, pid);
            if (!links.isEmpty()) {
                relationshipRepository.deleteAll(links);
            }
            personRepository.findById(pid).ifPresent(personRepository::delete);
        }
    }
}
