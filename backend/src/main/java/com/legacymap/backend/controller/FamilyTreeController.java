package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.dto.request.PersonUpdateRequest;
import com.legacymap.backend.dto.request.RelationshipCreateRequest;
import com.legacymap.backend.dto.request.TreeShareRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.FamilyTreeResponse;
import com.legacymap.backend.dto.response.PersonResponse;
import com.legacymap.backend.dto.response.RelationshipDTO;
import com.legacymap.backend.dto.response.RelationshipSuggestion;
import com.legacymap.backend.dto.response.SharedTreeAccessInfoResponse;
import com.legacymap.backend.dto.response.TreeAccessResponse;
import com.legacymap.backend.dto.response.TreeShareResponse;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.entity.TreeAccess;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.TreeAccessRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.FamilyTreeService;
import com.legacymap.backend.service.RelationshipService;
import com.legacymap.backend.service.RelationshipSuggestionService;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@RestController
@RequestMapping("/api/trees")
public class FamilyTreeController {

    @Autowired
    private FamilyTreeService familyTreeService;

    @Autowired
    private RelationshipService relationshipService;

    @Autowired
    private RelationshipSuggestionService relationshipSuggestionService;

    @Autowired
    private TreeAccessRepository treeAccessRepository;

    @Autowired
    private FamilyTreeRepository familyTreeRepository;

    @Autowired
    private UserRepository userRepository;

    private UUID parseUserId(String userId) {
        try {
            return UUID.fromString(userId);
        } catch (Exception e) {
            throw new AppException(ErrorCode.VALIDATION_FAILED);
        }
    }

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    // ==================== TREE MANAGEMENT ENDPOINTS ====================

    @PostMapping
    public ResponseEntity<ApiResponse<FamilyTreeResponse>> createTree(
            @RequestParam("userId") String userId,
            @RequestBody @Valid FamilyTreeCreateRequest req) {
        FamilyTree tree = familyTreeService.create(parseUserId(userId), req);
        FamilyTreeResponse response = FamilyTreeResponse.fromEntity(tree);
        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<FamilyTreeResponse>>> listTrees(
            @RequestParam("userId") String userId) {
        List<FamilyTree> trees = familyTreeService.listByUser(parseUserId(userId));
        List<FamilyTreeResponse> responses = trees.stream()
                .map(FamilyTreeResponse::fromEntity)
                .toList();
        return ResponseEntity.ok(ApiResponse.success(responses));
    }

    @PutMapping("/{treeId}")
    public ResponseEntity<ApiResponse<FamilyTreeResponse>> updateTree(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody FamilyTreeUpdateRequest req) {
        FamilyTree tree = familyTreeService.update(treeId, parseUserId(userId), req);
        FamilyTreeResponse response = FamilyTreeResponse.fromEntity(tree);
        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @DeleteMapping("/{treeId}")
    public ResponseEntity<ApiResponse<Void>> deleteTree(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        familyTreeService.delete(treeId, parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success());
    }

    // ==================== MEMBER MANAGEMENT ENDPOINTS ====================

    @PostMapping("/{treeId}/members")
    public ResponseEntity<ApiResponse<PersonResponse>> addMember(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody @Valid PersonCreateRequest req) {
        Person person = familyTreeService.addMember(treeId, parseUserId(userId), req);
        PersonResponse response = toPersonResponse(person);
        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @GetMapping("/{treeId}/members")
    public ResponseEntity<ApiResponse<List<PersonResponse>>> listMembers(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        List<Person> people = familyTreeService.listMembers(treeId, parseUserId(userId));
        List<PersonResponse> responses = people.stream()
                .map(this::toPersonResponse)
                .collect(Collectors.toList());
        return ResponseEntity.ok(ApiResponse.success(responses));
    }

    @GetMapping("/{treeId}/viewer/members")
    @Transactional(readOnly = true)
    public ResponseEntity<ApiResponse<List<Map<String, Object>>>> listMembersForViewer(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        List<Person> people = familyTreeService.listMembersForViewer(treeId, parseUserId(userId));
        List<Map<String, Object>> out = new ArrayList<>();
        for (Person p : people) {
            Map<String, Object> m = new HashMap<>();
            m.put("id", p.getId());
            m.put("fullName", p.getFullName());
            m.put("gender", p.getGender());
            m.put("birthDate", p.getBirthDate());
            m.put("deathDate", p.getDeathDate());
            m.put("birthPlace", p.getBirthPlace());
            m.put("deathPlace", p.getDeathPlace());
            m.put("biography", p.getBiography());
            m.put("avatarUrl", p.getAvatarUrl());
            m.put("phone", p.getPhone());
            m.put("email", p.getEmail());
            out.add(m);
        }
        return ResponseEntity.ok(ApiResponse.success(out));
    }

    @PutMapping("/{treeId}/members/{personId}")
    public ResponseEntity<ApiResponse<PersonResponse>> updateMember(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId,
            @RequestBody PersonUpdateRequest req) {
        Person person = familyTreeService.updateMember(treeId, parseUserId(userId), personId, req);
        PersonResponse response = toPersonResponse(person);
        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @DeleteMapping("/{treeId}/members/{personId}")
    public ResponseEntity<ApiResponse<Void>> deleteMember(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId) {
        familyTreeService.deleteMember(treeId, parseUserId(userId), personId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @DeleteMapping("/{treeId}/members/{personId}/safe")
    public ResponseEntity<ApiResponse<Void>> deleteMemberSafe(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId) {
        familyTreeService.deleteMemberSafe(treeId, parseUserId(userId), personId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    // ==================== RELATIONSHIP MANAGEMENT ENDPOINTS ====================

    @GetMapping("/{treeId}/relationships")
    public ResponseEntity<ApiResponse<List<RelationshipDTO>>> listRelationships(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        List<RelationshipDTO> rels = relationshipService.listByTree(treeId, parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success(rels));
    }

    @GetMapping("/{treeId}/viewer/relationships")
    public ResponseEntity<ApiResponse<List<RelationshipDTO>>> listRelationshipsForViewer(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId
    ) {
        UUID uid = parseUserId(userId);
        if (!familyTreeService.hasViewerAccess(treeId, uid)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }
        List<RelationshipDTO> rels = relationshipService.listByTree(treeId, uid);
        return ResponseEntity.ok(ApiResponse.success(rels));
    }

    @GetMapping("/{treeId}/persons/{personId}/relationships")
    public ResponseEntity<ApiResponse<List<RelationshipDTO>>> listPersonRelationships(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId) {
        List<RelationshipDTO> rels = relationshipService.listByPerson(treeId, parseUserId(userId), personId);
        return ResponseEntity.ok(ApiResponse.success(rels));
    }

    @PostMapping("/{treeId}/relationships")
    public ResponseEntity<ApiResponse<RelationshipDTO>> createRelationship(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody @Valid RelationshipCreateRequest req) {
        Relationship rel = relationshipService.create(treeId, parseUserId(userId), req);
        return ResponseEntity.ok(ApiResponse.success(RelationshipDTO.fromEntity(rel)));
    }

    @GetMapping("/{treeId}/relationships/suggest")
    public ResponseEntity<ApiResponse<List<RelationshipSuggestion>>> suggestRelationship(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestParam("person1Id") UUID person1Id,
            @RequestParam("person2Id") UUID person2Id) {
        List<RelationshipSuggestion> out = relationshipSuggestionService.suggest(treeId, parseUserId(userId), person1Id, person2Id);
        return ResponseEntity.ok(ApiResponse.success(out));
    }

    @PostMapping("/{treeId}/relationships/suggest/source")
    public ResponseEntity<ApiResponse<List<com.legacymap.backend.dto.response.PairSuggestionResponse>>> suggestForSource(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody com.legacymap.backend.dto.request.PairSuggestRequest body
    ) {
        java.util.Map<java.util.UUID, com.legacymap.backend.dto.response.RelationshipSuggestion> map =
                relationshipSuggestionService.suggestForSource(
                        treeId,
                        parseUserId(userId),
                        body.getSourceId(),
                        body.getCandidateIds()
                );
        java.util.List<com.legacymap.backend.dto.response.PairSuggestionResponse> list = new java.util.ArrayList<>();
        for (java.util.Map.Entry<java.util.UUID, com.legacymap.backend.dto.response.RelationshipSuggestion> e : map.entrySet()) {
            com.legacymap.backend.dto.response.RelationshipSuggestion rs = e.getValue();
            list.add(new com.legacymap.backend.dto.response.PairSuggestionResponse(
                    e.getKey(), rs.getType(), rs.getConfidence(), rs.getReasons()
            ));
        }
        return ResponseEntity.ok(ApiResponse.success(list));
    }

    @DeleteMapping("/{treeId}/relationships/{relationshipId}")
    public ResponseEntity<ApiResponse<Void>> deleteRelationship(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("relationshipId") UUID relationshipId,
            @RequestParam("userId") String userId) {
        relationshipService.delete(treeId, parseUserId(userId), relationshipId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @PostMapping("/{treeId}/maintenance/prune")
    public ResponseEntity<ApiResponse<Void>> pruneDisconnected(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        familyTreeService.pruneDisconnectedBloodlineAutoRoot(treeId, parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success());
    }

    // ==================== SHARE ENDPOINTS ====================

    @PostMapping("/{treeId}/share/public")
    public ResponseEntity<ApiResponse<TreeShareResponse>> generatePublicLink(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestParam(value = "permission", defaultValue = "view") String permission) {

        TreeShareResponse response = familyTreeService.generatePublicShareLink(
                treeId, parseUserId(userId), permission);
        return ResponseEntity.ok(ApiResponse.success(response, "Public share link created"));
    }

    @DeleteMapping("/{treeId}/share/public")
    public ResponseEntity<ApiResponse<Void>> disablePublicSharing(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {

        familyTreeService.disablePublicSharing(treeId, parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success());
    }

    @PostMapping("/{treeId}/share/user")
    public ResponseEntity<ApiResponse<TreeAccessResponse>> shareWithUser(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody @Valid TreeShareRequest req) {

        TreeAccess access = familyTreeService.shareWithUser(
                treeId,
                parseUserId(userId),
                req.getEmail(),
                req.getAccessLevel()
        );

        TreeAccessResponse response = TreeAccessResponse.fromEntity(access);
        return ResponseEntity.ok(ApiResponse.success(response, "Tree shared successfully"));
    }

    @GetMapping("/{treeId}/share/users")
    public ResponseEntity<ApiResponse<List<TreeAccessResponse>>> getSharedUsers(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {

        List<TreeAccess> accesses = familyTreeService.getSharedUsers(treeId, parseUserId(userId));
        List<TreeAccessResponse> responses = accesses.stream()
                .map(TreeAccessResponse::fromEntity)
                .toList();

        return ResponseEntity.ok(ApiResponse.success(responses));
    }

    @DeleteMapping("/{treeId}/share/users/{targetUserId}")
    public ResponseEntity<ApiResponse<Void>> revokeAccess(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("targetUserId") UUID targetUserId,
            @RequestParam("userId") String userId) {

        familyTreeService.revokeAccess(treeId, parseUserId(userId), targetUserId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @PostMapping("/{treeId}/save")
    public ResponseEntity<ApiResponse<String>> saveSharedTree(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {

        UUID parsedUserId = parseUserId(userId);

        FamilyTree tree = familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        if (tree.getCreatedBy().getId().equals(parsedUserId)) {
            log.info("User {} is already the owner of tree {}", parsedUserId, treeId);
            return ResponseEntity.ok(ApiResponse.success("You are already the owner of this tree"));
        }

        Optional<TreeAccess> existing = treeAccessRepository
                .findByUserIdAndFamilyTreeId(parsedUserId, treeId);

        if (existing.isPresent()) {
            log.info("Tree {} already saved to user {} dashboard", treeId, parsedUserId);
            return ResponseEntity.ok(ApiResponse.success("Tree already in your dashboard"));
        }

        String accessLevel = tree.getSharePermission() != null && "edit".equals(tree.getSharePermission())
                ? "edit"
                : "view";

        TreeAccess access = TreeAccess.builder()
                .userId(parsedUserId)
                .familyTreeId(treeId)
                .accessLevel(accessLevel)
                .grantedBy(tree.getCreatedBy())
                .build();

        treeAccessRepository.save(access);

        log.info("Tree saved to dashboard: Tree ID={}, User ID={}, Access Level={}",
                treeId, parsedUserId, accessLevel);

        return ResponseEntity.ok(ApiResponse.success("Tree saved to dashboard successfully"));
    }

    @GetMapping("/{treeId}/access")
    public ResponseEntity<ApiResponse<Map<String, String>>> checkAccess(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {

        UUID parsedUserId = parseUserId(userId);

        Optional<FamilyTree> ownedTree = familyTreeRepository.findByIdAndCreatedBy(
                treeId, loadUserOrThrow(parsedUserId));

        if (ownedTree.isPresent()) {
            return ResponseEntity.ok(ApiResponse.success(
                    Map.of("accessLevel", "admin")
            ));
        }

        Optional<TreeAccess> access = treeAccessRepository
                .findByUserIdAndFamilyTreeId(parsedUserId, treeId);

        if (access.isPresent()) {
            return ResponseEntity.ok(ApiResponse.success(
                    Map.of("accessLevel", access.get().getAccessLevel())
            ));
        }

        return ResponseEntity.ok(ApiResponse.success(
                Map.of("accessLevel", "none")
        ));
    }

    /**
     * API mới: Lấy thông tin access từ shareToken
     * - Dùng để frontend biết: có public không? có quyền edit không? owner là ai? v.v.
     */
    @GetMapping("/shared/{shareToken}/access-info")
    public ResponseEntity<ApiResponse<SharedTreeAccessInfoResponse>> getSharedTreeAccessInfo(
            @PathVariable("shareToken") UUID shareToken,
            @RequestParam(value = "userId", required = false) String userId) {

        UUID parsedUserId = userId != null ? parseUserId(userId) : null;

        SharedTreeAccessInfoResponse info = familyTreeService.getSharedTreeAccessInfo(
                shareToken, parsedUserId);

        return ResponseEntity.ok(ApiResponse.success(info));
    }

    // ==================== PUBLIC SHARE ENDPOINTS ====================

    @Transactional(readOnly = true)
    @GetMapping("/shared/{shareToken}")
    public ResponseEntity<ApiResponse<FamilyTreeResponse>> getSharedTree(
            @PathVariable("shareToken") UUID shareToken,
            @RequestParam(value = "userId", required = false) String userId) {

        UUID parsedUserId = userId != null ? parseUserId(userId) : null;
        FamilyTree tree = familyTreeService.getSharedTree(shareToken, parsedUserId);

        FamilyTreeResponse response = FamilyTreeResponse.fromEntity(tree);

        log.info("Shared Tree Response: id={}, name={}, permission={}",
                response.getId(), response.getName(), response.getSharePermission());

        if (response.getId() == null) {
            log.error("CRITICAL: Response missing ID for tree: {}", tree.getId());
            throw new AppException(ErrorCode.INTERNAL_SERVER_ERROR);
        }

        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @GetMapping("/shared/{shareToken}/members")
    public ResponseEntity<ApiResponse<List<PersonResponse>>> getSharedTreeMembers(
            @PathVariable("shareToken") UUID shareToken,
            @RequestParam(value = "userId", required = false) String userId) {

        UUID parsedUserId = userId != null ? parseUserId(userId) : null;
        FamilyTree tree = familyTreeService.getSharedTree(shareToken, parsedUserId);

        List<Person> members = familyTreeService.listMembers(tree.getId(), tree.getCreatedBy().getId());

        // ✅ Convert Entity sang DTO
        List<PersonResponse> response = members.stream()
                .map(this::toPersonResponse)
                .collect(Collectors.toList());

        log.info("Shared Tree Members: Tree ID={}, Members count={}",
                tree.getId(), response.size());

        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @PostMapping("/shared/{shareToken}/members")
    public ResponseEntity<ApiResponse<PersonResponse>> addSharedTreeMember(
            @PathVariable("shareToken") UUID shareToken,
            @RequestParam("userId") String userId,
            @RequestBody @Valid PersonCreateRequest req) {

        UUID parsedUserId = parseUserId(userId);
        FamilyTree tree = familyTreeService.getTreeByShareToken(shareToken);

        if (!familyTreeService.canEdit(tree.getId(), parsedUserId)) {
            throw new AppException(ErrorCode.PERMISSION_DENIED);
        }

        Person person = familyTreeService.addMember(tree.getId(), parsedUserId, req);
        PersonResponse response = toPersonResponse(person);
        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @PutMapping("/shared/{shareToken}/members/{personId}")
    public ResponseEntity<ApiResponse<PersonResponse>> updateSharedTreeMember(
            @PathVariable("shareToken") UUID shareToken,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId,
            @RequestBody PersonUpdateRequest req) {

        UUID parsedUserId = parseUserId(userId);
        FamilyTree tree = familyTreeService.getTreeByShareToken(shareToken);

        if (!familyTreeService.canEdit(tree.getId(), parsedUserId)) {
            throw new AppException(ErrorCode.PERMISSION_DENIED);
        }

        Person person = familyTreeService.updateMember(tree.getId(), tree.getCreatedBy().getId(), personId, req);
        PersonResponse response = toPersonResponse(person);
        return ResponseEntity.ok(ApiResponse.success(response));
    }

    @GetMapping("/shared/{shareToken}/relationships")
    public ResponseEntity<ApiResponse<List<RelationshipDTO>>> getSharedTreeRelationships(
            @PathVariable("shareToken") UUID shareToken,
            @RequestParam(value = "userId", required = false) String userId) {

        UUID parsedUserId = userId != null ? parseUserId(userId) : null;

        FamilyTree tree = familyTreeService.getSharedTree(shareToken, parsedUserId);

        List<RelationshipDTO> rels = relationshipService.listByTree(
                tree.getId(),
                tree.getCreatedBy().getId()
        );

        log.info("Shared Tree Relationships: Tree ID={}, Relationships count={}",
                tree.getId(), rels.size());

        return ResponseEntity.ok(ApiResponse.success(rels));
    }

    // ==================== HELPER METHODS ====================

    private PersonResponse toPersonResponse(Person person) {
        return PersonResponse.builder()
                .id(person.getId())
                .fullName(person.getFullName())
                .gender(person.getGender())
                .birthDate(person.getBirthDate())
                .birthPlace(person.getBirthPlace())
                .deathDate(person.getDeathDate())
                .deathPlace(person.getDeathPlace())
                .email(person.getEmail())
                .phone(person.getPhone())
                .avatarUrl(person.getAvatarUrl())
                .biography(person.getBiography())
                .familyTreeId(person.getFamilyTree() != null ? person.getFamilyTree().getId() : null)
                .build();
    }
}