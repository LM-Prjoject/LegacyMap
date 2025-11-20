package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.dto.request.PersonUpdateRequest;
import com.legacymap.backend.dto.request.RelationshipCreateRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.RelationshipDTO;
import com.legacymap.backend.dto.response.RelationshipSuggestion;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.service.FamilyTreeService;
import com.legacymap.backend.service.RelationshipService;
import com.legacymap.backend.service.RelationshipSuggestionService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.UUID;

@RestController
@RequestMapping("/api/trees")
public class FamilyTreeController {

    @Autowired
    private FamilyTreeService familyTreeService;

    @Autowired
    private RelationshipService relationshipService;

    @Autowired
    private RelationshipSuggestionService relationshipSuggestionService;

    private UUID parseUserId(String userId) {
        try {
            return UUID.fromString(userId);
        } catch (Exception e) {
            throw new AppException(ErrorCode.VALIDATION_FAILED);
        }
    }

    @PostMapping
    public ResponseEntity<ApiResponse<FamilyTree>> createTree(
            @RequestParam("userId") String userId,
            @RequestBody @Valid FamilyTreeCreateRequest req) {
        FamilyTree tree = familyTreeService.create(parseUserId(userId), req);
        return ResponseEntity.ok(ApiResponse.success(tree));
    }

    @GetMapping
    @Transactional(readOnly = true)
    public ResponseEntity<ApiResponse<List<Map<String, Object>>>> listTrees(
            @RequestParam("userId") String userId) {
        List<FamilyTree> trees = familyTreeService.listByUser(parseUserId(userId));
        List<Map<String, Object>> out = new ArrayList<>();
        for (FamilyTree t : trees) {
            Map<String, Object> m = new HashMap<>();
            m.put("id", t.getId());
            m.put("name", t.getName());
            m.put("description", t.getDescription());
            m.put("isPublic", t.getIsPublic() != null ? t.getIsPublic() : Boolean.FALSE);
            m.put("coverImageUrl", t.getCoverImageUrl());
            m.put("createdAt", t.getCreatedAt());
            m.put("updatedAt", t.getUpdatedAt());
            // expose owner id explicitly
            UUID ownerId = familyTreeService.getOwnerId(t.getId());
            m.put("createdById", ownerId);
            out.add(m);
        }
        return ResponseEntity.ok(ApiResponse.success(out));
    }

    @GetMapping("/viewable")
    @Transactional(readOnly = true)
    public ResponseEntity<ApiResponse<List<Map<String, Object>>>> listViewableTrees(
            @RequestParam("userId") String userId
    ) {
        List<FamilyTree> trees = familyTreeService.listViewableTrees(parseUserId(userId));
        // Build plain maps to avoid touching any lazy proxies during JSON serialization
        List<Map<String, Object>> out = new ArrayList<>();
        for (FamilyTree t : trees) {
            Map<String, Object> m = new HashMap<>();
            m.put("id", t.getId());
            m.put("name", t.getName());
            m.put("description", t.getDescription());
            m.put("isPublic", t.getIsPublic() != null ? t.getIsPublic() : Boolean.FALSE);
            m.put("coverImageUrl", t.getCoverImageUrl());
            m.put("createdAt", t.getCreatedAt());
            m.put("updatedAt", t.getUpdatedAt());
            m.put("createdBy", null); // createdBy is @JsonIgnore; expose id if needed
            out.add(m);
        }
        return ResponseEntity.ok(ApiResponse.success(out));
    }

    @GetMapping("/{treeId}/owner")
    @Transactional(readOnly = true)
    public ResponseEntity<ApiResponse<Map<String, Object>>> getTreeOwner(
            @PathVariable("treeId") UUID treeId
    ) {
        UUID ownerId = familyTreeService.getOwnerId(treeId);
        Map<String, Object> body = new HashMap<>();
        body.put("ownerId", ownerId);
        return ResponseEntity.ok(ApiResponse.success(body));
    }

    @PutMapping("/{treeId}")
    public ResponseEntity<ApiResponse<FamilyTree>> updateTree(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody FamilyTreeUpdateRequest req) {
        FamilyTree tree = familyTreeService.update(treeId, parseUserId(userId), req);
        return ResponseEntity.ok(ApiResponse.success(tree));
    }

    @DeleteMapping("/{treeId}")
    public ResponseEntity<ApiResponse<Void>> deleteTree(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        familyTreeService.delete(treeId, parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success());
    }

    //======================================================================================================================
    @PostMapping("/{treeId}/members")
    public ResponseEntity<ApiResponse<Person>> addMember(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody @Valid PersonCreateRequest req) {
        Person person = familyTreeService.addMember(treeId, parseUserId(userId), req);
        return ResponseEntity.ok(ApiResponse.success(person));
    }

    @GetMapping("/{treeId}/members")
    public ResponseEntity<ApiResponse<List<Person>>> listMembers(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId) {
        List<Person> people = familyTreeService.listMembers(treeId, parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success(people));
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
    public ResponseEntity<ApiResponse<Person>> updateMember(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId,
            @RequestBody PersonUpdateRequest req) {
        Person person = familyTreeService.updateMember(treeId, parseUserId(userId), personId, req);
        return ResponseEntity.ok(ApiResponse.success(person));
    }

    @DeleteMapping("/{treeId}/members/{personId}")
    public ResponseEntity<ApiResponse<Void>> deleteMember(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String userId) {
        familyTreeService.deleteMember(treeId, parseUserId(userId), personId);
        return ResponseEntity.ok(ApiResponse.success());
    }
    //==================================================================================================================

    @GetMapping("/{treeId}/relationships")
    public ResponseEntity<ApiResponse<List<RelationshipDTO>>> listRelationships(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId
    ) {
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
            @RequestParam("userId") String userId
    ) {
        List<RelationshipDTO> rels = relationshipService.listByPerson(treeId, parseUserId(userId), personId);
        return ResponseEntity.ok(ApiResponse.success(rels));
    }

    @PostMapping("/{treeId}/relationships")
    public ResponseEntity<ApiResponse<Relationship>> createRelationship(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody @Valid RelationshipCreateRequest req) {
        Relationship rel = relationshipService.create(treeId, parseUserId(userId), req);
        return ResponseEntity.ok(ApiResponse.success(rel));
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

    @DeleteMapping("/{treeId}/relationships/{relationshipId}")
    public ResponseEntity<ApiResponse<Void>> deleteRelationship(
            @PathVariable("treeId") UUID treeId,
            @PathVariable("relationshipId") UUID relationshipId,
            @RequestParam("userId") String userId) {
        relationshipService.delete(treeId, parseUserId(userId), relationshipId);
        return ResponseEntity.ok(ApiResponse.success());
    }
}