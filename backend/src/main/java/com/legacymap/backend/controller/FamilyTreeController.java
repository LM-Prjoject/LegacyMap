package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.dto.request.RelationshipCreateRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.service.FamilyTreeService;
import com.legacymap.backend.service.RelationshipService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/trees")
public class FamilyTreeController {

    @Autowired
    private FamilyTreeService familyTreeService;

    @Autowired
    private RelationshipService relationshipService;

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
    public ResponseEntity<ApiResponse<List<FamilyTree>>> listTrees(
            @RequestParam("userId") String userId) {
        List<FamilyTree> trees = familyTreeService.listByUser(parseUserId(userId));
        return ResponseEntity.ok(ApiResponse.success(trees));
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

    @PostMapping("/{treeId}/relationships")
    public ResponseEntity<ApiResponse<Relationship>> createRelationship(
            @PathVariable("treeId") UUID treeId,
            @RequestParam("userId") String userId,
            @RequestBody @Valid RelationshipCreateRequest req) {
        Relationship rel = relationshipService.create(treeId, parseUserId(userId), req);
        return ResponseEntity.ok(ApiResponse.success(rel));
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
