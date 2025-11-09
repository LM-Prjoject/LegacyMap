package com.legacymap.backend.controller;

import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.UserListResponse;
import com.legacymap.backend.dto.response.UserDetailResponse;
import com.legacymap.backend.dto.response.FamilyTreeResponse;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.service.AdminService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Slf4j
@RestController
@RequestMapping("/api/admin")
@RequiredArgsConstructor
@PreAuthorize("hasRole('ADMIN')")
public class AdminController {

    private final AdminService adminService;
    private final FamilyTreeRepository familyTreeRepository;

    @GetMapping("/users")
    public ResponseEntity<List<UserListResponse>> getAllUsers() {
        List<UserListResponse> users = adminService.getAllUsers();
        log.info("Admin retrieved {} users", users.size());
        return ResponseEntity.ok(users);
    }

    @GetMapping("/users/{userId}")
    public ResponseEntity<UserDetailResponse> getUserDetail(@PathVariable UUID userId) {
        UserDetailResponse userDetail = adminService.getUserDetail(userId);
        return ResponseEntity.ok(userDetail);
    }

    @PostMapping("/users/{userId}/ban")
    public ResponseEntity<ApiResponse> banUser(@PathVariable UUID userId) {
        adminService.banUser(userId);
        return ResponseEntity.ok(ApiResponse.builder()
                .message("User banned successfully")
                .build());
    }

    @PostMapping("/users/{userId}/unban")
    public ResponseEntity<ApiResponse> unbanUser(@PathVariable UUID userId) {
        adminService.unbanUser(userId);
        return ResponseEntity.ok(ApiResponse.builder()
                .message("User unbanned successfully")
                .build());
    }

    @GetMapping("/family-trees")
    public ResponseEntity<List<FamilyTreeResponse>> getAllFamilyTrees() {
        log.info("🌳 AdminController.getAllFamilyTrees() CALLED");
        try {
            log.info("📊 Accessing all family trees");

            List<FamilyTreeResponse> familyTrees = adminService.getAllFamilyTrees();

            log.info("✅ Admin retrieved {} family trees", familyTrees.size());

            // ✅ Log chi tiết response
            if (familyTrees != null && !familyTrees.isEmpty()) {
                log.info("📦 First tree: ID={}, Name={}",
                        familyTrees.get(0).getId(),
                        familyTrees.get(0).getName());
            } else {
                log.warn("⚠️ Family trees list is empty or null");
            }

            return ResponseEntity.ok(familyTrees);

        } catch (Exception e) {
            log.error("❌ Error getting family trees: {}", e.getMessage(), e);
            return ResponseEntity.status(500).build();
        }
    }

    @GetMapping("/stats")
    public ResponseEntity<Map<String, Object>> getAdminStats() {
        try {
            log.info("Admin accessing stats");
            Map<String, Object> stats = adminService.getAdminStats();
            return ResponseEntity.ok(stats);
        } catch (Exception e) {
            log.error("Error getting admin stats: {}", e.getMessage());
            return ResponseEntity.badRequest().build();
        }
    }

    // ✅ DEBUG ENDPOINT - Xóa sau khi fix xong
    @GetMapping("/debug/trees")
    public ResponseEntity<Map<String, Object>> debugTrees() {
        log.info("🔍 DEBUG: Checking family trees");

        Map<String, Object> debug = new HashMap<>();

        try {
            // ✅ Đếm trực tiếp trong database
            long count = familyTreeRepository.count();
            debug.put("totalTreesInDB", count);
            log.info("📊 Total trees in DB: {}", count);

            // ✅ Lấy tất cả không JOIN
            List<FamilyTree> simple = familyTreeRepository.findAllByOrderByCreatedAtDesc();
            debug.put("simpleQueryCount", simple.size());
            log.info("📊 Simple query returned: {}", simple.size());

            // ✅ Lấy với JOIN FETCH
            List<FamilyTree> withJoin = familyTreeRepository.findAllWithUserOrderByCreatedAtDesc();
            debug.put("joinQueryCount", withJoin.size());
            log.info("📊 JOIN FETCH query returned: {}", withJoin.size());

            // ✅ Log chi tiết first tree
            if (!simple.isEmpty()) {
                FamilyTree first = simple.get(0);
                Map<String, Object> firstTree = new HashMap<>();
                firstTree.put("id", first.getId());
                firstTree.put("name", first.getName());
                firstTree.put("description", first.getDescription());
                firstTree.put("isPublic", first.getIsPublic());
                firstTree.put("createdAt", first.getCreatedAt());

                try {
                    if (first.getCreatedBy() != null) {
                        firstTree.put("createdByEmail", first.getCreatedBy().getEmail());
                        firstTree.put("createdByUsername", first.getCreatedBy().getUsername());
                    } else {
                        firstTree.put("createdBy", "NULL");
                    }
                } catch (Exception e) {
                    firstTree.put("createdByError", e.getMessage());
                }

                debug.put("firstTree", firstTree);
            }

            // ✅ Test service method
            try {
                List<FamilyTreeResponse> serviceResult = adminService.getAllFamilyTrees();
                debug.put("serviceMethodCount", serviceResult.size());

                if (!serviceResult.isEmpty()) {
                    Map<String, Object> firstDto = new HashMap<>();
                    FamilyTreeResponse dto = serviceResult.get(0);
                    firstDto.put("id", dto.getId());
                    firstDto.put("name", dto.getName());
                    firstDto.put("createdByEmail", dto.getCreatedByEmail());
                    firstDto.put("memberCount", dto.getMemberCount());
                    debug.put("firstDTO", firstDto);
                }
            } catch (Exception e) {
                debug.put("serviceMethodError", e.getMessage());
                log.error("❌ Service method error: {}", e.getMessage(), e);
            }

            return ResponseEntity.ok(debug);

        } catch (Exception e) {
            log.error("❌ Debug error: {}", e.getMessage(), e);
            debug.put("error", e.getMessage());
            debug.put("errorType", e.getClass().getName());
            return ResponseEntity.status(500).body(debug);
        }
    }
}