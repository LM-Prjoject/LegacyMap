package com.legacymap.backend.controller;

import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.UserListResponse;
import com.legacymap.backend.dto.response.UserDetailResponse;
import com.legacymap.backend.dto.response.FamilyTreeResponse; // 🔥 THÊM
import com.legacymap.backend.service.AdminService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

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
        log.info("AdminController.getAllFamilyTrees() CALLED");
        try {
            log.info("Admin accessing all family trees");
            List<FamilyTreeResponse> familyTrees = adminService.getAllFamilyTrees();
            log.info("Admin retrieved {} family trees", familyTrees.size());
            return ResponseEntity.ok(familyTrees);
        } catch (Exception e) {
            log.error("Error getting family trees: {}", e.getMessage(), e);
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
}