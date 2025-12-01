package com.legacymap.backend.controller;

import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.UserListResponse;
import com.legacymap.backend.dto.response.UserDetailResponse;
import com.legacymap.backend.dto.response.FamilyTreeResponse;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.service.AdminService;
import com.legacymap.backend.service.UnbanRequestService;
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
    private final UnbanRequestService unbanRequestService;

    @GetMapping("/users")
    public ResponseEntity<List<UserListResponse>> getAllUsers() {
        List<UserListResponse> users = adminService.getAllUsers();
        log.info("Admin retrieved {} users", users.size());
        return ResponseEntity.ok(users);
    }

    @GetMapping("/users/online")
    public ResponseEntity<Map<String, Object>> getOnlineUsers() {
        try {
            List<UUID> onlineUserIds = adminService.getOnlineUserIds();
            long onlineCount = onlineUserIds.size();


            Map<String, Object> response = new HashMap<>();
            response.put("onlineUserIds", onlineUserIds);
            response.put("onlineCount", onlineCount);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("❌ Error getting online users: {}", e.getMessage(), e);
            return ResponseEntity.status(500).build();
        }
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
        try {
            List<FamilyTreeResponse> familyTrees = adminService.getAllFamilyTrees();
            return ResponseEntity.ok(familyTrees);
        } catch (Exception e) {
            return ResponseEntity.status(500).build();
        }
    }

    @GetMapping("/stats")
    public ResponseEntity<Map<String, Object>> getAdminStats() {
        try {
            Map<String, Object> stats = adminService.getAdminStats();
            return ResponseEntity.ok(stats);
        } catch (Exception e) {
            return ResponseEntity.badRequest().build();
        }
    }

    @PostMapping("/unban-requests/{id}/approve")
    public ResponseEntity<ApiResponse> approveUnbanRequest(@PathVariable UUID id) {
        unbanRequestService.approveRequest(id);
        return ResponseEntity.ok(ApiResponse.builder()
                .message("Đã chấp nhận yêu cầu mở khóa tài khoản.")
                .build());
    }

    @PostMapping("/unban-requests/{id}/deny")
    public ResponseEntity<ApiResponse> denyUnbanRequest(@PathVariable UUID id) {
        unbanRequestService.denyRequest(id);
        return ResponseEntity.ok(ApiResponse.builder()
                .message("Đã từ chối yêu cầu mở khóa tài khoản.")
                .build());
    }
}