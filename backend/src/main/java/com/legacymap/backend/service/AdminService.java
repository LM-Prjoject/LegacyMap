// src/main/java/com/legacymap/backend/service/AdminService.java
package com.legacymap.backend.service;

import com.legacymap.backend.dto.response.UserListResponse;
import com.legacymap.backend.dto.response.UserDetailResponse;
import com.legacymap.backend.dto.response.FamilyTreeResponse; // 🔥 THÊM

import java.util.List;
import java.util.Map;
import java.util.UUID;

public interface AdminService {
    List<UserListResponse> getAllUsers();
    UserDetailResponse getUserDetail(UUID userId);
    void banUser(UUID userId);
    void unbanUser(UUID userId);

    // 🔥 SỬA: Trả về DTO thay vì Entity
    List<FamilyTreeResponse> getAllFamilyTrees();
    Map<String, Object> getAdminStats();
}