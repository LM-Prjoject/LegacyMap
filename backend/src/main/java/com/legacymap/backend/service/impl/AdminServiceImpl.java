// src/main/java/com/legacymap/backend/service/impl/AdminServiceImpl.java
package com.legacymap.backend.service.impl;

import com.legacymap.backend.dto.response.UserListResponse;
import com.legacymap.backend.dto.response.UserDetailResponse;
import com.legacymap.backend.dto.response.FamilyTreeResponse;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.AdminService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class AdminServiceImpl implements AdminService {

    private final UserRepository userRepository;
    private final FamilyTreeRepository familyTreeRepository;

    @Override
    public List<UserListResponse> getAllUsers() {
        checkAdminPermission();

        List<User> users = userRepository.findAllByOrderByCreatedAtDesc();

        return users.stream()
                .map(this::convertToUserListResponse)
                .collect(Collectors.toList());
    }

    @Override
    public UserDetailResponse getUserDetail(UUID userId) {
        checkAdminPermission();

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        return UserDetailResponse.builder()
                .id(user.getId())
                .email(user.getEmail())
                .username(user.getUsername())
                .roleName(user.getRoleName())
                .isVerified(user.getIsVerified())
                .isActive(user.getIsActive())
                .isBanned(user.getIsBanned())
                .bannedAt(user.getBannedAt())
                .lastLogin(user.getLastLogin())
                .createdAt(user.getCreatedAt())
                .updatedAt(user.getUpdatedAt())
                .provider(user.getProvider())
                .build();
    }

    @Override
    @Transactional
    public void banUser(UUID userId) {
        checkAdminPermission();

        log.info("🎯 Admin attempting to ban user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> {
                    log.error("❌ User not found: {}", userId);
                    return new AppException(ErrorCode.USER_NOT_FOUND);
                });

        log.info("📋 Current user status - Email: {}, isBanned: {}", user.getEmail(), user.getIsBanned());

        // 🔥 CHECK: Nếu đã bị ban thì throw exception
        if (Boolean.TRUE.equals(user.getIsBanned())) {
            log.warn("⚠️ User {} is already banned", user.getEmail());
            throw new AppException(ErrorCode.USER_ALREADY_BANNED);
        }

        // 🔥 BAN USER
        user.setIsBanned(true);
        user.setBannedAt(OffsetDateTime.now());

        try {
            userRepository.save(user);
            log.info("🚫 ✅ Admin successfully banned user: {} ({})", user.getEmail(), userId);
        } catch (Exception e) {
            log.error("❌ Failed to save banned user: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to ban user", e);
        }
    }

    @Override
    @Transactional
    public void unbanUser(UUID userId) {
        checkAdminPermission();

        log.info("🎯 Admin attempting to unban user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> {
                    log.error("❌ User not found: {}", userId);
                    return new AppException(ErrorCode.USER_NOT_FOUND);
                });

        log.info("📋 Current user status - Email: {}, isBanned: {}", user.getEmail(), user.getIsBanned());

        // 🔥 CHECK: Nếu chưa bị ban thì throw exception
        if (Boolean.FALSE.equals(user.getIsBanned()) || user.getIsBanned() == null) {
            log.warn("⚠️ User {} is not banned", user.getEmail());
            throw new AppException(ErrorCode.USER_NOT_BANNED);
        }

        // 🔥 UNBAN USER
        user.setIsBanned(false);
        user.setBannedAt(null);

        try {
            userRepository.save(user);
            log.info("✅ ✅ Admin successfully unbanned user: {} ({})", user.getEmail(), userId);
        } catch (Exception e) {
            log.error("❌ Failed to save unbanned user: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to unban user", e);
        }
    }

    @Override
    public List<FamilyTreeResponse> getAllFamilyTrees() {
        checkAdminPermission();

        log.info("🌳 Admin accessing all family trees");

        try {
            List<FamilyTree> familyTrees = familyTreeRepository.findAllByOrderByCreatedAtDesc();

            log.info("📊 Found {} family trees in database", familyTrees.size());

            List<FamilyTreeResponse> response = familyTrees.stream()
                    .map(tree -> {
                        try {
                            return FamilyTreeResponse.fromEntity(tree);
                        } catch (Exception e) {
                            log.error("❌ Error converting tree {}: {}", tree.getId(), e.getMessage(), e);
                            return null;
                        }
                    })
                    .filter(dto -> dto != null)
                    .collect(Collectors.toList());

            log.info("✅ Successfully converted {} family trees to DTOs", response.size());
            return response;

        } catch (Exception e) {
            log.error("❌ Fatal error fetching family trees", e);
            throw new RuntimeException("Failed to fetch family trees: " + e.getMessage(), e);
        }
    }

    @Override
    public Map<String, Object> getAdminStats() {
        checkAdminPermission();

        Map<String, Object> stats = new HashMap<>();

        try {
            List<User> allUsers = userRepository.findAll();
            List<User> adminUsers = userRepository.findByRoleName("admin");
            List<User> bannedUsers = userRepository.findByIsBannedTrue();
            List<FamilyTree> allTrees = familyTreeRepository.findAll();

            stats.put("totalUsers", allUsers.size());
            stats.put("adminUsers", adminUsers.size());
            stats.put("bannedUsers", bannedUsers.size());
            stats.put("activeUsers", allUsers.size() - bannedUsers.size());
            stats.put("totalFamilyTrees", allTrees.size());
            stats.put("adminUserEmails", adminUsers.stream().map(User::getEmail).collect(Collectors.toList()));

            log.info("📊 Admin Stats: {}", stats);
        } catch (Exception e) {
            log.error("❌ Error calculating admin stats: {}", e.getMessage());
            stats.put("error", e.getMessage());
        }

        return stats;
    }

    private UserListResponse convertToUserListResponse(User user) {
        return UserListResponse.builder()
                .id(user.getId())
                .email(user.getEmail())
                .username(user.getUsername())
                .roleName(user.getRoleName())
                .isVerified(user.getIsVerified())
                .isActive(user.getIsActive())
                .isBanned(user.getIsBanned())
                .bannedAt(user.getBannedAt())
                .lastLogin(user.getLastLogin())
                .createdAt(user.getCreatedAt())
                .build();
    }

    /**
     * 🔥 CRITICAL: Check if current user has ADMIN role
     */
    private void checkAdminPermission() {
        var authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null || !authentication.isAuthenticated()) {
            log.error("❌ No authentication found in SecurityContext");
            throw new AppException(ErrorCode.UNAUTHENTICATED);
        }

        // Log current authentication details
        log.info("🔍 Checking admin permission for: {}", authentication.getName());
        log.info("🔍 Authorities: {}", authentication.getAuthorities());

        // Check if user has ROLE_ADMIN
        boolean isAdmin = authentication.getAuthorities().stream()
                .anyMatch(authority -> {
                    boolean matches = authority.getAuthority().equals("ROLE_ADMIN");
                    log.debug("🔍 Authority: {} - Matches ROLE_ADMIN: {}", authority.getAuthority(), matches);
                    return matches;
                });

        if (!isAdmin) {
            log.error("🚫 User {} does not have ROLE_ADMIN", authentication.getName());
            log.error("🚫 Available authorities: {}", authentication.getAuthorities());
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        log.info("✅ Admin permission verified for user: {}", authentication.getName());
    }
}