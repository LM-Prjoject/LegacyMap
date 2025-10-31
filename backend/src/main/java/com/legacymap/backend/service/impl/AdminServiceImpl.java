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
import com.legacymap.backend.repository.PersonRepository;
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
    private final PersonRepository personRepository;

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

        // 🔥 NEW: Kiểm tra không cho phép ban admin
        if ("admin".equalsIgnoreCase(user.getRoleName())) {
            log.error("🚫 Cannot ban admin user: {} ({})", user.getEmail(), userId);
            throw new AppException(ErrorCode.CANNOT_BAN_ADMIN);
        }

        String email = user.getEmail();
        log.info("📋 Current user status - Email: {}, isBanned: {}, role: {}",
                email, user.getIsBanned(), user.getRoleName());

        if (Boolean.TRUE.equals(user.getIsBanned())) {
            log.warn("⚠️ User {} is already banned", email);
            throw new AppException(ErrorCode.USER_ALREADY_BANNED);
        }

        // ✅ Tìm tất cả accounts có cùng email
        List<User> allAccountsWithSameEmail = userRepository.findAllByEmail(email);
        log.info("🔍 Found {} account(s) with email: {}", allAccountsWithSameEmail.size(), email);

        OffsetDateTime banTime = OffsetDateTime.now();

        // ✅ Ban tất cả accounts có cùng email (chỉ những account không phải admin)
        int bannedCount = 0;
        for (User account : allAccountsWithSameEmail) {
            // 🔥 NEW: Skip admin accounts
            if ("admin".equalsIgnoreCase(account.getRoleName())) {
                log.warn("⚠️ Skipping admin account: {} (ID: {})", account.getEmail(), account.getId());
                continue;
            }

            if (Boolean.FALSE.equals(account.getIsBanned()) || account.getIsBanned() == null) {
                account.setIsBanned(true);
                account.setBannedAt(banTime);

                try {
                    userRepository.save(account);
                    bannedCount++;
                    log.info("🚫 Banned account: {} (ID: {}, Provider: {})",
                            account.getEmail(), account.getId(), account.getProvider());
                } catch (Exception e) {
                    log.error("❌ Failed to ban account {}: {}", account.getId(), e.getMessage(), e);
                    throw new RuntimeException("Failed to ban account: " + account.getId(), e);
                }
            }
        }

        if (bannedCount == 0) {
            log.error("❌ No accounts were banned (all were admin accounts)");
            throw new AppException(ErrorCode.CANNOT_BAN_ADMIN);
        }

        log.info("✅ Successfully banned {} account(s) with email: {}", bannedCount, email);
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

        String email = user.getEmail();
        log.info("📋 Current user status - Email: {}, isBanned: {}", email, user.getIsBanned());

        if (Boolean.FALSE.equals(user.getIsBanned()) || user.getIsBanned() == null) {
            log.warn("⚠️ User {} is not banned", email);
            throw new AppException(ErrorCode.USER_NOT_BANNED);
        }

        // ✅ Tìm tất cả accounts có cùng email
        List<User> allAccountsWithSameEmail = userRepository.findAllByEmail(email);
        log.info("🔍 Found {} account(s) with email: {}", allAccountsWithSameEmail.size(), email);

        // ✅ Unban tất cả accounts có cùng email
        for (User account : allAccountsWithSameEmail) {
            if (Boolean.TRUE.equals(account.getIsBanned())) {
                account.setIsBanned(false);
                account.setBannedAt(null);

                try {
                    userRepository.save(account);
                    log.info("✅ Unbanned account: {} (ID: {}, Provider: {})",
                            account.getEmail(), account.getId(), account.getProvider());
                } catch (Exception e) {
                    log.error("❌ Failed to unban account {}: {}", account.getId(), e.getMessage(), e);
                    throw new RuntimeException("Failed to unban account: " + account.getId(), e);
                }
            }
        }

        log.info("✅ Successfully unbanned {} account(s) with email: {}", allAccountsWithSameEmail.size(), email);
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
                            FamilyTreeResponse dto = FamilyTreeResponse.fromEntity(tree);

                            long memberCount = personRepository.countByFamilyTree_Id(tree.getId());
                            dto.setMemberCount(memberCount);

                            log.debug("🌳 Tree '{}' has {} members", tree.getName(), memberCount);

                            return dto;
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

            long totalMembers = personRepository.countAllPersons();

            stats.put("totalUsers", allUsers.size());
            stats.put("adminUsers", adminUsers.size());
            stats.put("bannedUsers", bannedUsers.size());
            stats.put("activeUsers", allUsers.size() - bannedUsers.size());
            stats.put("totalFamilyTrees", allTrees.size());
            stats.put("totalMembers", totalMembers);
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

        log.info("🔍 Checking admin permission for: {}", authentication.getName());
        log.info("🔍 Authorities: {}", authentication.getAuthorities());

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