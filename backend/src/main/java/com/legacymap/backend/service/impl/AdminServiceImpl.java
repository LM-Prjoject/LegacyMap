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
import com.legacymap.backend.service.UserSessionService;  // ✅ ADD THIS IMPORT
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
    private final UserSessionService userSessionService;  // ✅ ADD THIS FIELD

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

        List<User> allAccountsWithSameEmail = userRepository.findAllByEmail(email);
        log.info("🔍 Found {} account(s) with email: {}", allAccountsWithSameEmail.size(), email);

        OffsetDateTime banTime = OffsetDateTime.now();

        int bannedCount = 0;
        for (User account : allAccountsWithSameEmail) {
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

        List<User> allAccountsWithSameEmail = userRepository.findAllByEmail(email);
        log.info("🔍 Found {} account(s) with email: {}", allAccountsWithSameEmail.size(), email);

        int unbannedCount = 0;
        for (User account : allAccountsWithSameEmail) {
            if (Boolean.TRUE.equals(account.getIsBanned())) {
                account.setIsBanned(false);
                account.setBannedAt(null);

                try {
                    userRepository.save(account);
                    unbannedCount++;
                    log.info("✅ Unbanned account: {} (ID: {}, Provider: {})",
                            account.getEmail(), account.getId(), account.getProvider());
                } catch (Exception e) {
                    log.error("❌ Failed to unban account {}: {}", account.getId(), e.getMessage(), e);
                    throw new RuntimeException("Failed to unban account: " + account.getId(), e);
                }
            }
        }

        if (unbannedCount == 0) {
            log.warn("⚠️ No accounts were unbanned");
            throw new AppException(ErrorCode.USER_NOT_BANNED);
        }

        log.info("✅ Successfully unbanned {} account(s) with email: {}", unbannedCount, email);
    }

    @Override
    @Transactional(readOnly = true)
    public List<FamilyTreeResponse> getAllFamilyTrees() {
        checkAdminPermission();

        log.info("🌳 Admin accessing all family trees");

        try {
            List<FamilyTree> familyTrees = familyTreeRepository.findAllWithUserOrderByCreatedAtDesc();

            log.info("📊 Found {} family trees in database", familyTrees.size());

            for (FamilyTree tree : familyTrees) {
                log.info("🌳 Tree ID: {}, Name: {}, CreatedBy: {}",
                        tree.getId(),
                        tree.getName(),
                        tree.getCreatedBy() != null ? tree.getCreatedBy().getEmail() : "NULL");
            }

            List<FamilyTreeResponse> response = familyTrees.stream()
                    .map(tree -> {
                        try {
                            FamilyTreeResponse dto = FamilyTreeResponse.fromEntity(tree);

                            long memberCount = personRepository.countByFamilyTree_Id(tree.getId());
                            dto.setMemberCount(memberCount);

                            log.debug("✅ Converted tree '{}' with {} members", tree.getName(), memberCount);

                            return dto;
                        } catch (Exception e) {
                            log.error("❌ Error converting tree {}: {}", tree.getId(), e.getMessage(), e);
                            return null;
                        }
                    })
                    .filter(dto -> dto != null)
                    .collect(Collectors.toList());

            log.info("✅ Successfully converted {} family trees to DTOs", response.size());

            if (!response.isEmpty()) {
                log.info("📦 First tree in response: {}", response.get(0));
            }

            return response;

        } catch (Exception e) {
            log.error("❌ Fatal error fetching family trees", e);
            throw new RuntimeException("Failed to fetch family trees: " + e.getMessage(), e);
        }
    }

    @Override
    @Transactional(readOnly = true)
    public Map<String, Object> getAdminStats() {
        checkAdminPermission();

        log.info("✅ AdminService: Calculating admin statistics");

        Map<String, Object> stats = new HashMap<>();

        try {
            List<User> allUsers = userRepository.findAll();

            int totalUsers = allUsers.size();

            long bannedUsers = allUsers.stream()
                    .filter(user -> Boolean.TRUE.equals(user.getIsBanned()))
                    .count();

            long activeUsers = totalUsers - bannedUsers;

            // ✅ Get real-time online users from session tracking
            long onlineUsers = userSessionService.countOnlineUsers();

            log.info("📊 Real-time online users: {}", onlineUsers);

            long adminUsers = allUsers.stream()
                    .filter(u -> "admin".equalsIgnoreCase(u.getRoleName()))
                    .count();

            long moderatorUsers = allUsers.stream()
                    .filter(u -> "moderator".equalsIgnoreCase(u.getRoleName()))
                    .count();

            long regularUsers = totalUsers - adminUsers - moderatorUsers;

            OffsetDateTime firstDayOfMonth = OffsetDateTime.now()
                    .withDayOfMonth(1)
                    .withHour(0)
                    .withMinute(0)
                    .withSecond(0)
                    .withNano(0);

            long newUsersThisMonth = allUsers.stream()
                    .filter(user -> user.getCreatedAt() != null &&
                            user.getCreatedAt().isAfter(firstDayOfMonth))
                    .count();

            OffsetDateTime oneDayAgo = OffsetDateTime.now().minusDays(1);
            OffsetDateTime oneWeekAgo = OffsetDateTime.now().minusDays(7);
            OffsetDateTime oneMonthAgo = OffsetDateTime.now().minusMonths(1);

            long activeToday = allUsers.stream()
                    .filter(user -> user.getLastLogin() != null &&
                            user.getLastLogin().isAfter(oneDayAgo))
                    .count();

            long activeThisWeek = allUsers.stream()
                    .filter(user -> user.getLastLogin() != null &&
                            user.getLastLogin().isAfter(oneWeekAgo))
                    .count();

            long activeThisMonth = allUsers.stream()
                    .filter(user -> user.getLastLogin() != null &&
                            user.getLastLogin().isAfter(oneMonthAgo))
                    .count();

            long totalFamilyTrees = familyTreeRepository.count();
            long totalMembers = personRepository.count();

            stats.put("totalUsers", totalUsers);
            stats.put("activeUsers", activeUsers);
            stats.put("bannedUsers", bannedUsers);
            stats.put("onlineUsers", onlineUsers);
            stats.put("adminUsers", adminUsers);
            stats.put("moderatorUsers", moderatorUsers);
            stats.put("regularUsers", regularUsers);
            stats.put("newUsersThisMonth", newUsersThisMonth);
            stats.put("totalFamilyTrees", totalFamilyTrees);
            stats.put("totalMembers", totalMembers);

            Map<String, Object> activityStats = new HashMap<>();
            activityStats.put("loginsToday", activeToday);
            activityStats.put("loginsThisWeek", activeThisWeek);
            activityStats.put("loginsThisMonth", activeThisMonth);
            activityStats.put("newUsersThisMonth", newUsersThisMonth);

            double loginsTodayPercent = totalUsers > 0 ? (activeToday * 100.0 / totalUsers) : 0;
            double newUsersPercent = totalUsers > 0 ? (newUsersThisMonth * 100.0 / totalUsers) : 0;

            activityStats.put("loginsTodayPercent", Math.round(loginsTodayPercent));
            activityStats.put("newUsersPercent", Math.round(newUsersPercent));

            stats.put("activityStats", activityStats);

            log.info("📊 Stats calculated: {} total users, {} online (real-time), {} active",
                    totalUsers, onlineUsers, activeUsers);

            return stats;

        } catch (Exception e) {
            log.error("❌ Error calculating admin stats", e);
            throw new RuntimeException("Failed to calculate admin statistics", e);
        }
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