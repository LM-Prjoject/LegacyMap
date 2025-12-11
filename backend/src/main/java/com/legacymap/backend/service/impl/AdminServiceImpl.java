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
    private final UserSessionService userSessionService;

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
        log.info("🔍 getUserDetail called for userId: {}", userId);
        checkAdminPermission();

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        log.info("🔍 Found user: {} ({})", user.getUsername(), user.getEmail());
        
        // Tính toán thống kê người dùng
        UserDetailResponse.UserStatistics statistics = calculateUserStatistics(user);

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
                .statistics(statistics)
                .build();
    }

    private UserDetailResponse.UserStatistics calculateUserStatistics(User user) {
        // Đếm số cây gia phả của user
        Long familyTreeCount = familyTreeRepository.countByCreatedById(user.getId());
        log.info("🔍 User {} statistics: familyTreeCount={}", user.getId(), familyTreeCount);
        
        // Tính thời gian đăng nhập lần cuối
        String lastLoginText = calculateLastLoginText(user.getLastLogin());
        log.info("🔍 User {} statistics: lastLoginText={}, lastLogin={}", user.getId(), lastLoginText, user.getLastLogin());
        
        // Tính số ngày sử dụng (từ ngày tạo tài khoản đến hiện tại)
        Long usageDays = calculateUsageDays(user.getCreatedAt());
        log.info("🔍 User {} statistics: usageDays={}, createdAt={}", user.getId(), usageDays, user.getCreatedAt());
        
        return UserDetailResponse.UserStatistics.builder()
                .familyTreeCount(familyTreeCount)
                .lastLoginText(lastLoginText)
                .usageDays(usageDays)
                .build();
    }
    
    private String calculateLastLoginText(OffsetDateTime lastLogin) {
        if (lastLogin == null) {
            return "Chưa bao giờ";
        }
        
        OffsetDateTime now = OffsetDateTime.now();
        long daysBetween = java.time.Duration.between(lastLogin, now).toDays();
        
        // Format giờ và ngày
        java.time.format.DateTimeFormatter formatter = java.time.format.DateTimeFormatter.ofPattern("HH:mm dd/MM/yyyy");
        String formattedDateTime = lastLogin.format(formatter);
        
        if (daysBetween == 0) {
            return "Hôm nay lúc " + lastLogin.format(java.time.format.DateTimeFormatter.ofPattern("HH:mm"));
        } else if (daysBetween == 1) {
            return "Hôm qua lúc " + lastLogin.format(java.time.format.DateTimeFormatter.ofPattern("HH:mm"));
        } else if (daysBetween < 7) {
            return daysBetween + " ngày trước (" + formattedDateTime + ")";
        } else if (daysBetween < 30) {
            long weeks = daysBetween / 7;
            return weeks + " tuần trước (" + formattedDateTime + ")";
        } else if (daysBetween < 365) {
            long months = daysBetween / 30;
            return months + " tháng trước (" + formattedDateTime + ")";
        } else {
            long years = daysBetween / 365;
            return years + " năm trước (" + formattedDateTime + ")";
        }
    }
    
    private Long calculateUsageDays(OffsetDateTime createdAt) {
        if (createdAt == null) {
            return 0L;
        }
        
        OffsetDateTime now = OffsetDateTime.now();
        return java.time.Duration.between(createdAt, now).toDays();
    }

    @Override
    @Transactional
    public void banUser(UUID userId) {
        checkAdminPermission();

        User user = userRepository.findById(userId)
                .orElseThrow(() -> {
                    log.error("User not found: {}", userId);
                    return new AppException(ErrorCode.USER_NOT_FOUND);
                });

        if ("admin".equalsIgnoreCase(user.getRoleName())) {
            log.error("Cannot ban admin user: {} ({})", user.getEmail(), userId);
            throw new AppException(ErrorCode.CANNOT_BAN_ADMIN);
        }

        String email = user.getEmail();

        if (Boolean.TRUE.equals(user.getIsBanned())) {
            throw new AppException(ErrorCode.USER_ALREADY_BANNED);
        }

        List<User> allAccountsWithSameEmail = userRepository.findAllByEmail(email);
        log.info("🔍 Found {} account(s) with email: {}", allAccountsWithSameEmail.size(), email);

        OffsetDateTime banTime = OffsetDateTime.now();

        int bannedCount = 0;
        for (User account : allAccountsWithSameEmail) {
            if ("admin".equalsIgnoreCase(account.getRoleName())) {
                continue;
            }

            if (Boolean.FALSE.equals(account.getIsBanned()) || account.getIsBanned() == null) {
                account.setIsBanned(true);
                account.setBannedAt(banTime);

                try {
                    userRepository.save(account);
                    bannedCount++;
                } catch (Exception e) {
                    throw new RuntimeException("Failed to ban account: " + account.getId(), e);
                }
            }
        }

        if (bannedCount == 0) {
            throw new AppException(ErrorCode.CANNOT_BAN_ADMIN);
        }
    }

    @Override
    @Transactional
    public void unbanUser(UUID userId) {
        checkAdminPermission();

        User user = userRepository.findById(userId)
                .orElseThrow(() -> {
                    log.error("User not found: {}", userId);
                    return new AppException(ErrorCode.USER_NOT_FOUND);
                });

        String email = user.getEmail();

        if (Boolean.FALSE.equals(user.getIsBanned()) || user.getIsBanned() == null) {
            throw new AppException(ErrorCode.USER_NOT_BANNED);
        }

        List<User> allAccountsWithSameEmail = userRepository.findAllByEmail(email);

        int unbannedCount = 0;
        for (User account : allAccountsWithSameEmail) {
            if (Boolean.TRUE.equals(account.getIsBanned())) {
                account.setIsBanned(false);
                account.setFailedAttempts(0);
                account.setIsActive(true);
                account.setBannedAt(null);

                try {
                    userRepository.save(account);
                    unbannedCount++;
                } catch (Exception e) {
                    throw new RuntimeException("Failed to unban account: " + account.getId(), e);
                }
            }
        }

        if (unbannedCount == 0) {
            throw new AppException(ErrorCode.USER_NOT_BANNED);
        }
    }

    @Override
    @Transactional(readOnly = true)
    public List<FamilyTreeResponse> getAllFamilyTrees() {
        checkAdminPermission();

        try {
            List<FamilyTree> familyTrees = familyTreeRepository.findAllWithUserOrderByCreatedAtDesc();

            List<FamilyTreeResponse> response = familyTrees.stream()
                    .map(tree -> {
                        try {
                            FamilyTreeResponse dto = FamilyTreeResponse.fromEntity(tree);

                            long memberCount = personRepository.countByFamilyTree_Id(tree.getId());
                            dto.setMemberCount(memberCount);

                            return dto;
                        } catch (Exception e) {
                            log.error("Error converting tree {}: {}", tree.getId(), e.getMessage(), e);
                            return null;
                        }
                    })
                    .filter(dto -> dto != null)
                    .collect(Collectors.toList());

            if (!response.isEmpty()) {
                log.info("First tree in response: {}", response.get(0));
            }

            return response;

        } catch (Exception e) {
            log.error("Fatal error fetching family trees", e);
            throw new RuntimeException("Failed to fetch family trees: " + e.getMessage(), e);
        }
    }

    @Override
    @Transactional(readOnly = true)
    public Map<String, Object> getAdminStats() {
        checkAdminPermission();

        Map<String, Object> stats = new HashMap<>();

        try {
            List<User> allUsers = userRepository.findAll();

            int totalUsers = allUsers.size();

            long bannedUsers = allUsers.stream()
                    .filter(user -> Boolean.TRUE.equals(user.getIsBanned()))
                    .count();

            long activeUsers = totalUsers - bannedUsers;
            List<UUID> onlineUserIds = userSessionService.getOnlineUserIds();
            long onlineUsers = onlineUserIds.size();
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

            return stats;

        } catch (Exception e) {
            throw new RuntimeException("Failed to calculate admin statistics", e);
        }
    }

    @Override
    public List<UUID> getOnlineUserIds() {
        checkAdminPermission();
        return userSessionService.getOnlineUserIds();
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
            throw new AppException(ErrorCode.UNAUTHENTICATED);
        }
        boolean isAdmin = authentication.getAuthorities().stream()
                .anyMatch(authority -> {
                    boolean matches = authority.getAuthority().equals("ROLE_ADMIN");
                    return matches;
                });

        if (!isAdmin) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }
    }
}