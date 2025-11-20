package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.UserCreateRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.UserSearchResponse;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.service.UserService;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.UUID;

@Slf4j
@RestController
@RequestMapping("/api/users")
public class UserController {

    @Autowired
    private UserService userService;

    @GetMapping("/search")
    public ResponseEntity<ApiResponse<List<UserSearchResponse>>> searchUsers(
            @RequestParam("q") String keyword,
            @RequestParam(value = "limit", defaultValue = "10") int limit
    ) {
        return ResponseEntity.ok(ApiResponse.success(userService.searchUsers(keyword, limit)));
    }

    @PostMapping("/register")
    public ResponseEntity<ApiResponse<User>> createUser(@RequestBody @Valid UserCreateRequest request) {
        User user = userService.createRequest(request);
        return ResponseEntity.ok(ApiResponse.success(user));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<User>> getUser(
            @PathVariable UUID id,
            Authentication authentication
    ) {
        validateUserAccess(id, authentication);

        User user = userService.getUserById(id);
        return ResponseEntity.ok(ApiResponse.success(user));
    }

    // Public minimal info for display (no auth required)
    @GetMapping("/{id}/basic")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getBasicUserInfo(
            @PathVariable UUID id
    ) {
        User user = userService.getUserById(id);
        UserProfile profile = userService.getUserProfileOnly(id);
        Map<String, Object> resp = new HashMap<>();
        resp.put("id", user.getId());
        resp.put("username", user.getUsername());
        resp.put("fullName", profile != null ? profile.getFullName() : null);
        return ResponseEntity.ok(ApiResponse.success(resp));
    }

    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<UserProfile>> updateUser(
            @PathVariable UUID id,
            @RequestBody UserProfile profile,
            Authentication authentication
    ) {
        log.info("Update request for userId: {} by principal: {}", id, authentication.getPrincipal());

        validateUserAccess(id, authentication);

        UserProfile updated = userService.updateUserProfile(id, profile);
        return ResponseEntity.ok(ApiResponse.success(updated));
    }

    @GetMapping("/{id}/profile")
    public ResponseEntity<ApiResponse<UserProfile>> getUserProfile(
            @PathVariable UUID id,
            Authentication authentication
    ) {
        // Kiểm tra quyền
        validateUserAccess(id, authentication);

        UserProfile profile = userService.getUserProfileOnly(id);
        return ResponseEntity.ok(ApiResponse.success(profile));
    }

    /**
     * Helper method để validate user có quyền truy cập resource không
     */
    private void validateUserAccess(UUID resourceUserId, Authentication authentication) {
        if (authentication == null || authentication.getPrincipal() == null) {
            log.warn("No authentication found");
            throw new AppException(ErrorCode.UNAUTHENTICATED);
        }

        String principalStr = authentication.getPrincipal().toString();
        UUID authenticatedUserId;

        try {
            authenticatedUserId = UUID.fromString(principalStr);
        } catch (IllegalArgumentException e) {
            log.error("Invalid UUID format in principal: {}", principalStr);
            throw new AppException(ErrorCode.UNAUTHENTICATED);
        }

        if (!authenticatedUserId.equals(resourceUserId)) {
            log.warn("User {} attempted to access resource of user {}",
                    authenticatedUserId, resourceUserId);
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        log.debug("Access granted for user: {}", authenticatedUserId);
    }

}