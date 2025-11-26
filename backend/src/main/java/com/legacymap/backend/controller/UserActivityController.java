package com.legacymap.backend.controller;

import com.legacymap.backend.service.UserSessionService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class UserActivityController {

    private final UserSessionService sessionService;

    /**
     * ✅ Heartbeat endpoint - Frontend gọi mỗi 30 giây để update activity
     */
    @PostMapping("/heartbeat")
    public ResponseEntity<Map<String, String>> updateActivity(
            HttpServletRequest request,
            Authentication authentication
    ) {
        try {
            // Lấy session token từ Authorization header
            String authHeader = request.getHeader("Authorization");
            if (authHeader != null && authHeader.startsWith("Bearer ")) {
                String token = authHeader.substring(7);
                sessionService.updateActivity(token);
            }

            return ResponseEntity.ok(Map.of("status", "ok"));
        } catch (Exception e) {
            log.error("Error updating user activity: {}", e.getMessage());
            return ResponseEntity.ok(Map.of("status", "error")); // Still return 200 to not break frontend
        }
    }

    /**
     * Check if user is online
     */
    @GetMapping("/online-status")
    public ResponseEntity<Map<String, Object>> getOnlineStatus(Authentication authentication) {
        // Implementation here
        return ResponseEntity.ok(Map.of("online", true));
    }
}