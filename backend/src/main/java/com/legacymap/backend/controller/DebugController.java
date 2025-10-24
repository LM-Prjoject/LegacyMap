// src/main/java/com/legacymap/backend/controller/DebugController.java
package com.legacymap.backend.controller;

import com.legacymap.backend.dto.response.FamilyTreeResponse;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@RestController
@RequestMapping("/api/debug")
@RequiredArgsConstructor
public class DebugController {

    private final UserRepository userRepository;
    private final FamilyTreeRepository familyTreeRepository;

    @GetMapping("/stats")
    public ResponseEntity<Map<String, Object>> getDebugStats() {
        Map<String, Object> stats = new HashMap<>();

        // User stats
        List<User> allUsers = userRepository.findAll();
        List<User> adminUsers = userRepository.findByRoleName("admin");

        stats.put("totalUsers", allUsers.size());
        stats.put("adminUsers", adminUsers.size());
        stats.put("adminUserEmails", adminUsers.stream().map(User::getEmail).toList());

        // Family tree stats - Sử dụng DTO để tránh lỗi serialization
        List<FamilyTree> allTrees = familyTreeRepository.findAll();
        List<FamilyTreeResponse> treeDTOs = allTrees.stream()
                .map(FamilyTreeResponse::fromEntity)
                .collect(Collectors.toList());

        stats.put("totalFamilyTrees", allTrees.size());
        stats.put("familyTrees", treeDTOs); // 🔥 SỬA: Dùng DTO thay vì Entity

        log.info("🔍 Debug Stats: {}", stats);
        return ResponseEntity.ok(stats);
    }

    @GetMapping("/family-trees-test")
    public ResponseEntity<?> testFamilyTrees() {
        try {
            List<FamilyTree> allTrees = familyTreeRepository.findAll();
            log.info("🔍 Found {} family trees", allTrees.size());

            // Test conversion to DTO
            List<FamilyTreeResponse> dtos = allTrees.stream()
                    .map(FamilyTreeResponse::fromEntity)
                    .collect(Collectors.toList());

            return ResponseEntity.ok().body(
                    new Object() {
                        public int count = allTrees.size();
                        public List<FamilyTreeResponse> trees = dtos;
                        public String status = "SUCCESS";
                    }
            );
        } catch (Exception e) {
            log.error("❌ Debug Family Trees Error: {}", e.getMessage(), e);
            return ResponseEntity.internalServerError().body(
                    new Object() {
                        public String error = e.getMessage();
                        public String type = e.getClass().getSimpleName();
                        public String status = "ERROR";
                    }
            );
        }
    }
}