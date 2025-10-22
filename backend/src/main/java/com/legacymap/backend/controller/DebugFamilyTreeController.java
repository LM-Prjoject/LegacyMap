// src/main/java/com/legacymap/backend/controller/DebugFamilyTreeController.java
package com.legacymap.backend.controller;

import com.legacymap.backend.repository.FamilyTreeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/api/debug")
@RequiredArgsConstructor
public class DebugFamilyTreeController {

    private final FamilyTreeRepository familyTreeRepository;

    @GetMapping("/family-trees-simple")
    public ResponseEntity<?> getSimpleFamilyTrees() {
        try {
            log.info("🔍 Starting simple family trees debug...");

            // Test 1: Basic count
            long count = familyTreeRepository.count();
            log.info("🔍 Family trees count: {}", count);

            // Test 2: Simple data without relationships
            var result = new HashMap<String, Object>();
            result.put("count", count);
            result.put("status", "SUCCESS");
            result.put("message", "Basic count works");

            log.info("🔍 Simple debug completed successfully");
            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("❌ SIMPLE DEBUG ERROR: {}", e.getMessage(), e);
            return ResponseEntity.internalServerError().body(
                    Map.of(
                            "error", e.getMessage(),
                            "errorType", e.getClass().getName(),
                            "status", "ERROR"
                    )
            );
        }
    }

    @GetMapping("/family-trees-with-data")
    public ResponseEntity<?> getFamilyTreesWithData() {
        try {
            log.info("🔍 Testing family trees with data...");

            // Test with actual data but simplified
            var trees = familyTreeRepository.findAll();
            log.info("🔍 Found {} trees", trees.size());

            var simplifiedTrees = trees.stream()
                    .map(tree -> Map.of(
                            "id", tree.getId().toString(),
                            "name", tree.getName(),
                            "description", tree.getDescription(),
                            "createdBy", tree.getCreatedBy() != null ? tree.getCreatedBy().getId().toString() : "null"
                    ))
                    .toList();

            var result = Map.of(
                    "count", trees.size(),
                    "trees", simplifiedTrees,
                    "status", "SUCCESS"
            );

            log.info("🔍 Data debug completed successfully");
            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("❌ DATA DEBUG ERROR: {}", e.getMessage(), e);
            return ResponseEntity.internalServerError().body(
                    Map.of(
                            "error", e.getMessage(),
                            "errorType", e.getClass().getName(),
                            "status", "ERROR",
                            "stackTrace", e.getStackTrace()[0].toString()
                    )
            );
        }
    }
}