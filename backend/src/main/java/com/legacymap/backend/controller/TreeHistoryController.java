package com.legacymap.backend.controller;

import com.legacymap.backend.dto.response.TreeHistoryResponse;
import com.legacymap.backend.service.impl.AuditLogService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Slf4j
@RestController
@RequestMapping("/api/trees")
@RequiredArgsConstructor
public class TreeHistoryController {

    private final AuditLogService auditLogService;

    @GetMapping("/{treeId}/history")
    public ResponseEntity<Page<TreeHistoryResponse>> getTreeHistory(
            @PathVariable UUID treeId,
            @PageableDefault(size = 20) Pageable pageable) {

        log.info("📋 Getting history for tree: {}", treeId);
        try {
            Page<TreeHistoryResponse> history = auditLogService.getTreeHistory(treeId, pageable);
            log.info("📋 Successfully retrieved {} history items for tree: {}",
                    history.getTotalElements(), treeId);
            return ResponseEntity.ok(history);
        } catch (Exception e) {
            log.error("❌ Error getting history for tree: {}", treeId, e);
            return ResponseEntity.internalServerError().build();
        }
    }
}