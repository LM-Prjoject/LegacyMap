package com.legacymap.backend.service;

import com.legacymap.backend.entity.AuditLog;
import com.legacymap.backend.repository.AuditLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AuditLogService {

    private final AuditLogRepository auditLogRepository;

    @Transactional
    public void log(
            UUID userId,
            UUID treeId,
            String entityType,
            UUID entityId,
            String action,
            Map<String, Object> oldData,
            Map<String, Object> newData
    ) {
        AuditLog log = AuditLog.builder()
                .userId(userId)
                .treeId(treeId)
                .entityType(entityType)
                .entityId(entityId)
                .action(action)
                .oldData( oldData)
                .newData(newData)
                .createdAt(OffsetDateTime.now())
                .build();

        auditLogRepository.save(log);
    }
}
