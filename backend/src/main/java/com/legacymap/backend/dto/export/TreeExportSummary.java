package com.legacymap.backend.dto.export;

import java.time.OffsetDateTime;

public record TreeExportSummary(
        String treeId,
        String name,
        String description,
        String ownerName,
        String ownerEmail,
        OffsetDateTime createdAt,
        long memberCount,
        int generationCount,
        long maleCount,
        long femaleCount,
        long otherCount,
        long aliveCount,
        long deceasedCount
) {}
