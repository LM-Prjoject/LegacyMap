package com.legacymap.backend.dto.export;

import java.util.List;

public record TreeExportDTO(
        TreeExportSummary summary,
        List<MemberExportRow> members
) {}
