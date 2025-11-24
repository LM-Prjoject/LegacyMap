package com.legacymap.backend.dto.export;

public record MemberExportRow(
        int index,
        String fullName,
        String gender,
        String birthYear,
        String deathYear,
        String role
) {}
