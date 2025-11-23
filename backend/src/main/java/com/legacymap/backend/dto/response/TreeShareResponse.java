package com.legacymap.backend.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TreeShareResponse {
    private UUID treeId;
    private String treeName;
    private UUID shareToken;
    private String shareUrl;  // Full URL để chia sẻ
    private String publicShareUrl; // Link share công khai
    private Integer sharedWithCount; // Số người được chia sẻ
    private String sharePermission; // ✅ THÊM field này

}