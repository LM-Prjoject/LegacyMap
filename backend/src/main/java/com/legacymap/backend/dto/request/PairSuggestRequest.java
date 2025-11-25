package com.legacymap.backend.dto.request;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class PairSuggestRequest {
    private UUID sourceId;
    private List<UUID> candidateIds;
}
