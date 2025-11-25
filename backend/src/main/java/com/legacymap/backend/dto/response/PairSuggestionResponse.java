package com.legacymap.backend.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PairSuggestionResponse {
    private UUID candidateId;
    private String type;       // parent|child|spouse|sibling
    private double confidence; // 0..1
    private List<String> reasons;
}
