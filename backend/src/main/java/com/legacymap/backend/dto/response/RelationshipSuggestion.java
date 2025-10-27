package com.legacymap.backend.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RelationshipSuggestion {
    private String type;       // parent|child|spouse|sibling
    private double confidence; // 0..1
    private List<String> reasons;
}
