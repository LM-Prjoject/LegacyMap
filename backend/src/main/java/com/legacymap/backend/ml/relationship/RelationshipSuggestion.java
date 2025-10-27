package com.legacymap.backend.ml.relationship;

import com.legacymap.backend.enums.RelationshipType;

import java.util.UUID;

public class RelationshipSuggestion {
    private final UUID candidateId;
    private final RelationshipType relation;
    private final double confidence;

    public RelationshipSuggestion(UUID candidateId, RelationshipType relation, double confidence) {
        this.candidateId = candidateId;
        this.relation = relation;
        this.confidence = confidence;
    }

    public UUID getCandidateId() {
        return candidateId;
    }

    public RelationshipType getRelation() {
        return relation;
    }

    public double getConfidence() {
        return confidence;
    }
}
