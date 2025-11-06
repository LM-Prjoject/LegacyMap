package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.Relationship;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RelationshipDTO {
    private UUID id;
    private UUID person1Id;
    private UUID person2Id;
    private String relationshipType;
    private String notes;
    private LocalDateTime createdAt;
    private UUID createdBy;

    public static RelationshipDTO fromEntity(Relationship relationship) {
        return RelationshipDTO.builder()
                .id(relationship.getId())
                .person1Id(relationship.getPerson1().getId())
                .person2Id(relationship.getPerson2().getId())
                .relationshipType(relationship.getRelationshipType())
                .notes(relationship.getNotes())
                .createdAt(relationship.getCreatedAt().toLocalDateTime())
                .createdBy(relationship.getCreatedBy().getId())
                .build();
    }
}
