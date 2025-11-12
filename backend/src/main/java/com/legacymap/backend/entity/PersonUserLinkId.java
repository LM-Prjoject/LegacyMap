package com.legacymap.backend.entity;

import jakarta.persistence.Embeddable;
import lombok.*;
import java.io.Serializable;
import java.util.UUID;

@Embeddable
@Data
@NoArgsConstructor
@AllArgsConstructor
public class PersonUserLinkId implements Serializable {
    private UUID personId;
    private UUID userId;
}
