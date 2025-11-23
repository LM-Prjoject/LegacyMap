package com.legacymap.backend.entity;

import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class TreeAccessId implements Serializable {
    private UUID userId;
    private UUID familyTreeId;
}