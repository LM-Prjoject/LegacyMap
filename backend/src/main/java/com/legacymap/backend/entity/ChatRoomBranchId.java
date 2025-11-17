package com.legacymap.backend.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Embeddable
public class ChatRoomBranchId implements Serializable {

    @Column(name = "room_id", nullable = false)
    private UUID roomId;

    @Column(name = "branch_person_id", nullable = false)
    private UUID branchPersonId;
}

