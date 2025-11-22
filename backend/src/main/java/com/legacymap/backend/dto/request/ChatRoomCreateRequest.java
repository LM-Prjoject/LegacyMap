package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.ChatRoom;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class ChatRoomCreateRequest {
    @NotNull
    private ChatRoom.ChatRoomType roomType;

    @NotBlank
    private String name;
    private String description;
    private UUID familyTreeId;
    private UUID branchPersonId;
    private List<UUID> memberUserIds;
    private List<UUID> branchPersonIds;
}

