package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.UUID;

@Data
public class DirectRoomCreateRequest {
    @NotNull
    private UUID targetUserId;
    private String name;
}

