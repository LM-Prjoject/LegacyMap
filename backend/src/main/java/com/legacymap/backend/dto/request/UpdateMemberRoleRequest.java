package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.ChatRoomMember;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class UpdateMemberRoleRequest {
    @NotNull
    @Enumerated(EnumType.STRING)
    private ChatRoomMember.ChatMemberRole role;
}
