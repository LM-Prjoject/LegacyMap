package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.ChatRoomMember;
import lombok.Data;

import java.util.UUID;

@Data
public class JoinRoomRequest {
    private UUID personId;
    private ChatRoomMember.ChatMemberRole role = ChatRoomMember.ChatMemberRole.member;
}

