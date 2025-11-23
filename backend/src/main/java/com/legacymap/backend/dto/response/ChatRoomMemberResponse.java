package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.ChatRoomMember;
import lombok.Builder;
import lombok.Value;

import java.time.OffsetDateTime;
import java.util.UUID;

@Value
@Builder
public class ChatRoomMemberResponse {
    UUID userId;
    String username;
    UUID personId;
    ChatRoomMember.ChatMemberRole role;
    OffsetDateTime joinedAt;
    OffsetDateTime lastReadAt;
    Boolean isMuted;
    String nickname;
}