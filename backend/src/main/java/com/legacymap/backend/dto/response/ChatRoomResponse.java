package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.ChatRoom;
import lombok.Builder;
import lombok.Value;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Value
@Builder
public class ChatRoomResponse {
    UUID id;
    String name;
    String description;
    ChatRoom.ChatRoomType roomType;
    Boolean active;
    UUID familyTreeId;
    OffsetDateTime createdAt;
    OffsetDateTime updatedAt;
    List<ChatRoomMemberResponse> members;
}

