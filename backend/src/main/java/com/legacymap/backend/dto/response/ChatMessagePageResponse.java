package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Value;

import java.util.List;

@Value
@Builder
public class ChatMessagePageResponse {
    List<ChatMessageResponse> messages;
    boolean hasMore;
}

