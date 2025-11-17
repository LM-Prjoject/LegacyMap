package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class AttachmentUploadResponse {
    String fileUrl;
    String originalName;
    long size;
    String contentType;
    ChatMessageResponse message;
}

