package com.legacymap.backend.dto.request;


import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class ChatRequest {
    @NotBlank(message = "sessionId is required")
    private String sessionId;

    @NotBlank(message = "message is required")
    private String message;
}