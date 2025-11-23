package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class UpdateMessageRequest {
    @NotBlank(message = "Message text is required")
    private String messageText;
}
