package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;

@Data
public class TreeShareRequest {

    @Email(message = "Invalid email format")
    private String email; // Email của người được chia sẻ (optional, có thể null nếu share public link)

    @NotBlank(message = "Access level is required")
    @Pattern(regexp = "view|edit|admin", message = "Access level must be: view, edit, or admin")
    private String accessLevel; // "view" hoặc "edit" hoặc "admin"

    private String message; // Optional: tin nhắn kèm theo
}