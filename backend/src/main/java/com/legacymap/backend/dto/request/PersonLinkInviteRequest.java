package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class PersonLinkInviteRequest {
    @NotBlank
    @Email
    private String email;

    private String linkType = "self";
}
