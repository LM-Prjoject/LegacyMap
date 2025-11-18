package com.legacymap.backend.dto.response;

import lombok.Builder;
import lombok.Value;

import java.util.UUID;

@Value
@Builder
public class UserSearchResponse {
    UUID id;
    String email;
    String username;
    String fullName;
    String phone;
    String avatarUrl;
}

