package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.User;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@AllArgsConstructor
@Data
@RequiredArgsConstructor
public class AuthenticationResponse {
    private User user;
    private String token;
}
