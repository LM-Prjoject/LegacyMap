package com.legacymap.backend.dto.response;

import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@AllArgsConstructor
@Data
@RequiredArgsConstructor
public class AuthenticationResponse {
    private Map<String, Object> user;
    private String token;
}
