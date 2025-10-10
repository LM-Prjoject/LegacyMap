package com.legacymap.backend.dto.request;

import lombok.*;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
@Getter
@Setter
public class LoginRequest {
    private String identifier; // có thể là email hoặc username
    private String password;
}
