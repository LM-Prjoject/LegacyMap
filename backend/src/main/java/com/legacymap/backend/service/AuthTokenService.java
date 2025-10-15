package com.legacymap.backend.service;

import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.AuthTokenRepository;
import lombok.Builder;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.concurrent.ThreadLocalRandom;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Builder
public class AuthTokenService {

    private final AuthTokenRepository authTokenRepository;

    public AuthToken createSessionToken(User user) {
        String token = generateRandomToken();
        OffsetDateTime expiresAt = OffsetDateTime.now().plusDays(7);

        AuthToken authToken = AuthToken.builder()
                .user(user)
                .token(token)
                .type("session")
                .expiresAt(expiresAt)
                .used(false)
                .build();

        return authTokenRepository.save(authToken);
    }

    private String generateRandomToken() {
        return UUID.randomUUID().toString().replace("-", "") +
                Long.toHexString(ThreadLocalRandom.current().nextLong());
    }

    public AuthToken createEmailVerificationToken(User user) {
        AuthToken token = AuthToken.builder()
                .user(user)
                .type("email_verification")
                .token(UUID.randomUUID().toString().replace("-", ""))
                .expiresAt(OffsetDateTime.now().plusMinutes(5))
                .used(false)
                .build();
        return authTokenRepository.save(token);
    }

//    //Hàm tiện lợi (convenience) để tạo password reset token với TTL mặc định 30 phút
//    public AuthToken createSetPasswordToken(User user) {
//        return createSetPasswordToken(user, java.time.Duration.ofMinutes(30));
//    }
//    //Tạo password reset token với TTL tùy ý (tham số ttl)
//    public AuthToken createSetPasswordToken(User user, Duration ttl) {
//        AuthToken token = AuthToken.builder()
//                .user(user)
//                .type("password_reset") // khớp với PasswordResetService.resetPassword(...)
//                .token(java.util.UUID.randomUUID().toString().replace("-", ""))
//                .expiresAt(OffsetDateTime.now().plus(ttl))
//                .used(false)
//                .build();
//        return authTokenRepository.save(token);
//    }
}
