package com.legacymap.backend.service;

import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserRepository;
import jakarta.mail.MessagingException;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class PasswordResetService {

    private final UserRepository userRepository;
    private final AuthTokenRepository authTokenRepository;
    private final PasswordEncoder passwordEncoder;
    private final PasswordResetEmailService passwordResetEmailService;

    public ApiResponse<Void> requestReset(String email, String resetPageBaseUrl) {
        Optional<User> userOpt = userRepository.findByEmail(email);
        if (userOpt.isEmpty()) {
            return ApiResponse.error(ErrorCode.USER_NOT_FOUND, "Email không tồn tại");
        }

        User user = userOpt.get();

        // Create password reset token
        AuthToken token = AuthToken.builder()
                .user(user)
                .type("password_reset")
                .token(java.util.UUID.randomUUID().toString().replace("-", ""))
                .expiresAt(OffsetDateTime.now().plusMinutes(30))
                .used(false)
                .build();
        authTokenRepository.save(token);

        String resetUrl = resetPageBaseUrl + (resetPageBaseUrl.contains("?") ? "&" : "?") + "token=" + token.getToken();
        try {
            passwordResetEmailService.sendResetEmail(user.getEmail(), user.getUsername(), resetUrl);
        } catch (MessagingException e) {
            return ApiResponse.error(ErrorCode.SEND_EMAIL_FAILED, "Gửi email thất bại");
        }

        return ApiResponse.success(null, "Nếu email tồn tại, liên kết đặt lại đã được gửi");
    }

    @Transactional
    public ApiResponse<Void> resetPassword(String token, String newPassword) {
        Optional<AuthToken> tokenOpt = authTokenRepository.findByTokenAndType(token, "password_reset");
        if (tokenOpt.isEmpty()) {
            return ApiResponse.error(ErrorCode.INVALID_TOKEN, "Token không hợp lệ");
        }

        AuthToken authToken = tokenOpt.get();
        if (Boolean.TRUE.equals(authToken.getUsed())) {
            return ApiResponse.error(ErrorCode.TOKEN_ALREADY_USED, "Token đã được sử dụng");
        }
        if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
            return ApiResponse.error(ErrorCode.TOKEN_EXPIRED, "Token đã hết hạn");
        }

        User user = authToken.getUser();
        user.setPasswordHash(passwordEncoder.encode(newPassword));
        userRepository.save(user);

        user.setIsActive(true);
        user.setFailedAttempts(0);

        authToken.setUsed(true);
        authTokenRepository.save(authToken);

        return ApiResponse.success(null, "Đổi mật khẩu thành công");
    }
}
