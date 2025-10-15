package com.legacymap.backend.api.auth;


import com.legacymap.backend.controller.auth.AuthController;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.repository.AuthTokenRepository;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.AuthTokenService;
import com.legacymap.backend.service.AuthenticationService;
import com.legacymap.backend.service.UserService;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.time.OffsetDateTime;
import java.util.Optional;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


//giả lập token đã hết hạn và kiểm tra redirect đúng về FE với err=token_expired
@WebMvcTest(controllers = AuthController.class)
@AutoConfigureMockMvc(addFilters = false)
public class AuthVerifyControllerTest {

    @Autowired MockMvc mockMvc;

    // Mock toàn bộ dependency mà AuthController @Autowired
    @MockitoBean AuthTokenRepository authTokenRepository;
    @MockitoBean UserService userService;
    @MockitoBean AuthTokenService authTokenService;
    @MockitoBean UserRepository userRepository;
    @MockitoBean UserProfileRepository userProfileRepository;
    @MockitoBean AuthenticationService authenticationService;
    @MockitoBean PasswordEncoder passwordEncoder;

    @Test
    void verifyEmail_withExpiredToken_redirectsToTokenExpired() throws Exception {
        // Giả lập AuthToken hết hạn
        AuthToken expired = Mockito.mock(AuthToken.class);
        when(expired.getExpiresAt()).thenReturn(OffsetDateTime.now().minusMinutes(10));
        when(expired.getUsed()).thenReturn(false);

        when(authTokenRepository.findByTokenAndType("expired-token", "email_verification"))
                .thenReturn(Optional.of(expired));

        // Gọi endpoint
        mockMvc.perform(get("/api/auth/verify").param("token", "expired-token"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("http://localhost:5173?showLogin=1&err=token_expired"));

        // Đảm bảo không kích hoạt luồng "kích hoạt user" khi token đã hết hạn
        verify(userRepository, never()).save(any());
        verify(authTokenRepository, never()).save(any());
    }
}
