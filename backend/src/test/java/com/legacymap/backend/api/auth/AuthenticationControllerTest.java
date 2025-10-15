package com.legacymap.backend.api.auth;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.controller.auth.AuthController;
import com.legacymap.backend.dto.request.AuthenticationRequest;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
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
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.web.servlet.MockMvc;

// ✅ NEW imports
import org.springframework.test.context.bean.override.mockito.MockitoBean;

//import java.util.Optional;
//import java.util.UUID;
//
//import static org.mockito.ArgumentMatchers.any;
//import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


//kiểm tra đầu vào HTTP → đầu ra JSON của AuthController khi đăng nhập thành công
@WebMvcTest(controllers = AuthController.class)
@AutoConfigureMockMvc(addFilters = false)
public class AuthenticationControllerTest {

    @Autowired MockMvc mockMvc;
    @Autowired ObjectMapper objectMapper;

    @MockitoBean UserService userService;
    @MockitoBean AuthTokenService authTokenService;
    @MockitoBean UserRepository userRepository;
    @MockitoBean UserProfileRepository userProfileRepository;
    @MockitoBean AuthTokenRepository authTokenRepository;
    @MockitoBean AuthenticationService authenticationService;
    @MockitoBean PasswordEncoder passwordEncoder;

    @Test
    void login_returns200_onValidRequest() throws Exception {
        var userId = java.util.UUID.fromString("1b9516a7-2f5c-4e25-aee2-937d1014dff4");
        var user = new User();
        user.setId(userId);
        user.setEmail("valid@site.com");
        user.setUsername("valid");
        user.setRoleName("USER");
        user.setIsActive(true);
        user.setIsVerified(true);

        AuthToken token = Mockito.mock(AuthToken.class);
        Mockito.when(token.getToken()).thenReturn("session-token-123");


        when(userService.login("valid@site.com", "password")).thenReturn(user);
        when(authTokenService.createSessionToken(user)).thenReturn(token);
        when(userProfileRepository.findById(userId)).thenReturn(java.util.Optional.empty());

        var body = AuthenticationRequest.builder()
                .identifier("valid@site.com")
                .password("password")
                .build();

        mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(body)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.result.user.email").value("valid@site.com"))
                .andExpect(jsonPath("$.result.token").value("session-token-123"));
    }
}
