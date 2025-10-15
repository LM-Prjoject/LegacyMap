//package com.legacymap.backend.api.auth;
//
//import com.fasterxml.jackson.databind.ObjectMapper;
//import com.legacymap.backend.controller.auth.AuthController;
//import com.legacymap.backend.entity.User;
//import com.legacymap.backend.repository.AuthTokenRepository;
//import com.legacymap.backend.repository.UserProfileRepository;
//import com.legacymap.backend.repository.UserRepository;
//import com.legacymap.backend.service.AuthTokenService;
//import com.legacymap.backend.service.AuthenticationService;
//import com.legacymap.backend.service.UserService;
//import org.junit.jupiter.api.Test;
//import org.mockito.Mockito;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
//import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
//import org.springframework.http.MediaType;
//import org.springframework.security.crypto.password.PasswordEncoder;
//import org.springframework.test.web.servlet.MockMvc;
//import org.springframework.test.context.bean.override.mockito.MockitoBean;
//
//import java.util.Map;
//import java.util.Optional;
//import java.util.UUID;
//
//import static org.mockito.ArgumentMatchers.*;
//import static org.mockito.Mockito.*;
//import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
//import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
//
///**
// * Unit test cho endpoint quên mật khẩu:
// *  POST /api/auth/forgot-password
// *
// * Giả định payload: {"identifier": "<email>"}
// * Nếu code của bạn dùng "email" thì đổi key cho khớp.
// */
//@WebMvcTest(controllers = AuthController.class)
//@AutoConfigureMockMvc(addFilters = false)
//class ForgotPasswordControllerTest {
//
//    @Autowired MockMvc mockMvc;
//    @Autowired ObjectMapper objectMapper;
//
//    // Mock toàn bộ bean mà AuthController @Autowired
//    @MockitoBean AuthTokenRepository authTokenRepository;
//    @MockitoBean UserService userService;
//    @MockitoBean AuthTokenService authTokenService;
//    @MockitoBean UserRepository userRepository;
//    @MockitoBean UserProfileRepository userProfileRepository;
//    @MockitoBean AuthenticationService authenticationService;
//    @MockitoBean PasswordEncoder passwordEncoder;
//
//    @Test
//    void forgotPassword_whenEmailExists_createsResetToken_andReturns200() throws Exception {
//        // Arrange
//        String email = "exists@site.com";
//        UUID userId = UUID.randomUUID();
//
//        User user = new User();
//        user.setId(userId);
//        user.setEmail(email);
//        user.setUsername("someone");
//        user.setIsActive(true);
//        user.setIsVerified(true);
//
//        // Tìm user theo email/identifier tồn tại
//        when(userRepository.findByEmail(email)).thenReturn(Optional.of(user));
//        // Nếu Controller gọi qua service khác (vd UserService.findByIdentifier), bạn stub method đó tương tự
//        // when(userService.findByIdentifier(email)).thenReturn(Optional.of(user));
//
//        // Giả sử controller sẽ gọi service để tạo token đặt lại mật khẩu
//        doNothing().when(authTokenService).createSetPasswordToken(eq(user));
//
//        // Act + Assert
//        mockMvc.perform(
//                        post("/api/auth/forgot-password")
//                                .contentType(MediaType.APPLICATION_JSON)
//                                .content(objectMapper.writeValueAsString(Map.of("identifier", email)))
//                )
//                // Tuỳ code thực tế trả 200 hay 204 — chỉnh lại cho khớp
//                .andExpect(status().isOk());
//
//        // Verify: có tạo token
//        verify(authTokenService, times(1)).createSetPasswordToken(eq(user));
//        // Không được lộ thông tin nhạy cảm trong response (tuỳ bạn có assert body hay không)
//    }
//
//    @Test
//    void forgotPassword_whenEmailNotFound_stillReturns200_andDoesNotCreateToken() throws Exception {
//        // Arrange
//        String email = "unknown@site.com";
//
//        // Không có user
//        when(userRepository.findByEmail(email)).thenReturn(Optional.empty());
//        // when(userService.findByIdentifier(email)).thenReturn(Optional.empty());
//
//        // Act + Assert
//        mockMvc.perform(
//                        post("/api/auth/forgot-password")
//                                .contentType(MediaType.APPLICATION_JSON)
//                                .content(objectMapper.writeValueAsString(Map.of("identifier", email)))
//                )
//                // Nhiều hệ thống trả 200/204 để tránh dò email — chỉnh lại tuỳ thực tế
//                .andExpect(status().isOk());
//
//        // Verify: không tạo token
//        verify(authTokenService, never()).createSetPasswordToken(any(User.class));
//    }
//}
