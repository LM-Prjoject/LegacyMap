package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.AuthenticationService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;

@Slf4j
@Component
@RequiredArgsConstructor
public class OAuth2SuccessHandler implements AuthenticationSuccessHandler {

    private final UserRepository userRepository;
    private final AuthenticationService authenticationService;

    @Value("${app.frontend.url:http://localhost:3000}")
    private String frontendUrl;

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request,
                                        HttpServletResponse response,
                                        Authentication authentication) throws IOException {

        String baseRedirectUrl = frontendUrl + "/signin";

        try {
            log.info("OAuth2 authentication success handler started");

            // 1. Lấy email từ OAuth2User
            OAuth2User oauthUser = (OAuth2User) authentication.getPrincipal();
            String email = oauthUser.getAttribute("email");

            log.info("Google email: {}", email);

            if (email == null || email.trim().isEmpty()) {
                log.error("Missing email from Google OAuth");
                String redirectUrl = baseRedirectUrl + "?error=" + URLEncoder.encode("missing_email", StandardCharsets.UTF_8);
                response.sendRedirect(redirectUrl);
                return;
            }

            // 2. CHECK BAN TRƯỚC - Quan trọng nhất
            log.info("Checking if email is banned: {}", email);
            if (isEmailBanned(email)) {
                log.warn("Email {} is BANNED - blocking login", email);
                String redirectUrl = baseRedirectUrl + "?error=" + URLEncoder.encode("banned", StandardCharsets.UTF_8);
                response.sendRedirect(redirectUrl);
                return;
            }

            log.info("Email {} is NOT banned - proceeding", email);

            // 3. Tìm user trong database
            Optional<User> userOpt = userRepository.findByEmail(email);

            if (userOpt.isEmpty()) {
                log.error("User not found in database: {}", email);
                String redirectUrl = baseRedirectUrl + "?error=" + URLEncoder.encode("user_not_found", StandardCharsets.UTF_8);
                response.sendRedirect(redirectUrl);
                return;
            }

            User user = userOpt.get();
            log.info("User found: ID={}, Provider={}, isBanned={}",
                    user.getId(), user.getProvider(), user.getIsBanned());

            // 4. Double check ban status (safety)
            if (Boolean.TRUE.equals(user.getIsBanned())) {
                log.warn("User {} is banned (double check)", email);
                String redirectUrl = baseRedirectUrl + "?error=" + URLEncoder.encode("banned", StandardCharsets.UTF_8);
                response.sendRedirect(redirectUrl);
                return;
            }

            // 5. Update last login
            user.setLastLogin(OffsetDateTime.now());
            userRepository.save(user);
            log.info("Updated last login for user: {}", email);

            // 6. Generate JWT token
            String jwt = authenticationService.generateAccessToken(user);
            log.info("Generated JWT token for user: {}", email);

            // 7. Redirect với token và hỗ trợ returnUrl
            String targetUrl = frontendUrl + "/auth/google-success?token=" + URLEncoder.encode(jwt, StandardCharsets.UTF_8);

            // ✅ THÊM: Lấy returnUrl từ session nếu có
            String returnUrl = (String) request.getSession().getAttribute("returnUrl");
            if (returnUrl != null) {
                request.getSession().removeAttribute("returnUrl"); // Clear sau khi lấy

                // Validate returnUrl
                if (returnUrl.startsWith("/") && !returnUrl.contains("//")) {
                    targetUrl += "&returnUrl=" + URLEncoder.encode(returnUrl, StandardCharsets.UTF_8);
                    log.info("Added returnUrl from session: {}", returnUrl);
                } else {
                    log.warn("Invalid returnUrl detected in session: {}", returnUrl);
                }
            }

            // ✅ THÊM: Lấy returnUrl từ request parameter (nếu không có trong session)
            String paramReturnUrl = request.getParameter("returnUrl");
            if (paramReturnUrl != null && !paramReturnUrl.isEmpty() && returnUrl == null) {
                // ✅ Validate returnUrl cơ bản (chỉ cho phép relative URLs)
                if (paramReturnUrl.startsWith("/") && !paramReturnUrl.contains("//")) {
                    targetUrl += "&returnUrl=" + URLEncoder.encode(paramReturnUrl, StandardCharsets.UTF_8);
                    log.info("Added returnUrl from request parameter: {}", paramReturnUrl);
                } else {
                    log.warn("Invalid returnUrl detected in parameter: {}", paramReturnUrl);
                }
            }

            log.info("Redirecting to: {}", targetUrl.replace(jwt, "JWT_TOKEN_REDACTED")); // Security log
            response.sendRedirect(targetUrl);

        } catch (Exception e) {
            log.error("CRITICAL ERROR in OAuth2SuccessHandler: {}", e.getMessage(), e);
            log.error("Exception type: {}", e.getClass().getName());

            // Redirect về signin với error message
            String redirectUrl = baseRedirectUrl + "?error=" + URLEncoder.encode("auth_failed", StandardCharsets.UTF_8);

            try {
                response.sendRedirect(redirectUrl);
            } catch (Exception redirectError) {
                log.error("Failed to redirect after error: {}", redirectError.getMessage());
                // Fallback: ghi response trực tiếp
                response.setStatus(HttpServletResponse.SC_FOUND);
                response.setHeader("Location", redirectUrl);
            }
        }
    }

    /**
     * Kiểm tra xem email có bị ban không
     * Check TẤT CẢ accounts có cùng email
     */
    private boolean isEmailBanned(String email) {
        try {
            // SỬA: Sử dụng method đúng từ repository
            List<User> allAccountsWithEmail = userRepository.findAllByEmail(email);

            log.info("Found {} account(s) with email: {}", allAccountsWithEmail.size(), email);

            // Log chi tiết từng account
            for (User user : allAccountsWithEmail) {
                log.info("   - Account: ID={}, Provider={}, isBanned={}, bannedAt={}",
                        user.getId(), user.getProvider(), user.getIsBanned(), user.getBannedAt());
            }

            // Nếu CÓ BẤT KỲ account nào bị ban → email bị ban
            boolean isBanned = allAccountsWithEmail.stream()
                    .anyMatch(user -> Boolean.TRUE.equals(user.getIsBanned()));

            log.info("Email {} ban status: {}", email, isBanned);

            return isBanned;

        } catch (Exception e) {
            log.error("Error checking ban status for email {}: {}", email, e.getMessage());
            // Nếu có lỗi, coi như không bị ban để không block user nhầm
            return false;
        }
    }
}