package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.AuthenticationService;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.time.Duration;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class OAuth2SuccessHandler implements AuthenticationSuccessHandler {
    @Autowired
    UserRepository userRepository;
    @Autowired
    AuthenticationService authenticationService;
    @Value("${app.frontend.url:http://localhost:5173}")
    private String frontendUrl;

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
                                        Authentication authentication) throws IOException, ServletException {

        OAuth2User oauthUser = (OAuth2User) authentication.getPrincipal();
        String email = oauthUser.getAttribute("email");
        Boolean first = Optional.ofNullable((Boolean) oauthUser.getAttribute("firstLogin")).orElse(false);

        Optional<User> userOpt = userRepository.findByEmail(email);
        if (userOpt.isEmpty()) {
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "User not found");
            return;
        }

        User user = userOpt.get();
        user.setLastLogin(java.time.OffsetDateTime.now());
        userRepository.save(user);

        if (first) {
            String setupToken = authenticationService.generateSetupPasswordToken(user);
            String target = frontendUrl + "/set-password?token=" + setupToken;
            response.sendRedirect(target);
            return;
        }

        String jwt = authenticationService.generateAccessToken(user);

        ResponseCookie cookie = ResponseCookie.from("accessToken", jwt)
                .httpOnly(true)
                .secure(false)
                .path("/")
                .maxAge(Duration.ofDays(7))
                .sameSite("Lax")
                .build();

        response.addHeader(HttpHeaders.SET_COOKIE, cookie.toString());
        response.sendRedirect(frontendUrl + "/");
    }
}