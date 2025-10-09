package com.legacymap.backend.family.controller.auth;

import com.legacymap.backend.family.entity.AuthToken;
import com.legacymap.backend.family.entity.User;
import com.legacymap.backend.family.repository.AuthTokenRepository;
import com.legacymap.backend.family.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.OffsetDateTime;

@Controller
@RequestMapping("/auth")
public class AuthController {

    @Autowired
    private AuthTokenRepository authTokenRepository;

    @Autowired
    private UserRepository userRepository;

    public AuthController(AuthTokenRepository authTokenRepository, UserRepository userRepository) {
        this.authTokenRepository = authTokenRepository;
        this.userRepository = userRepository;
    }

    @GetMapping("/verify")
    public ResponseEntity<String> verifyEmail(@RequestParam("token") String token) {
        AuthToken authToken = authTokenRepository.findByTokenAndType(token, "email_verification")
                .orElseThrow(() -> new RuntimeException("Invalid token"));

        if (authToken.getExpiresAt().isBefore(OffsetDateTime.now())) {
            return ResponseEntity.badRequest().body("Token expired");
        }

        if (Boolean.TRUE.equals(authToken.getUsed())) {
            return ResponseEntity.badRequest().body("Token already used");
        }

        User user = authToken.getUser();
        user.setIsVerified(true);
        user.setIsActive(true);
        userRepository.save(user);

        authToken.setUsed(true);
        authTokenRepository.save(authToken);

        return ResponseEntity.ok("Email verified successfully!");
    }
}
