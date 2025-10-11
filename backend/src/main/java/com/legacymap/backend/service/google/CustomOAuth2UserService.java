package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public OAuth2User loadUser(OAuth2UserRequest req) {
        OAuth2User oauth = super.loadUser(req);
        String email = oauth.getAttribute("email");
        String name  = Optional.ofNullable(oauth.getAttribute("name")).orElse("User").toString();

        boolean firstLogin = false;
        User user = userRepository.findByEmail(email).orElse(null);

        if (user == null) {
            String base = email.substring(0, email.indexOf('@')).replaceAll("[^a-zA-Z0-9._-]", "");
            String username = suggestUniqueUsername(base);

            user = new User();
            user.setEmail(email);
            user.setUsername(username);
            user.setPasswordHash(passwordEncoder.encode(UUID.randomUUID().toString()));
            user.setRoleName("user");
            user.setIsActive(true);
            user.setIsVerified(true);
            user.setCreatedAt(OffsetDateTime.now());
            user.setUpdatedAt(OffsetDateTime.now());
            userRepository.save(user);
            firstLogin = true;
        }

        Map<String, Object> attrs = new HashMap<>(oauth.getAttributes());
        attrs.put("firstLogin", firstLogin);
        attrs.put("dbUserId", user.getId().toString());

        return new DefaultOAuth2User(
                Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")),
                attrs,
                "email"
        );
    }

    private String suggestUniqueUsername(String base) {
        String candidate = base;
        int i = 0;
        while (userRepository.existsByUsername(candidate)) {
            i++;
            candidate = base + i;
        }
        return candidate;
    }
}