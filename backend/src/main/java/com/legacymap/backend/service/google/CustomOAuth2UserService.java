package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.repository.UserProfileRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final UserRepository userRepository;
    private final UserProfileRepository userProfileRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    @Transactional
    public OAuth2User loadUser(OAuth2UserRequest req) {
        OAuth2User oauth = super.loadUser(req);

        String email = Optional.ofNullable(oauth.getAttribute("email"))
                .map(Object::toString)
                .orElseThrow(() -> new IllegalArgumentException("Google account has no email"));
        String name = Optional.ofNullable(oauth.getAttribute("name")).map(Object::toString).orElse("User");
        String picture = Optional.ofNullable(oauth.getAttribute("picture")).map(Object::toString).orElse(null);

        log.info("Processing OAuth2 login for email: {}", email);

        User user = userRepository.findByEmail(email).orElse(null);

        if (user == null) {
            log.info("Creating new user for OAuth2 email: {}", email);
            String base = email.substring(0, email.indexOf('@')).replaceAll("[^a-zA-Z0-9._-]", "");
            String username = suggestUniqueUsername(base);

            user = User.builder()
                    .email(email)
                    .username(username)
                    .passwordHash(passwordEncoder.encode(UUID.randomUUID().toString()))
                    .roleName("user")
                    .isActive(true)
                    .isVerified(true)
                    .provider("google")
                    .build();

            user = userRepository.save(user);

            UserProfile profile = UserProfile.builder()
                    .user(user)
                    .fullName(name)
                    .avatarUrl(picture)
                    .build();
            userProfileRepository.save(profile);

            log.info("Created new user with ID: {}", user.getId());

        } else {
            log.info("Existing user found for OAuth2 email: {}", email);
            UserProfile profile = userProfileRepository.findByUserId(user.getId()).orElse(null);
            if (profile == null) {
                profile = UserProfile.builder()
                        .user(user)
                        .fullName(name)
                        .avatarUrl(picture)
                        .build();
                userProfileRepository.save(profile);
                log.info("Created new profile for existing user");
            } else {
                boolean changed = false;
                if (isBlank(profile.getFullName()) && !isBlank(name)) {
                    profile.setFullName(name);
                    changed = true;
                    log.debug("Updated full name for user: {}", user.getId());
                }
                if (isBlank(profile.getAvatarUrl()) && !isBlank(picture)) {
                    profile.setAvatarUrl(picture);
                    changed = true;
                    log.debug("Updated avatar URL for user: {}", user.getId());
                }
                if (changed) {
                    userProfileRepository.save(profile);
                    log.info("Updated profile for user: {}", user.getId());
                }
            }

            // Check if user is banned or inactive
            if (Boolean.TRUE.equals(user.getIsBanned())) {
                log.warn("Banned user attempted OAuth2 login: {}", user.getId());
                throw new IllegalStateException("Account is banned");
            }
            if (Boolean.FALSE.equals(user.getIsActive())) {
                log.warn("Inactive user attempted OAuth2 login: {}", user.getId());
                throw new IllegalStateException("Account is inactive");
            }
        }

        Map<String, Object> attrs = new HashMap<>(oauth.getAttributes());
        attrs.put("dbUserId", user.getId().toString());

        log.info("OAuth2 authentication successful for user: {}", user.getId());

        return new DefaultOAuth2User(
                Collections.singletonList(new SimpleGrantedAuthority("ROLE_" + user.getRoleName().toUpperCase())),
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
        log.debug("Generated unique username: {}", candidate);
        return candidate;
    }

    private boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }
}