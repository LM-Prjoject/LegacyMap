package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.repository.UserProfileRepository;
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
import org.springframework.transaction.annotation.Transactional;

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
            user.setProvider("google");
            user = userRepository.save(user);

            UserProfile profile = new UserProfile();
            profile.setUser(user);
            profile.setFullName(name);
            profile.setAvatarUrl(picture);
            userProfileRepository.save(profile);

        } else {
            UserProfile profile = userProfileRepository.findById(user.getId()).orElse(null);
            if (profile == null) {
                profile = new UserProfile();
                profile.setUser(user);
            }
            boolean changed = false;
            if (isBlank(profile.getFullName()) && !isBlank(name)) {
                profile.setFullName(name);
                changed = true;
            }
            if (isBlank(profile.getAvatarUrl()) && !isBlank(picture)) {
                profile.setAvatarUrl(picture);
                changed = true;
            }
            if (changed || profile.getUserId() == null) {
                userProfileRepository.save(profile);
            }

             if (Boolean.TRUE.equals(user.getIsBanned()) || Boolean.FALSE.equals(user.getIsActive())) {
                 throw new IllegalStateException("Account is disabled or banned");
             }
        }

        Map<String, Object> attrs = new HashMap<>(oauth.getAttributes());
        attrs.put("dbUserId", user.getId().toString()); // nếu front cần

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
        return candidate;
    }

    private boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }
}