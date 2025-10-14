package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserRequest;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserService;
import org.springframework.security.oauth2.core.oidc.user.DefaultOidcUser;
import org.springframework.security.oauth2.core.oidc.user.OidcUser;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class CustomOidcUserService extends OidcUserService {

    private final UserRepository userRepository;
    private final UserProfileRepository userProfileRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    @Transactional
    public OidcUser loadUser(OidcUserRequest userRequest) {
        OidcUser oidc = super.loadUser(userRequest);

        String email = Optional.ofNullable(oidc.getEmail())
                .orElseThrow(() -> new IllegalArgumentException("Google account has no email"));
        String name = Optional.ofNullable(oidc.getFullName()).orElse("User");
        String picture = Optional.ofNullable(oidc.getPicture()).orElse(null);

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
            // Nếu tài khoản local đã tồn tại -> không tạo mới, chỉ cập nhật provider thành google
            if (user.getProvider() == null || "local".equalsIgnoreCase(user.getProvider())) {
                user.setProvider("google");
                userRepository.save(user);
            }
            UserProfile profile = userProfileRepository.findById(user.getId()).orElse(null);
            if (profile == null) {
                profile = new UserProfile();
                profile.setUser(user);
            }
            boolean changed = false;
            if (name != null && (profile.getFullName() == null || !name.equals(profile.getFullName()))) {
                profile.setFullName(name);
                changed = true;
            }
            if (picture != null && (profile.getAvatarUrl() == null || !picture.equals(profile.getAvatarUrl()))) {
                profile.setAvatarUrl(picture);
                changed = true;
            }
            if (changed || profile.getUserId() == null) {
                userProfileRepository.save(profile);
            }
        }

        // Build authorities & return DefaultOidcUser using email as name attribute
        List<SimpleGrantedAuthority> authorities = List.of(new SimpleGrantedAuthority("ROLE_" + user.getRoleName().toUpperCase()));
        Map<String, Object> claims = new HashMap<>(oidc.getClaims());
        claims.put("dbUserId", user.getId().toString());
        claims.put("email", user.getEmail());
        claims.put("name", name);
        if (picture != null) claims.put("picture", picture);

        return new DefaultOidcUser(authorities, oidc.getIdToken(), oidc.getUserInfo(), "email");
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
