package com.legacymap.backend.service.google;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.DisabledException;
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

@Slf4j
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

        log.info("Google OIDC login for email: {}", email);

        // CHECK BAN TRƯỚC - QUAN TRỌNG NHẤT
        if (isEmailBanned(email)) {
            log.warn("BLOCKED: Email {} is banned", email);
            throw new DisabledException("BANNED: Account is banned");
        }

        User user = userRepository.findByEmail(email).orElse(null);

        if (user == null) {
            log.info("Creating new Google user: {}", email);
            String base = email.substring(0, email.indexOf('@')).replaceAll("[^a-zA-Z0-9._-]", "");
            String username = suggestUniqueUsername(base);

            // SỬA: Sử dụng Builder pattern thay vì setter trực tiếp
            user = User.builder()
                    .email(email)
                    .username(username)
                    .passwordHash(passwordEncoder.encode(UUID.randomUUID().toString()))
                    .roleName("user")
                    .isActive(true)
                    .isVerified(true)
                    .isBanned(false)
                    .provider("google")
                    .build();

            user = userRepository.save(user);

            // SỬA: Sử dụng Builder pattern cho UserProfile
            UserProfile profile = UserProfile.builder()
                    .user(user)
                    .fullName(name)
                    .avatarUrl(picture)
                    .build();
            userProfileRepository.save(profile);

            log.info("Created new Google user with ID: {}", user.getId());

        } else {
            log.info("Existing user found: {}", email);

            UserProfile profile = userProfileRepository.findByUserId(user.getId()).orElse(null);
            if (profile == null) {
                profile = UserProfile.builder()
                        .user(user)
                        .fullName(name)
                        .avatarUrl(picture)
                        .build();
                userProfileRepository.save(profile);
                log.info("Created new profile for existing user: {}", user.getId());
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

            // FIXED: Throw Spring Security exceptions
            if (Boolean.TRUE.equals(user.getIsBanned())) {
                log.warn("User {} is banned", email);
                throw new DisabledException("BANNED: Account is banned");
            }

            if (Boolean.FALSE.equals(user.getIsActive())) {
                log.warn("User {} is disabled", email);
                throw new DisabledException("DISABLED: Account is disabled");
            }
        }

        List<SimpleGrantedAuthority> authorities =
                List.of(new SimpleGrantedAuthority("ROLE_" + user.getRoleName().toUpperCase()));

        log.info("OIDC authentication successful for user: {}", user.getId());

        return new DefaultOidcUser(authorities, oidc.getIdToken(), oidc.getUserInfo(), "email");
    }

    /**
     * Kiểm tra email có bị ban không (check tất cả accounts)
     */
    private boolean isEmailBanned(String email) {
        try {
            Optional<User> user = userRepository.findByEmail(email);

            if (user.isPresent()) {
                boolean isBanned = Boolean.TRUE.equals(user.get().getIsBanned());
                if (isBanned) {
                    log.warn("Email {} has banned account(s)", email);
                }
                return isBanned;
            }

            return false;

        } catch (Exception e) {
            log.error("Error checking ban status: {}", e.getMessage());
            return false;
        }
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