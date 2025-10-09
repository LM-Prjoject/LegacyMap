package com.legacymap.backend.family.service;

import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.family.dto.request.UserCreateRequest;
import com.legacymap.backend.family.entity.User;
import com.legacymap.backend.family.entity.UserProfile;
import com.legacymap.backend.family.entity.AuthToken;
import com.legacymap.backend.family.repository.UserProfileRepository;
import com.legacymap.backend.family.repository.UserRepository;
import com.legacymap.backend.family.repository.AuthTokenRepository;
import jakarta.mail.MessagingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.UUID;

@Service
public class UserService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserProfileRepository userProfileRepository;

    @Autowired
    private AuthTokenRepository authTokenRepository;

    @Autowired
    private EmailService emailService;

    @Transactional
    public User createRequest(UserCreateRequest request) {

        if (userRepository.existsByUsername(request.getUsername())
                || userRepository.findByEmail(request.getEmail()).isPresent()) {
            throw new AppException(ErrorCode.USER_EXISTED);
        }


        PasswordEncoder encoder = new BCryptPasswordEncoder(10);
        User user = new User();
        user.setUsername(request.getUsername().trim());
        user.setEmail(request.getEmail().trim().toLowerCase());
        user.setPasswordHash(encoder.encode(request.getPassword()));
        user.setRoleName("user");
        user.setIsActive(false);
        user.setIsVerified(false);

        user = userRepository.save(user);


        UserProfile profile = new UserProfile();
        profile.setUser(user);
        profile.setFullName(request.getFullName());
        profile.setClanName(request.getClanName());
        profile.setGender(request.getGender());
        profile.setPhone(request.getPhone());
        profile.setDob(request.getDob());
        profile.setAddress(request.getAddress());
        userProfileRepository.save(profile);

        String tokenStr = UUID.randomUUID().toString().replace("-", "");
        AuthToken token = AuthToken.builder()
                .user(user)
                .token(tokenStr)
                .type("email_verification")
                .expiresAt(OffsetDateTime.now().plusHours(24))
                .used(false)
                .build();
        authTokenRepository.save(token);

        try {
            emailService.sendVerificationEmail(user.getEmail(), profile.getFullName(), tokenStr);
        } catch (MessagingException e) {
            throw new AppException(ErrorCode.SEND_EMAIL_FAILED);
        }

        return user;
    }
}
