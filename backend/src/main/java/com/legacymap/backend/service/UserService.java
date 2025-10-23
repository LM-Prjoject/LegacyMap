package com.legacymap.backend.service;

import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.dto.request.UserCreateRequest;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.repository.UserRepository;
import jakarta.mail.MessagingException;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserProfileRepository userProfileRepository;

    @Autowired
    private AuthTokenService authTokenService;

    @Autowired
    private EmailService emailService;

    // ❌ XÓA method login() này - dùng AuthenticationService.login() thay thế
    /*
    public User login(String identifier, String rawPassword) {
        // ... old code
        Optional<User> userOpt = identifier.contains("@")
                ? userRepository.findByEmail(identifier.trim().toLowerCase())
                : userRepository.findByUsername(identifier.trim());

        User user = userOpt.orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        if (!Boolean.TRUE.equals(user.getIsVerified())) {
            throw new AppException(ErrorCode.ACCOUNT_NOT_VERIFIED);
        }

        if (!Boolean.TRUE.equals(user.getIsActive())) {
            throw new AppException(ErrorCode.ACCOUNT_DISABLED);
        }

        if (!passwordEncoder.matches(rawPassword, user.getPasswordHash())) {
            increaseFailedAttempts(user);

            if (!Boolean.TRUE.equals(user.getIsActive())) {
                throw new AppException(ErrorCode.ACCOUNT_DISABLED);
            }

            throw new AppException(ErrorCode.INVALID_CREDENTIALS);
        }

        resetFailedAttempts(user);
        user.setLastLogin(java.time.OffsetDateTime.now());
        userRepository.save(user);

        return user;
    }
    */

    @Transactional
    public User createRequest(UserCreateRequest request) {

        if (userRepository.existsByUsername(request.getUsername())) {
            throw new AppException(ErrorCode.USER_EXISTED);
        }

        if (userRepository.existsByEmail(request.getEmail())) {
            throw new AppException(ErrorCode.EMAIL_EXISTED);
        }

        // ✅ SỬA: Đổi từ strength 10 sang 12 để khớp với SecurityConfig
        PasswordEncoder encoder = new BCryptPasswordEncoder(12);
        User user = new User();
        user.setUsername(request.getUsername().trim());
        user.setEmail(request.getEmail().trim().toLowerCase());
        user.setPasswordHash(encoder.encode(request.getPassword()));
        user.setRoleName("user");
        user.setIsActive(false);
        user.setIsVerified(false);
        user.setProvider("local");

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

        AuthToken verifyToken = authTokenService.createEmailVerificationToken(user);

        try {
            emailService.sendVerificationEmail(user.getEmail(), profile.getFullName(), verifyToken.getToken());
        } catch (MessagingException e) {
            throw new AppException(ErrorCode.SEND_EMAIL_FAILED);
        }

        return user;
    }

    @Transactional(readOnly = true)
    public User getUserById(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    @Transactional
    public UserProfile updateUserProfile(UUID userId, UserProfile updatedProfile) {
        UserProfile existingProfile = userProfileRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        existingProfile.setFullName(updatedProfile.getFullName());
        existingProfile.setClanName(updatedProfile.getClanName());
        existingProfile.setGender(updatedProfile.getGender());
        existingProfile.setPhone(updatedProfile.getPhone());
        existingProfile.setDob(updatedProfile.getDob());
        existingProfile.setAddress(updatedProfile.getAddress());
        existingProfile.setAvatarUrl(updatedProfile.getAvatarUrl());

        if (updatedProfile.getDescription() != null) {
            existingProfile.setDescription(updatedProfile.getDescription());
        }

        return userProfileRepository.save(existingProfile);
    }

    @Transactional(readOnly = true)
    public UserProfile getUserProfileOnly(UUID userId) {
        return userProfileRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    public void increaseFailedAttempts(User user) {
        int newAccess = (user.getFailedAttempts() == null ? 0 : user.getFailedAttempts()) + 1;
        user.setFailedAttempts(newAccess);
        if (newAccess >= 3) {
            user.setIsActive(false);
        }
        userRepository.save(user);
    }

    public void resetFailedAttempts(User user) {
        user.setFailedAttempts(0);
        userRepository.save(user);
    }
}
