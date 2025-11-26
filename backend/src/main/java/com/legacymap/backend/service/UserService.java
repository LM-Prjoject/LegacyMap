package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.UserCreateRequest;
import com.legacymap.backend.dto.response.UserSearchResponse;
import com.legacymap.backend.entity.AuthToken;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.UserProfileRepository;
import com.legacymap.backend.repository.UserRepository;
import jakarta.mail.MessagingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
public class UserService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserProfileRepository userProfileRepository;

    @Autowired
    private AuthTokenService authTokenService;

    @Autowired
    private EmailService emailService;

    @Transactional
    public User createRequest(UserCreateRequest request) {

        if (userRepository.existsByUsername(request.getUsername())) {
            throw new AppException(ErrorCode.USER_EXISTED);
        }

        if (userRepository.existsByEmail(request.getEmail())) {
            throw new AppException(ErrorCode.EMAIL_EXISTED);
        }

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

    @Transactional(readOnly = true)
    public User findByEmail(String email) {
        if (email == null) return null;
        return userRepository.findByEmail(email.trim().toLowerCase()).orElse(null);
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
      
        return userProfileRepository.save(existingProfile);
    }

    @Transactional(readOnly = true)
    public UserProfile getUserProfileOnly(UUID userId) {
        return userProfileRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    @Transactional(readOnly = true)
    public List<UserSearchResponse> searchUsers(String keyword, int limit) {
        if (keyword == null || keyword.trim().length() < 2) {
            return List.of();
        }
        int size = Math.max(1, Math.min(limit, 20));
        Pageable pageable = PageRequest.of(0, size);
        return userRepository.searchUsers(keyword.trim(), pageable)
                .stream()
                .map(user -> {
                    UserProfile profile = userProfileRepository.findById(user.getId()).orElse(null);
                    return UserSearchResponse.builder()
                            .id(user.getId())
                            .email(user.getEmail())
                            .username(user.getUsername())
                            .fullName(profile != null ? profile.getFullName() : null)
                            .phone(profile != null ? profile.getPhone() : null)
                            .avatarUrl(profile != null ? profile.getAvatarUrl() : null)
                            .build();
                })
                .toList();
    }

}
