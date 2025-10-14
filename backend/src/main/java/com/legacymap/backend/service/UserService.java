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


    public User login(String identifier, String rawPassword) {
        Optional<User> userOpt;

        // Nếu identifier chứa dấu '@' thì là email
        if (identifier.contains("@")) {
            userOpt = userRepository.findByEmail(identifier);
        } else {
            userOpt = userRepository.findByUsername(identifier);
        }

        User user = userOpt.orElseThrow(() ->
                new AppException(ErrorCode.USER_NOT_FOUND)
        );

        // kiểm tra trạng thái tài khoản
        if (!Boolean.TRUE.equals(user.getIsVerified())) {
            throw new AppException(ErrorCode.ACCOUNT_NOT_VERIFIED);
        }

        if (!Boolean.TRUE.equals(user.getIsActive())) {
            throw new AppException(ErrorCode.ACCOUNT_DISABLED);
        }

        // kiểm tra password bằng BCrypt
        if (!passwordEncoder.matches(rawPassword, user.getPasswordHash())) {
            throw new AppException(ErrorCode.INVALID_CREDENTIALS);
        }

        // cập nhật last login
        user.setLastLogin(java.time.OffsetDateTime.now());
        userRepository.save(user);

        return user;
    }

    @Transactional
    public User createRequest(UserCreateRequest request) {

        if (userRepository.existsByUsername(request.getUsername())) {
            throw new AppException(ErrorCode.USER_EXISTED);
        }

        if (userRepository.existsByEmail(request.getEmail())) {
            throw new AppException(ErrorCode.EMAIL_EXISTED);
        }

        PasswordEncoder encoder = new BCryptPasswordEncoder(10);
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
}
