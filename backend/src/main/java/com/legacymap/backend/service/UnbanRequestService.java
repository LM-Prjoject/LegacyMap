package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.NotificationCreateRequest;
import com.legacymap.backend.entity.Notification;
import com.legacymap.backend.entity.UnbanRequest;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.enums.UnbanRequestStatus;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.UnbanRequestRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class UnbanRequestService {

    private final UserRepository userRepository;
    private final UnbanRequestRepository unbanRequestRepository;
    private final EmailService emailService;
    private final NotificationService notificationService;
    private final AuditLogService auditLogService;

    @Transactional
    public void createRequest(String identifier, String reason) {
        Optional<User> userOpt = userRepository.findByEmail(identifier);
        if (userOpt.isEmpty()) {
            userOpt = userRepository.findByUsername(identifier);
        }
        if (userOpt.isEmpty()) {
            throw new AppException(ErrorCode.USER_NOT_FOUND);
        }

        User user = userOpt.get();
        if (!Boolean.TRUE.equals(user.getIsBanned())) {
            throw new AppException(ErrorCode.BAD_REQUEST, "Tài khoản này hiện không bị khóa.");
        }

        unbanRequestRepository.findFirstByUserAndStatusOrderByCreatedAtDesc(
                user, UnbanRequestStatus.PENDING
        ).ifPresent(r -> {
            throw new AppException(ErrorCode.BAD_REQUEST, "Bạn đã gửi yêu cầu mở khóa, vui lòng chờ xử lý.");
        });

        UnbanRequest req = UnbanRequest.builder()
                .user(user)
                .identifier(identifier)
                .reason(reason)
                .status(UnbanRequestStatus.PENDING)
                .build();

        unbanRequestRepository.save(req);
        notifyAdminsAboutUnbanRequest(req, user);

        auditLogService.log(
                user.getId(),
                null, // treeId - không có tree cho unban request
                "USER",
                user.getId(),
                "UNBAN_REQUEST",
                null,
                Map.of("reason", reason, "identifier", identifier)
        );
    }

    private void notifyAdminsAboutUnbanRequest(UnbanRequest req, User requester) {
        var admins = userRepository.findByRoleName("admin");
        for (User admin : admins) {
            NotificationCreateRequest nReq = new NotificationCreateRequest();
            nReq.setTitle("Yêu cầu mở khóa tài khoản");
            nReq.setMessage(String.format(
                    "Người dùng %s (%s) đã gửi yêu cầu mở khóa tài khoản.",
                    requester.getUsername(),
                    requester.getEmail()
            ));
            nReq.setType(Notification.NotificationType.system);
            String reason = req.getReason() != null ? req.getReason() : "";
            nReq.setRelatedEntity(Map.of(
                    "type", "unban_request",
                    "id", req.getId().toString(),
                    "userId", requester.getId().toString(),
                    "userEmail", requester.getEmail(),
                    "reason", reason
            ));

            notificationService.createNotification(admin.getId(), nReq);
        }
    }

    private User getAdmin() {
        return userRepository.findByRoleName("admin").get(0);
    }

    @Transactional
    public void approveRequest(UUID requestId) {
        UnbanRequest req = unbanRequestRepository.findById(requestId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));

        if (req.getStatus() != UnbanRequestStatus.PENDING) {
            throw new AppException(ErrorCode.BAD_REQUEST, "Yêu cầu đã được xử lý.");
        }

        User admin = getAdmin();
        User user = req.getUser();

        req.setStatus(UnbanRequestStatus.APPROVED);
        req.setProcessedAt(OffsetDateTime.now());
        req.setProcessedBy(admin);
        unbanRequestRepository.save(req);

        user.setIsBanned(false);
        user.setIsActive(true);
        user.setFailedAttempts(0);
        user.setLockUntil(null);
        user.setBannedAt(null);
        userRepository.save(user);

        if (user.getEmail() != null) {
            String subject = "Yêu cầu mở khóa tài khoản của bạn đã thành công";
            String body = """
                    Xin chào,
                    
                    Yêu cầu mở khóa tài khoản của bạn đã được CHẤP NHẬN.
                    Tài khoản đã được mở khóa, bạn có thể đăng nhập lại tại:
                    
                    https://legacy-map-ebon.vercel.app/
                    
                    Trân trọng,
                    Đội ngũ LegacyMap
                    """;
            emailService.sendEmail(user.getEmail(), subject, body);
        }
    }

    @Transactional
    public void denyRequest(UUID requestId) {
        UnbanRequest req = unbanRequestRepository.findById(requestId)
                .orElseThrow(() -> new AppException(ErrorCode.NOT_FOUND));

        if (req.getStatus() != UnbanRequestStatus.PENDING) {
            throw new AppException(ErrorCode.BAD_REQUEST, "Yêu cầu đã được xử lý.");
        }

        User admin = getAdmin();
        User user = req.getUser();

        req.setStatus(UnbanRequestStatus.DENIED);
        req.setProcessedAt(OffsetDateTime.now());
        req.setProcessedBy(admin);
        unbanRequestRepository.save(req);

        if (user.getEmail() != null) {
            String subject = "Yêu cầu mở khóa tài khoản của bạn không thành công";
            String body = """
                    Xin chào,

                    Rất tiếc, yêu cầu mở khóa tài khoản của bạn không được chấp nhận.

                    Nếu bạn cho rằng đây là nhầm lẫn, vui lòng phản hồi email này để được hỗ trợ thêm.

                    Trân trọng,
                    Đội ngũ LegacyMap
                    """;
            emailService.sendEmail(user.getEmail(), subject, body);
        }
    }
}
