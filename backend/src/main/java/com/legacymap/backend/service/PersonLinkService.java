package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.PersonLinkInviteRequest;
import com.legacymap.backend.dto.response.PersonClaimResponse;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.PersonUserLink;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.PersonUserLinkRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class PersonLinkService {

    private final PersonRepository personRepository;
    private final UserRepository userRepository;
    private final PersonUserLinkRepository personUserLinkRepository;
    private final NotificationService notificationService;
    private final EmailService emailService;

    private Person loadPersonOrThrow(UUID personId) {
        return personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
    }

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    private void assertOwnerOfTree(UUID inviterId, Person person) {
        UUID ownerId = person.getFamilyTree().getCreatedBy().getId();
        if (!ownerId.equals(inviterId)) throw new AppException(ErrorCode.UNAUTHORIZED);
    }

    @Transactional
    public void inviteByEmail(UUID inviterId, UUID personId, PersonLinkInviteRequest req) {
        Person person = loadPersonOrThrow(personId);
        assertOwnerOfTree(inviterId, person);

        String email = req.getEmail() != null ? req.getEmail().trim().toLowerCase() : null;
        if (email == null || email.isBlank()) throw new AppException(ErrorCode.VALIDATION_FAILED);

        // Enforce unique email within the same family tree
        String currentEmail = person.getEmail() != null ? person.getEmail().trim().toLowerCase() : null;
        boolean sameAsCurrent = currentEmail != null && currentEmail.equals(email);
        if (!sameAsCurrent) {
            boolean existsInTree = personRepository.existsByFamilyTree_IdAndEmailIgnoreCase(person.getFamilyTree().getId(), email);
            if (existsInTree) {
                throw new AppException(ErrorCode.PERSON_EMAIL_EXISTS_IN_TREE);
            }
        }

        // Auto-assign person's email if empty
        if (currentEmail == null || currentEmail.isBlank()) {
            person.setEmail(email);
            personRepository.save(person);
        }

        // Nếu user đã tồn tại theo email → tạo link pending + thông báo + email
        Optional<User> targetUserOpt = userRepository.findByEmail(email);
        if (targetUserOpt.isPresent()) {
            User target = targetUserOpt.get();

            // Ràng buộc: chỉ 1 self verified mỗi person
            if (personUserLinkRepository.existsByPerson_IdAndLinkTypeAndVerifiedIsTrue(person.getId(), PersonUserLink.LinkType.self))
                throw new AppException(ErrorCode.VALIDATION_FAILED);

            // Upsert link pending
            PersonUserLink link = personUserLinkRepository
                    .findByPerson_IdAndUser_Id(person.getId(), target.getId())
                    .orElseGet(() -> PersonUserLink.builder()
                            .id(new com.legacymap.backend.entity.PersonUserLinkId(person.getId(), target.getId()))
                            .person(person)
                            .user(target)
                            .linkType(PersonUserLink.LinkType.self)
                            .verified(false)
                            .linkedAt(OffsetDateTime.now(ZoneOffset.UTC))
                            .build());

            link.setLinkType(PersonUserLink.LinkType.self);
            link.setVerified(false);
            link.setVerifiedAt(null);
            personUserLinkRepository.save(link);

            // Thông báo cho user được mời
            notificationService.sendSystemNotification(target.getId(),
                    "Lời mời liên kết hồ sơ",
                    "Bạn được mời xác nhận liên kết với hồ sơ " + person.getFullName());

            // Gửi email mời (trang trọng, thân thiện)
            try {
                User inviter = loadUserOrThrow(inviterId);
                emailService.sendPersonInviteEmail(email,
                        inviter.getUsername(),
                        person.getFullName());
            } catch (Exception e) {
                log.warn("Send invite email failed: {}", e.getMessage());
            }
            return;
        }

        // Nếu email CHƯA có tài khoản → chỉ gửi CTA đăng ký/đăng nhập
        try {
            User inviter = loadUserOrThrow(inviterId);
            emailService.sendPersonInviteEmail(email,
                    inviter.getUsername(),
                    person.getFullName());
        } catch (Exception e) {
            log.warn("Send invite email failed: {}", e.getMessage());
        }
    }

    @Transactional
    public void acceptClaim(UUID currentUserId, UUID personId) {
        Person person = loadPersonOrThrow(personId);

        // Ràng buộc self duy nhất đã xác thực
        if (personUserLinkRepository.existsByPerson_IdAndLinkTypeAndVerifiedIsTrue(person.getId(), PersonUserLink.LinkType.self))
            throw new AppException(ErrorCode.VALIDATION_FAILED);

        // Nếu đã có pending link cho user này → xác nhận
        Optional<PersonUserLink> linkOpt = personUserLinkRepository
                .findByPerson_IdAndUser_Id(person.getId(), currentUserId);

        PersonUserLink link = linkOpt.orElseGet(() -> {
            // Nếu không có pending, cho phép tạo mới nếu person.email == user.email
            User me = loadUserOrThrow(currentUserId);
            String personEmail = person.getEmail() != null ? person.getEmail().trim().toLowerCase() : null;
            if (personEmail == null || !personEmail.equals(me.getEmail().trim().toLowerCase())) {
                throw new AppException(ErrorCode.UNAUTHORIZED);
            }
            return PersonUserLink.builder()
                    .id(new com.legacymap.backend.entity.PersonUserLinkId(person.getId(), me.getId()))
                    .person(person)
                    .user(me)
                    .linkType(PersonUserLink.LinkType.self)
                    .linkedAt(OffsetDateTime.now(ZoneOffset.UTC))
                    .build();
        });

        link.setLinkType(PersonUserLink.LinkType.self);
        link.setVerified(true);
        link.setVerifiedAt(OffsetDateTime.now(ZoneOffset.UTC));
        personUserLinkRepository.save(link);

        // Thông báo cho chủ cây
        UUID ownerId = person.getFamilyTree().getCreatedBy().getId();
        notificationService.sendSystemNotification(ownerId,
                "Xác nhận liên kết hồ sơ",
                "Người dùng đã xác nhận liên kết với hồ sơ " + person.getFullName());
    }

    @Transactional
    public void rejectClaim(UUID currentUserId, UUID personId) {
        Person person = loadPersonOrThrow(personId);
        personUserLinkRepository.findByPerson_IdAndUser_Id(person.getId(), currentUserId)
                .ifPresent(personUserLinkRepository::delete);

        UUID ownerId = person.getFamilyTree().getCreatedBy().getId();
        notificationService.sendSystemNotification(ownerId,
                "Từ chối liên kết hồ sơ",
                "Người dùng đã từ chối liên kết với hồ sơ " + person.getFullName());
    }

    @Transactional(readOnly = true)
    public List<PersonClaimResponse> listPendingClaims(UUID userId) {
        User user = loadUserOrThrow(userId);
        return personUserLinkRepository.findByUser_IdAndVerifiedIsFalse(user.getId())
                .stream()
                .map(link -> PersonClaimResponse.builder()
                        .personId(link.getPerson().getId())
                        .personFullName(link.getPerson().getFullName())
                        .familyTreeId(link.getPerson().getFamilyTree().getId())
                        .linkType(link.getLinkType().name())
                        .invitedAt(link.getLinkedAt())
                        .build())
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public boolean isSelfVerified(UUID personId) {
        Person person = loadPersonOrThrow(personId);
        return personUserLinkRepository.existsByPerson_IdAndLinkTypeAndVerifiedIsTrue(
                person.getId(), PersonUserLink.LinkType.self
        );
    }

    @Transactional
    public void unlink(UUID requesterId, UUID personId, UUID userId) {
        Person person = loadPersonOrThrow(personId);
        User user = loadUserOrThrow(userId);

        boolean requesterIsOwner = person.getFamilyTree().getCreatedBy().getId().equals(requesterId);
        boolean requesterIsSelf = userId.equals(requesterId);
        if (!requesterIsOwner && !requesterIsSelf) throw new AppException(ErrorCode.UNAUTHORIZED);

        personUserLinkRepository.findByPerson_IdAndUser_Id(person.getId(), user.getId())
                .ifPresent(personUserLinkRepository::delete);

        if (!requesterIsSelf) {
            notificationService.sendSystemNotification(user.getId(),
                    "Hủy liên kết hồ sơ",
                    "Liên kết của bạn với hồ sơ " + person.getFullName() + " đã được gỡ");
        }
    }
}
