package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.dto.request.PersonUpdateRequest;
import com.legacymap.backend.dto.response.TreeShareResponse;
import com.legacymap.backend.dto.response.SharedTreeAccessInfoResponse; // ĐÃ THÊM
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.TreeAccess;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.TreeAccessRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class FamilyTreeService {

    @Autowired
    private FamilyTreeRepository familyTreeRepository;

    @Autowired
    private PersonRepository personRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private TreeAccessRepository treeAccessRepository;

    @Autowired
    private EmailService emailService;

    @Value("${app.frontend.url}")
    private String frontendUrl;

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    @Transactional
    public FamilyTree create(UUID userId, FamilyTreeCreateRequest req) {
        User creator = loadUserOrThrow(userId);
        FamilyTree tree = FamilyTree.builder()
                .name(req.getName())
                .description(req.getDescription())
                .isPublic(req.getIsPublic() != null ? req.getIsPublic() : false)
                .coverImageUrl(req.getCoverImageUrl())
                .createdBy(creator)
                .build();
        return familyTreeRepository.save(tree);
    }

    private FamilyTree findOwnedTreeOrThrow(UUID treeId, UUID userId) {
        User user = loadUserOrThrow(userId);
        return familyTreeRepository.findByIdAndCreatedBy(treeId, user)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
    }

    @Transactional
    public FamilyTree update(UUID treeId, UUID userId, FamilyTreeUpdateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        if (req.getName() != null) tree.setName(req.getName());
        if (req.getDescription() != null) tree.setDescription(req.getDescription());
        if (req.getIsPublic() != null) tree.setIsPublic(req.getIsPublic());
        if (req.getCoverImageUrl() != null) tree.setCoverImageUrl(req.getCoverImageUrl());
        return familyTreeRepository.save(tree);
    }

    @Transactional
    public void delete(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        familyTreeRepository.delete(tree);
    }

    @Transactional
    public Person addMember(UUID treeId, UUID userId, PersonCreateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        User creator = loadUserOrThrow(userId);
        Person p = Person.builder()
                .familyTree(tree)
                .fullName(req.getFullName())
                .gender(req.getGender())
                .birthDate(req.getBirthDate())
                .deathDate(req.getDeathDate())
                .birthPlace(req.getBirthPlace())
                .deathPlace(req.getDeathPlace())
                .biography(req.getBiography())
                .avatarUrl(req.getAvatarUrl())
                .email(req.getEmail())
                .phone(req.getPhone())
                .createdBy(creator)
                .build();
        return personRepository.save(p);
    }

    @Transactional(readOnly = true)
    public List<FamilyTree> listByUser(UUID userId) {
        // ✅ SỬA: Gọi method mới để lấy cả cây sở hữu và cây được chia sẻ
        return getUserTrees(userId);
    }

    @Transactional(readOnly = true)
    public List<Person> listMembers(UUID treeId, UUID userId) {
        FamilyTree tree = familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        boolean hasAccess = tree.getCreatedBy().getId().equals(userId)
                || canView(treeId, userId);

        if (!hasAccess) {
            throw new AppException(ErrorCode.PERMISSION_DENIED);
        }

        return personRepository.findAllByFamilyTree_Id(tree.getId());
    }

    @Transactional
    public Person updateMember(UUID treeId, UUID userId, UUID personId, PersonUpdateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }
        if (req.getFullName() != null) p.setFullName(req.getFullName());
        if (req.getGender() != null) p.setGender(req.getGender());
        if (req.getBirthDate() != null) p.setBirthDate(req.getBirthDate());
        if (req.getDeathDate() != null) p.setDeathDate(req.getDeathDate());
        if (req.getBirthPlace() != null) p.setBirthPlace(req.getBirthPlace());
        if (req.getDeathPlace() != null) p.setDeathPlace(req.getDeathPlace());
        if (req.getBiography() != null) p.setBiography(req.getBiography());
        if (req.getAvatarUrl() != null) p.setAvatarUrl(req.getAvatarUrl());
        if (req.getEmail() != null) p.setEmail(req.getEmail());
        if (req.getPhone() != null) p.setPhone(req.getPhone());
        return personRepository.save(p);
    }

    @Transactional
    public void deleteMember(UUID treeId, UUID userId, UUID personId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        Person p = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        if (!p.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }
        personRepository.delete(p);
    }

    // ==================== SHARING LOGIC ====================

    @Transactional
    public TreeShareResponse generatePublicShareLink(UUID treeId, UUID userId, String permission) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);

        if (!permission.equals("view") && !permission.equals("edit")) {
            throw new AppException(ErrorCode.VALIDATION_FAILED);
        }

        tree.setIsPublic(true);
        tree.setSharePermission(permission);
        familyTreeRepository.save(tree);

        long sharedCount = treeAccessRepository.countByFamilyTreeId(treeId);
        String publicUrl = frontendUrl + "/trees/shared/" + tree.getShareToken();

        return TreeShareResponse.builder()
                .treeId(tree.getId())
                .treeName(tree.getName())
                .shareToken(tree.getShareToken())
                .shareUrl(publicUrl)
                .publicShareUrl(publicUrl)
                .sharedWithCount((int) sharedCount)
                .sharePermission(tree.getSharePermission())
                .build();
    }

    @Transactional
    public TreeAccess shareWithUser(UUID treeId, UUID ownerId, String targetEmail, String accessLevel) {
        log.info("START: shareWithUser - treeId: {}, ownerId: {}, targetEmail: {}, accessLevel: {}",
                treeId, ownerId, targetEmail, accessLevel);

        FamilyTree tree = findOwnedTreeOrThrow(treeId, ownerId);
        User owner = loadUserOrThrow(ownerId);
        User targetUser = userRepository.findByEmail(targetEmail)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        if (targetUser.getId().equals(ownerId)) {
            throw new AppException(ErrorCode.CANNOT_SHARE_TO_SELF);
        }

        Optional<TreeAccess> existing = treeAccessRepository.findByUserIdAndFamilyTreeId(
                targetUser.getId(), treeId);

        TreeAccess access;
        if (existing.isPresent()) {
            access = existing.get();
            access.setAccessLevel(accessLevel);
            access = treeAccessRepository.save(access);
            log.info("UPDATED: Existing TreeAccess updated for user: {}", targetEmail);
        } else {
            access = TreeAccess.builder()
                    .userId(targetUser.getId())
                    .familyTreeId(tree.getId())
                    .accessLevel(accessLevel)
                    .grantedBy(owner)
                    .build();
            access = treeAccessRepository.save(access);
            log.info("CREATED: New TreeAccess created for user: {}", targetEmail);
        }

        try {
            String shareUrl = frontendUrl + "/trees/shared/" + tree.getShareToken();
            emailService.sendTreeShareNotification(
                    targetEmail,
                    targetUser.getUsername(),
                    tree.getName(),
                    owner.getUsername(),
                    accessLevel,
                    shareUrl
            );
            log.info("SUCCESS: Sent share notification email to {}", targetEmail);
        } catch (Exception e) {
            log.error("FAILED: Error sending email to {}: {}", targetEmail, e.getMessage(), e);
        }

        log.info("COMPLETED: shareWithUser finished for user: {}", targetEmail);
        return access;
    }

    @Transactional(readOnly = true)
    public List<TreeAccess> getSharedUsers(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        return treeAccessRepository.findAllByFamilyTreeIdWithUsers(tree.getId());
    }

    @Transactional
    public void revokeAccess(UUID treeId, UUID ownerId, UUID targetUserId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, ownerId);
        treeAccessRepository.deleteByUserIdAndFamilyTreeId(targetUserId, tree.getId());
    }

    @Transactional
    public void disablePublicSharing(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        tree.setIsPublic(false);
        familyTreeRepository.save(tree);
    }

    @Transactional(readOnly = true)
    public FamilyTree getTreeByShareToken(UUID shareToken) {
        FamilyTree tree = familyTreeRepository.findByShareToken(shareToken)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        if (tree.getCreatedBy() != null) {
            tree.getCreatedBy().getEmail();
        }
        return tree;
    }

    @Transactional(readOnly = true)
    public boolean canEdit(UUID treeId, UUID userId) {
        Optional<FamilyTree> ownedTree = familyTreeRepository.findByIdAndCreatedBy(
                treeId, loadUserOrThrow(userId));
        if (ownedTree.isPresent()) {
            return true;
        }

        Optional<FamilyTree> tree = familyTreeRepository.findById(treeId);
        if (tree.isPresent() && Boolean.TRUE.equals(tree.get().getIsPublic())
                && "edit".equals(tree.get().getSharePermission())) {
            return true;
        }

        Optional<TreeAccess> access = treeAccessRepository.findByUserIdAndFamilyTreeId(userId, treeId);
        return access.isPresent() &&
                ("edit".equals(access.get().getAccessLevel()) || "admin".equals(access.get().getAccessLevel()));
    }

    @Transactional(readOnly = true)
    public boolean canView(UUID treeId, UUID userId) {
        Optional<FamilyTree> ownedTree = familyTreeRepository.findByIdAndCreatedBy(
                treeId, loadUserOrThrow(userId));
        if (ownedTree.isPresent()) {
            return true;
        }

        Optional<TreeAccess> access = treeAccessRepository.findByUserIdAndFamilyTreeId(userId, treeId);
        return access.isPresent();
    }

    @Transactional(readOnly = true)
    public FamilyTree getSharedTree(UUID shareToken, UUID userId) {
        FamilyTree tree = getTreeByShareToken(shareToken);

        if (tree.getCreatedBy() != null) {
            tree.getCreatedBy().getId();
        }

        if (Boolean.TRUE.equals(tree.getIsPublic())) {
            return tree;
        }

        if (userId == null) {
            throw new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND);
        }

        if (tree.getCreatedBy().getId().equals(userId) || canView(tree.getId(), userId)) {
            return tree;
        }

        throw new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND);
    }

    // ==================== MỚI: THÔNG TIN ACCESS QUA SHARE TOKEN ====================

    /**
     * Lấy thông tin access từ shareToken
     * Dùng cho frontend khi mở link shared để biết:
     * - Có phải owner không?
     * - Có quyền edit không?
     * - Vai trò hiện tại (OWNER / EDITOR / VIEWER)
     */
    @Transactional(readOnly = true)
    public SharedTreeAccessInfoResponse getSharedTreeAccessInfo(UUID shareToken, UUID userId) {
        FamilyTree tree = getTreeByShareToken(shareToken);

        boolean canEdit = false;
        boolean canView = false;
        String role = "VIEWER";

        if (userId != null) {
            // Kiểm tra owner
            if (tree.getCreatedBy().getId().equals(userId)) {
                canEdit = true;
                canView = true;
                role = "OWNER";
            } else {
                // Kiểm tra tree_access (chia sẻ cá nhân)
                Optional<TreeAccess> access = treeAccessRepository
                        .findByUserIdAndFamilyTreeId(userId, tree.getId());

                if (access.isPresent()) {
                    String accessLevel = access.get().getAccessLevel();
                    canView = true;

                    if ("edit".equals(accessLevel) || "admin".equals(accessLevel)) {
                        canEdit = true;
                        role = "EDITOR";
                    }
                } else if (Boolean.TRUE.equals(tree.getIsPublic())) {
                    // Public link
                    canView = true;
                    if ("edit".equals(tree.getSharePermission())) {
                        canEdit = true;
                        role = "EDITOR";
                    }
                }
            }
        } else {
            // Chưa đăng nhập → chỉ có thể xem nếu public
            if (Boolean.TRUE.equals(tree.getIsPublic())) {
                canView = true;
                if ("edit".equals(tree.getSharePermission())) {
                    canEdit = true;
                }
            }
        }

        return SharedTreeAccessInfoResponse.builder()
                .treeId(tree.getId())
                .treeName(tree.getName())
                .canEdit(canEdit)
                .canView(canView)
                .role(role)
                .build();
    }

    // ==================== MỚI: LẤY CẢ CÂY SỞ HỮU VÀ CÂY ĐƯỢC CHIA SẺ ====================

    /**
     * ✅ MỚI: Lấy tất cả cây gia phả của user (bao gồm cả cây sở hữu và cây được chia sẻ)
     * Sử dụng JPQL query hiệu quả thay vì merge thủ công
     */
    @Transactional(readOnly = true)
    public List<FamilyTree> getUserTrees(UUID userId) {
        log.info("Getting all accessible trees for user: {}", userId);

        // ✅ SỬ DỤNG METHOD MỚI: Single query hiệu quả
        List<FamilyTree> allTrees = familyTreeRepository.findAllAccessibleByUser(userId);

        log.info("Total accessible trees for user {}: {}", userId, allTrees.size());
        return allTrees;
    }
}