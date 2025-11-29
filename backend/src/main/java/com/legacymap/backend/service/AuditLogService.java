package com.legacymap.backend.service;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.legacymap.backend.dto.response.TreeHistoryResponse;
import com.legacymap.backend.entity.*;
import com.legacymap.backend.repository.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.annotation.Propagation;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional
public class AuditLogService {

    private final AuditLogRepository auditLogRepository;
    private final FamilyTreeRepository treeRepository;
    private final UserRepository userRepository;

    @Transactional(readOnly = true, propagation = Propagation.REQUIRED)
    public Page<TreeHistoryResponse> getTreeHistory(UUID treeId, Pageable pageable) {
        log.debug("📋 Getting tree history - treeId: {}, pageable: {}", treeId, pageable);

        try {
            Page<AuditLog> auditLogs = auditLogRepository.findByTreeIdWithUserOrderByCreatedAtDesc(treeId, pageable);
            log.debug("📋 Found {} audit logs for tree: {}", auditLogs.getTotalElements(), treeId);

            if (auditLogs.getContent().isEmpty()) {
                log.debug("📋 No audit logs found for tree: {}", treeId);
            } else {
                log.debug("📋 First audit log user: {}",
                        auditLogs.getContent().get(0).getUser() != null ?
                                auditLogs.getContent().get(0).getUser().getUsername() : "null");
            }
            return auditLogs.map(this::toResponse);
        } catch (Exception e) {
            log.error("❌ Error getting tree history: {}", e.getMessage(), e);
            throw e;
        }
    }

    // Log khi thêm member
    @Transactional
    public void logMemberCreated(UUID treeId, UUID userId, UUID personId, Person person) {
        try {
            log.debug("🔄 START logMemberCreated - treeId: {}, userId: {}, personId: {}", treeId, userId, personId);
            Map<String, Object> newData = toSafeMap(person);
            log.debug("✅ Serialized newData: {}", newData != null ? "success" : "null");

            saveHistory(treeId, userId, "CREATED",
                    "MEMBER", personId, person.getFullName(),
                    null, newData,
                    "Đã thêm thành viên: " + person.getFullName());
            log.debug("✅ COMPLETED logMemberCreated");
        } catch (Exception e) {
            log.error("❌ FAILED logMemberCreated - treeId: {}, userId: {}", treeId, userId, e);
        }
    }

    // Log khi sửa member
    @Transactional
    public void logMemberUpdated(UUID treeId, UUID userId, UUID personId, Person oldPerson, Person newPerson) {
        try {
            log.debug("🔄 START logMemberUpdated - treeId: {}, userId: {}, personId: {}", treeId, userId, personId);
            Map<String, Object> oldData = toSafeMap(oldPerson);
            Map<String, Object> newData = toSafeMap(newPerson);
            log.debug("✅ Serialized - oldData: {}, newData: {}",
                    oldData != null ? "success" : "null",
                    newData != null ? "success" : "null");

            saveHistory(treeId, userId, "UPDATED",
                    "MEMBER", personId, newPerson.getFullName(),
                    oldData, newData,
                    "Đã cập nhật thành viên: " + newPerson.getFullName());
            log.debug("✅ COMPLETED logMemberUpdated");
        } catch (Exception e) {
            log.error("❌ FAILED logMemberUpdated - treeId: {}, userId: {}, personId: {}",
                    treeId, userId, personId, e);
        }
    }

    // Log khi xóa member
    @Transactional
    public void logMemberDeleted(UUID treeId, UUID userId, UUID personId, Person person) {
        try {
            log.debug("🔄 START logMemberDeleted - treeId: {}, userId: {}, personId: {}", treeId, userId, personId);
            Map<String, Object> oldData = toSafeMap(person);
            log.debug("✅ Serialized oldData: {}", oldData != null ? "success" : "null");

            saveHistory(treeId, userId, "DELETED",
                    "MEMBER", personId, person.getFullName(),
                    oldData, null,
                    "Đã xóa thành viên: " + person.getFullName());
            log.debug("✅ COMPLETED logMemberDeleted");
        } catch (Exception e) {
            log.error("❌ FAILED logMemberDeleted - treeId: {}, userId: {}, personId: {}",
                    treeId, userId, personId, e);
        }
    }

    // Log khi thêm relationship
    @Transactional
    public void logRelationshipCreated(UUID treeId, UUID userId, Relationship relationship) {
        try {
            log.debug("🔄 START logRelationshipCreated - treeId: {}, userId: {}", treeId, userId);
            String desc = String.format("Đã thêm quan hệ: %s - %s (%s)",
                    relationship.getPerson1().getFullName(),
                    relationship.getPerson2().getFullName(),
                    relationship.getRelationshipType());

            Map<String, Object> newData = toSafeMap(relationship);
            log.debug("✅ Serialized relationship data: {}", newData != null ? "success" : "null");

            saveHistory(treeId, userId, "CREATED",
                    "RELATIONSHIP", relationship.getId(), desc,
                    null, newData, desc);
            log.debug("✅ COMPLETED logRelationshipCreated");
        } catch (Exception e) {
            log.error("❌ FAILED logRelationshipCreated - treeId: {}, userId: {}", treeId, userId, e);
        }
    }

    // ✅ FIXED: Safe Map Converter - KHÔNG dùng ObjectMapper để tránh StackOverflowError
    private Map<String, Object> toSafeMap(Object obj) {
        if (obj == null) return null;

        try {
            Map<String, Object> safeMap = new HashMap<>();

            if (obj instanceof Person) {
                Person p = (Person) obj;
                safeMap.put("id", p.getId() != null ? p.getId().toString() : null);
                safeMap.put("fullName", p.getFullName());
                safeMap.put("gender", p.getGender());
                safeMap.put("birthDate", p.getBirthDate() != null ? p.getBirthDate().toString() : null);
                safeMap.put("deathDate", p.getDeathDate() != null ? p.getDeathDate().toString() : null);
                safeMap.put("birthPlace", p.getBirthPlace());
                safeMap.put("deathPlace", p.getDeathPlace());
                safeMap.put("biography", p.getBiography());
                safeMap.put("avatarUrl", p.getAvatarUrl());
                safeMap.put("phone", p.getPhone());
                safeMap.put("email", p.getEmail());
                // ❌ TUYỆT ĐỐI KHÔNG serialize createdBy, familyTree để tránh vòng lặp
                safeMap.put("createdById", p.getCreatedBy() != null ? p.getCreatedBy().getId().toString() : null);
                safeMap.put("familyTreeId", p.getFamilyTree() != null ? p.getFamilyTree().getId().toString() : null);

            } else if (obj instanceof Relationship) {
                Relationship r = (Relationship) obj;
                safeMap.put("id", r.getId() != null ? r.getId().toString() : null);
                safeMap.put("person1Id", r.getPerson1() != null ? r.getPerson1().getId().toString() : null);
                safeMap.put("person1Name", r.getPerson1() != null ? r.getPerson1().getFullName() : null);
                safeMap.put("person2Id", r.getPerson2() != null ? r.getPerson2().getId().toString() : null);
                safeMap.put("person2Name", r.getPerson2() != null ? r.getPerson2().getFullName() : null);
                safeMap.put("relationshipType", r.getRelationshipType());
                // ❌ TUYỆT ĐỐI KHÔNG serialize person1, person2 objects

            } else {
                // ⚠️ KHÔNG DÙNG ObjectMapper cho Hibernate entities - chỉ dùng cho DTO đơn giản
                log.warn("⚠️ Using ObjectMapper fallback for non-entity object: {}", obj.getClass().getSimpleName());

                // Chỉ dùng ObjectMapper cho các class không phải entity
                if (!obj.getClass().getName().contains("com.legacymap.backend.entity")) {
                    ObjectMapper safeMapper = createSafeObjectMapper();
                    return safeMapper.convertValue(obj, Map.class);
                } else {
                    // Entity khác - chỉ lấy ID và tên class
                    safeMap.put("_entityType", obj.getClass().getSimpleName());
                    safeMap.put("_warning", "Entity serialization disabled to prevent StackOverflow");
                }
            }

            return safeMap;

        } catch (Exception e) {
            log.error("🚨 CRITICAL: Failed to convert object to safe map - {}: {}",
                    obj.getClass().getSimpleName(), e.getMessage());

            // Fallback cực kỳ đơn giản
            Map<String, Object> fallback = new HashMap<>();
            fallback.put("_error", "Serialization failed");
            fallback.put("_type", obj.getClass().getSimpleName());
            return fallback;
        }
    }

    // ObjectMapper an toàn (chỉ dùng cho non-entity objects)
    private ObjectMapper createSafeObjectMapper() {
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.configure(SerializationFeature.FAIL_ON_SELF_REFERENCES, false);
        mapper.configure(SerializationFeature.FAIL_ON_UNWRAPPED_TYPE_IDENTIFIERS, false);
        return mapper;
    }

    // Helper method - THÊM DEBUG CHI TIẾT
    private void saveHistory(UUID treeId, UUID userId, String action,
                             String entityType, UUID entityId, String entityName,
                             Map<String, Object> oldData, Map<String, Object> newData,
                             String description) {
        try {
            log.debug("💾 START saveHistory - treeId: {}, userId: {}, action: {}", treeId, userId, action);

            FamilyTree tree = treeRepository.findById(treeId)
                    .orElseThrow(() -> new RuntimeException("Tree not found: " + treeId));
            User user = userRepository.findById(userId)
                    .orElseThrow(() -> new RuntimeException("User not found: " + userId));

            AuditLog auditLog = AuditLog.builder()
                    .tree(tree)
                    .user(user)
                    .action(action)
                    .entityType(entityType)
                    .entityId(entityId)
                    .entityName(entityName)
                    .oldData(oldData)
                    .newData(newData)
                    .description(description)
                    .build();

            AuditLog saved = auditLogRepository.save(auditLog);
            log.info("✅ SUCCESS: Saved audit log - id: {}, desc: {}", saved.getId(), description);
            log.debug("💾 COMPLETED saveHistory");

        } catch (Exception e) {
            log.error("❌ FAILED to save audit log - treeId: {}, userId: {}, action: {}",
                    treeId, userId, action, e);
        }
    }

    // ✅ FIXED HOÀN TOÀN: toResponse method - lấy đầy đủ thông tin user và userProfile
    private TreeHistoryResponse toResponse(AuditLog auditLog) {
        log.debug("📄 Converting audit log to response - id: {}, action: {}",
                auditLog.getId(), auditLog.getAction());

        // ✅ Lấy đầy đủ thông tin user
        String userName = "Unknown User";
        String userAvatar = null;

        if (auditLog.getUser() != null) {
            User user = auditLog.getUser();

            // Lấy tên từ UserProfile hoặc username
            if (user.getUserProfile() != null && user.getUserProfile().getFullName() != null) {
                userName = user.getUserProfile().getFullName();
            } else {
                userName = user.getUsername();
            }

            // Lấy avatar từ UserProfile
            if (user.getUserProfile() != null && user.getUserProfile().getAvatarUrl() != null) {
                userAvatar = user.getUserProfile().getAvatarUrl();
            }

            log.debug("✅ User info - name: {}, avatar: {}", userName, userAvatar != null ? "present" : "null");
        }

        TreeHistoryResponse response = TreeHistoryResponse.builder()
                .id(auditLog.getId())
                .userName(userName)
                .userAvatar(userAvatar)
                .action(auditLog.getAction())
                .entityType(auditLog.getEntityType())
                .entityName(auditLog.getEntityName())
                .description(auditLog.getDescription())
                .oldValue(auditLog.getOldData() != null ? formatData(auditLog.getOldData()) : "No old data")
                .newValue(auditLog.getNewData() != null ? formatData(auditLog.getNewData()) : "No new data")
                .createdAt(auditLog.getCreatedAt())
                .build();

        log.debug("✅ Converted audit log - id: {}, userName: {}, action: {}",
                auditLog.getId(), userName, auditLog.getAction());

        return response;
    }

    // ✅ Helper method để format data đẹp hơn
    private String formatData(Map<String, Object> data) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(data);
        } catch (Exception e) {
            return data.toString();
        }
    }

    @Transactional
    public void log(
            UUID userId,
            String entityType,
            UUID entityId,
            String action,
            Map<String, Object> oldData,
            Map<String, Object> newData
    ) {
        User userRef = null;
        if (userId != null) {
            userRef = new User();
            userRef.setId(userId);
        }

        AuditLog log = AuditLog.builder()
                .user(userRef)
                .entityType(entityType)
                .entityId(entityId)
                .action(action)
                .oldData(oldData)
                .newData(newData)
                .build();

        auditLogRepository.save(log);
    }
}