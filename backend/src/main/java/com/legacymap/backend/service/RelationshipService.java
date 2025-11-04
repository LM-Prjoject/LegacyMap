package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.RelationshipCreateRequest;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.enums.RelationshipType;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.RelationshipRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.dto.response.RelationshipDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class RelationshipService {

    @Autowired
    private RelationshipRepository relationshipRepository;

    @Autowired
    private FamilyTreeRepository familyTreeRepository;

    @Autowired
    private PersonRepository personRepository;

    @Autowired
    private UserRepository userRepository;

    private User loadUser(UUID userId) {
        return userRepository.findById(userId).orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    private FamilyTree loadOwnedTree(UUID treeId, UUID userId) {
        User user = loadUser(userId);
        return familyTreeRepository.findByIdAndCreatedBy(treeId, user)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
    }

    private Person loadPerson(UUID personId) {
        return personRepository.findById(personId).orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
    }

    private RelationshipType parseType(String type) {
        if (type == null) throw new AppException(ErrorCode.RELATIONSHIP_INVALID_TYPE);
        String t = type.trim().toUpperCase();
        try {
            return RelationshipType.valueOf(t);
        } catch (IllegalArgumentException ex) {
            throw new AppException(ErrorCode.RELATIONSHIP_INVALID_TYPE);
        }
    }

    private String toDb(RelationshipType type) {
        return type.name().toLowerCase();
    }

    @Transactional
    public Relationship create(UUID treeId, UUID userId, RelationshipCreateRequest req) {
        RelationshipType typeEnum = req.getRelationshipType();

        if (req.getPerson1Id().equals(req.getPerson2Id())) {
            throw new AppException(ErrorCode.RELATIONSHIP_SELF_LINK);
        }

        User user = loadUser(userId);
        FamilyTree tree = familyTreeRepository.findByIdAndCreatedBy(treeId, user)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));

        Person p1 = loadPerson(req.getPerson1Id());
        Person p2 = loadPerson(req.getPerson2Id());

        if (!p1.getFamilyTree().getId().equals(tree.getId()) || !p2.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }

        if (typeEnum == RelationshipType.CHILD) {
            typeEnum = RelationshipType.PARENT;
            Person tmp = p1; p1 = p2; p2 = tmp;
        }

        String type = toDb(typeEnum);

        if (typeEnum == RelationshipType.SPOUSE || typeEnum == RelationshipType.SIBLING) {
            boolean dup = relationshipRepository
                    .existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(treeId, p1.getId(), p2.getId(), type)
                    || relationshipRepository
                    .existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(treeId, p2.getId(), p1.getId(), type);
            if (dup) throw new AppException(ErrorCode.RELATIONSHIP_ALREADY_EXISTS);
        } else {
            if (relationshipRepository
                    .existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(treeId, p1.getId(), p2.getId(), type)) {
                throw new AppException(ErrorCode.RELATIONSHIP_ALREADY_EXISTS);
            }
        }

        Relationship main = Relationship.builder()
                .familyTree(tree)
                .person1(p1)
                .person2(p2)
                .relationshipType(type)
                .notes(req.getNotes())
                .createdBy(user)
                .build();
        main = relationshipRepository.save(main);

        RelationshipType reciprocalEnum = switch (typeEnum) {
            case PARENT -> RelationshipType.CHILD;
            case CHILD  -> RelationshipType.PARENT;
            case SPOUSE -> RelationshipType.SPOUSE;
            case SIBLING-> RelationshipType.SIBLING;
        };
        String reciprocalType = toDb(reciprocalEnum);

        if (!relationshipRepository
                .existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(treeId, p2.getId(), p1.getId(), reciprocalType)) {
            Relationship reciprocal = Relationship.builder()
                    .familyTree(tree)
                    .person1(p2)
                    .person2(p1)
                    .relationshipType(reciprocalType)
                    .notes(req.getNotes())
                    .createdBy(user)
                    .build();
            relationshipRepository.save(reciprocal);
        }

        return main;
    }

    @Transactional
    public void delete(UUID treeId, UUID userId, UUID relationshipId) {
        FamilyTree tree = loadOwnedTree(treeId, userId);
        Relationship rel = relationshipRepository.findByIdAndFamilyTree_Id(relationshipId, tree.getId())
                .orElseThrow(() -> new AppException(ErrorCode.RELATIONSHIP_NOT_FOUND));

        // attempt to also delete reciprocal
        RelationshipType current = parseType(rel.getRelationshipType());
        RelationshipType reciprocalEnum = switch (current) {
            case PARENT -> RelationshipType.CHILD;
            case CHILD -> RelationshipType.PARENT;
            case SPOUSE -> RelationshipType.SPOUSE;
            case SIBLING -> RelationshipType.SIBLING;
        };
        String reciprocalType = toDb(reciprocalEnum);
        relationshipRepository.findByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(
                tree.getId(), rel.getPerson2().getId(), rel.getPerson1().getId(), reciprocalType
        ).ifPresent(relationshipRepository::delete);

        relationshipRepository.delete(rel);
    }

    public List<RelationshipDTO> listByTree(UUID treeId, UUID userId) {
        try {
            // Tạm thời bỏ qua kiểm tra quyền, chỉ kiểm tra sự tồn tại của cây
            if (!familyTreeRepository.existsById(treeId)) {
                throw new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND);
            }
            
            List<Relationship> relationships = relationshipRepository.findByFamilyTreeId(treeId);
            System.out.println("Found " + relationships.size() + " relationships for tree: " + treeId);
            
            // Chuyển đổi từ Entity sang DTO
            return relationships.stream()
                    .map(RelationshipDTO::fromEntity)
                    .collect(Collectors.toList());
                    
        } catch (AppException e) {
            throw e;
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Lỗi khi lấy danh sách mối quan hệ: " + e.getMessage());
            throw new AppException(ErrorCode.INTERNAL_ERROR);
        }
    }

    public List<RelationshipDTO> listByPerson(UUID treeId, UUID userId, UUID personId) {
        try {
            // Check if tree exists and user has access
            FamilyTree tree = loadOwnedTree(treeId, userId);
            
            // Check if person exists and belongs to the tree
            Person person = loadPerson(personId);
            if (!person.getFamilyTree().getId().equals(treeId)) {
                throw new AppException(ErrorCode.PERSON_NOT_FOUND);
            }
            
            // Get all relationships where the person is either person1 or person2
            List<Relationship> relationships = relationshipRepository.findByPerson1IdOrPerson2Id(personId, personId);
            
            // Convert to DTOs
            return relationships.stream()
                    .map(RelationshipDTO::fromEntity)
                    .collect(Collectors.toList());
                    
        } catch (AppException e) {
            throw e;
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Lỗi khi lấy danh sách mối quan hệ của người dùng: " + e.getMessage());
            throw new AppException(ErrorCode.INTERNAL_ERROR);
        }
    }
}
