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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

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
        RelationshipType typeEnum = parseType(req.getRelationshipType());
        if (req.getPerson1Id().equals(req.getPerson2Id())) {
            throw new AppException(ErrorCode.RELATIONSHIP_SELF_LINK);
        }

        FamilyTree tree = loadOwnedTree(treeId, userId);
        Person p1 = loadPerson(req.getPerson1Id());
        Person p2 = loadPerson(req.getPerson2Id());

        if (!p1.getFamilyTree().getId().equals(tree.getId()) || !p2.getFamilyTree().getId().equals(tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }

        String type = toDb(typeEnum);
        // prevent duplicates
        if (relationshipRepository.existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(treeId, p1.getId(), p2.getId(), type)) {
            throw new AppException(ErrorCode.RELATIONSHIP_ALREADY_EXISTS);
        }

        Relationship main = Relationship.builder()
                .familyTree(tree)
                .person1(p1)
                .person2(p2)
                .relationshipType(type)
                .notes(req.getNotes())
                .createdBy(loadUser(userId))
                .build();
        main = relationshipRepository.save(main);

        // reciprocal
        RelationshipType reciprocalEnum = switch (typeEnum) {
            case PARENT -> RelationshipType.CHILD;
            case CHILD -> RelationshipType.PARENT;
            case SPOUSE -> RelationshipType.SPOUSE;
            case SIBLING -> RelationshipType.SIBLING;
        };
        String reciprocalType = toDb(reciprocalEnum);

        if (!relationshipRepository.existsByFamilyTree_IdAndPerson1_IdAndPerson2_IdAndRelationshipType(treeId, p2.getId(), p1.getId(), reciprocalType)) {
            Relationship reciprocal = Relationship.builder()
                    .familyTree(tree)
                    .person1(p2)
                    .person2(p1)
                    .relationshipType(reciprocalType)
                    .notes(req.getNotes())
                    .createdBy(loadUser(userId))
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
}
