package com.legacymap.backend.service;

import com.legacymap.backend.dto.response.RelationshipSuggestion;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.RelationshipRepository;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.ai.GeminiSuggestClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service("relationshipHeuristicService")
public class RelationshipSuggestionService {

    @Autowired
    private RelationshipRepository relationshipRepository;
    @Autowired
    private FamilyTreeRepository familyTreeRepository;
    @Autowired
    private PersonRepository personRepository;
    @Autowired
    private UserRepository userRepository;

    @Autowired
    private GeminiSuggestClient geminiSuggestClient;

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

    @Transactional(readOnly = true)
    public List<RelationshipSuggestion> suggest(UUID treeId, UUID userId, UUID person1Id, UUID person2Id) {

        FamilyTree tree = loadOwnedTree(treeId, userId);
        Person p1 = loadPerson(person1Id);
        Person p2 = loadPerson(person2Id);
        if (!Objects.equals(p1.getFamilyTree().getId(), tree.getId()) || !Objects.equals(p2.getFamilyTree().getId(), tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }
        if (person1Id.equals(person2Id)) {
            throw new AppException(ErrorCode.RELATIONSHIP_SELF_LINK);
        }

        List<Relationship> rels = relationshipRepository.findAllByFamilyTree_Id(treeId);

        // Try AI first; fallback to heuristic if absent or empty
        try {
            java.util.Optional<java.util.List<RelationshipSuggestion>> ai =
                    geminiSuggestClient.suggest(tree, p1, p2, rels);
            if (ai.isPresent() && !ai.get().isEmpty()) {
                return ai.get();
            }
        } catch (Exception ignored) {
            // fallback below
        }
        // AI-only mode: if AI has no suggestion, return empty list
        return java.util.Collections.emptyList();
    }

    @Transactional(readOnly = true)
    public Map<UUID, RelationshipSuggestion> suggestForSource(
            UUID treeId,
            UUID userId,
            UUID sourceId,
            List<UUID> candidateIds
    ) {
        FamilyTree tree = loadOwnedTree(treeId, userId);
        Person source = loadPerson(sourceId);
        if (!Objects.equals(source.getFamilyTree().getId(), tree.getId())) {
            throw new AppException(ErrorCode.RELATIONSHIP_NOT_SAME_TREE);
        }

        List<Person> candidates = personRepository.findAllById(candidateIds);
        // Filter only those in same tree and not self
        candidates.removeIf(p -> p == null || Objects.equals(p.getId(), sourceId) || !Objects.equals(p.getFamilyTree().getId(), tree.getId()));

        List<Relationship> rels = relationshipRepository.findAllByFamilyTree_Id(treeId);

        Map<UUID, RelationshipSuggestion> out = geminiSuggestClient.suggestForSourceBatch(tree, source, candidates, rels);
        return out == null ? java.util.Collections.emptyMap() : out;
    }
}