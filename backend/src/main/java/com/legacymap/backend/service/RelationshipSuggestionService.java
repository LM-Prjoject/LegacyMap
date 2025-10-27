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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.Period;
import java.util.*;
import java.util.stream.Collectors;

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

        // Build quick lookup maps
        Map<UUID, Set<UUID>> parentsOf = new HashMap<>();
        Map<UUID, Set<UUID>> childrenOf = new HashMap<>();
        Map<UUID, Set<UUID>> spousesOf = new HashMap<>();

        for (Relationship r : rels) {
            UUID a = r.getPerson1().getId();
            UUID b = r.getPerson2().getId();
            String type = r.getRelationshipType();
            if ("parent".equals(type)) {
                childrenOf.computeIfAbsent(a, k -> new HashSet<>()).add(b);
                parentsOf.computeIfAbsent(b, k -> new HashSet<>()).add(a);
            } else if ("child".equals(type)) {
                // store both ways to be robust if only one direction exists
                parentsOf.computeIfAbsent(a, k -> new HashSet<>()).add(b);
                childrenOf.computeIfAbsent(b, k -> new HashSet<>()).add(a);
            } else if ("spouse".equals(type)) {
                spousesOf.computeIfAbsent(a, k -> new HashSet<>()).add(b);
            } else if ("sibling".equals(type)) {
                // optional: not required for rules, but may be used
            }
        }

        // If an exact type already exists between p1 and p2, we could down-rank it
        Set<String> existing = rels.stream()
                .filter(r -> r.getPerson1().getId().equals(person1Id) && r.getPerson2().getId().equals(person2Id))
                .map(Relationship::getRelationshipType)
                .collect(Collectors.toSet());
        Set<String> existingReverse = rels.stream()
                .filter(r -> r.getPerson1().getId().equals(person2Id) && r.getPerson2().getId().equals(person1Id))
                .map(Relationship::getRelationshipType)
                .collect(Collectors.toSet());

        List<RelationshipSuggestion> out = new ArrayList<>();

        // Helper: age gap
        Double ageGap = computeAgeGapYears(p1.getBirthDate(), p2.getBirthDate());

        // PARENT suggestion (p1 -> p2)
        out.add(scoreParentChild("parent", person1Id, person2Id, ageGap, parentsOf, childrenOf, existing, existingReverse));
        // CHILD suggestion (p1 <- p2)
        out.add(scoreParentChild("child", person1Id, person2Id, ageGap, parentsOf, childrenOf, existing, existingReverse));

        // SIBLING suggestion
        out.add(scoreSibling(person1Id, person2Id, parentsOf, existing, existingReverse));

        // SPOUSE suggestion
        out.add(scoreSpouse(ageGap, person1Id, person2Id, parentsOf, existing, existingReverse));

        // Filter negatives and sort desc by confidence
        return out.stream()
                .filter(s -> s.getConfidence() > 0)
                .sorted(Comparator.comparing(RelationshipSuggestion::getConfidence).reversed())
                .collect(Collectors.toList());
    }

    private RelationshipSuggestion scoreParentChild(
            String type,
            UUID p1,
            UUID p2,
            Double ageGap,
            Map<UUID, Set<UUID>> parentsOf,
            Map<UUID, Set<UUID>> childrenOf,
            Set<String> existing,
            Set<String> existingReverse
    ) {
        double score = 0.0;
        List<String> reasons = new ArrayList<>();

        boolean suggestParent = "parent".equals(type);
        boolean suggestChild = "child".equals(type);

        // Age-based heuristic
        if (ageGap != null) {
            if (ageGap >= 16 && ageGap <= 60) {
                score += 0.5;
                reasons.add("Age gap supports parent/child: ~" + ageGap.intValue() + " years");
            } else if (ageGap > 60) {
                score += 0.2; // possible but less likely
                reasons.add("Large age gap " + ageGap.intValue() + " years");
            } else if (ageGap < 12) {
                score -= 0.6; // unlikely
                reasons.add("Small age gap " + ageGap.intValue() + " years");
            }
        } else {
            reasons.add("Missing birth dates");
        }

        // Conflict checks (if already spouse or sibling, down-rank)
        if (existing.contains("spouse") || existingReverse.contains("spouse")) {
            score -= 0.3;
            reasons.add("Existing spouse edge");
        }
        if (existing.contains("sibling") || existingReverse.contains("sibling")) {
            score -= 0.4;
            reasons.add("Existing sibling edge");
        }

        // Graph consistency: if they already share a parent, avoid parent-child
        if (sharesParent(p1, p2, parentsOf)) {
            score -= 0.6;
            reasons.add("They share a parent");
        }

        String outType = type;
        return new RelationshipSuggestion(outType, clamp(score), reasons);
    }

    private RelationshipSuggestion scoreSibling(
            UUID p1,
            UUID p2,
            Map<UUID, Set<UUID>> parentsOf,
            Set<String> existing,
            Set<String> existingReverse
    ) {
        double score = 0.0;
        List<String> reasons = new ArrayList<>();

        if (sharesParent(p1, p2, parentsOf)) {
            score += 0.7;
            reasons.add("Share at least one parent");
        }
        if (existing.contains("parent") || existingReverse.contains("parent") ||
                existing.contains("child") || existingReverse.contains("child")) {
            score -= 0.6;
            reasons.add("Existing parent-child edge");
        }

        return new RelationshipSuggestion("sibling", clamp(score), reasons);
    }

    private RelationshipSuggestion scoreSpouse(
            Double ageGap,
            UUID p1,
            UUID p2,
            Map<UUID, Set<UUID>> parentsOf,
            Set<String> existing,
            Set<String> existingReverse
    ) {
        double score = 0.0;
        List<String> reasons = new ArrayList<>();

        if (ageGap != null) {
            if (ageGap <= 10) { score += 0.5; reasons.add("Age gap <= 10"); }
            else if (ageGap <= 15) { score += 0.3; reasons.add("Age gap <= 15"); }
            else if (ageGap > 25) { score -= 0.3; reasons.add("Age gap > 25"); }
        } else {
            reasons.add("Missing birth dates");
        }

        if (sharesParent(p1, p2, parentsOf)) {
            score -= 0.7; reasons.add("Share parent (unlikely spouses)");
        }
        if (existing.contains("parent") || existingReverse.contains("parent") ||
                existing.contains("child") || existingReverse.contains("child")) {
            score -= 0.8; reasons.add("Existing parent-child edge");
        }

        return new RelationshipSuggestion("spouse", clamp(score), reasons);
    }

    private boolean sharesParent(UUID a, UUID b, Map<UUID, Set<UUID>> parentsOf) {
        Set<UUID> pa = parentsOf.getOrDefault(a, Collections.emptySet());
        Set<UUID> pb = parentsOf.getOrDefault(b, Collections.emptySet());
        for (UUID x : pa) if (pb.contains(x)) return true;
        return false;
    }

    private Double computeAgeGapYears(LocalDate d1, LocalDate d2) {
        if (d1 == null || d2 == null) return null;
        if (d1.isEqual(d2)) return 0.0;
        LocalDate older = d1.isBefore(d2) ? d1 : d2;
        LocalDate younger = d1.isBefore(d2) ? d2 : d1;
        return (double) Period.between(older, younger).getYears();
    }

    private double clamp(double v) {
        if (v < 0) return 0.0;
        if (v > 1) return 1.0;
        return v;
    }
}
