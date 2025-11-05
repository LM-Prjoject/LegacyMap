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

        // SPOUSE suggestion - pass childrenOf map as well
        out.add(scoreSpouse(ageGap, person1Id, person2Id, parentsOf, childrenOf, existing, existingReverse));

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

        // Get person details
        Person person1 = personRepository.findById(p1).orElse(null);
        Person person2 = personRepository.findById(p2).orElse(null);

        if (person1 == null || person2 == null) {
            return new RelationshipSuggestion("sibling", 0.0, Collections.singletonList("Person not found"));
        }

        // 1. Check if same person
        if (p1.equals(p2)) {
            return new RelationshipSuggestion("sibling", 0.0, Collections.singletonList("Same person"));
        }

        // 2. Check last name (siblings typically have the same last name in Vietnam)
        boolean sameLastName = hasSameLastName(person1.getFullName(), person2.getFullName());
        if (sameLastName) {
            score += 0.4;
            reasons.add("Same last name");
        } else {
            score -= 0.3;
            reasons.add("Different last names");
        }

        // 3. Check if they share parents
        if (sharesParent(p1, p2, parentsOf)) {
            score += 0.6;
            reasons.add("Share at least one parent");
        }

        // 4. Age gap check (siblings typically have smaller age gaps)
        Double ageGap = computeAgeGapYears(person1.getBirthDate(), person2.getBirthDate());
        if (ageGap != null) {
            if (ageGap <= 15) {
                score += 0.3;
                reasons.add("Age gap " + ageGap.intValue() + " years");
            } else {
                score -= 0.2;
                reasons.add("Large age gap for siblings: " + ageGap.intValue() + " years");
            }
        }

        // 5. Check for existing relationships that would conflict
        if (existing.contains("parent") || existingReverse.contains("parent") ||
                existing.contains("child") || existingReverse.contains("child") ||
                existing.contains("spouse") || existingReverse.contains("spouse")) {
            score -= 0.7;
            reasons.add("Conflicting relationship exists");
        }

        // 6. If they are already marked as siblings
        if (existing.contains("sibling") || existingReverse.contains("sibling")) {
            score = 0.0;  // No need to suggest if already siblings
            reasons.add("Already siblings");
        }

        return new RelationshipSuggestion("sibling", clamp(score), reasons);
    }

    private boolean hasSameLastName(String name1, String name2) {
        if (name1 == null || name2 == null || name1.trim().isEmpty() || name2.trim().isEmpty()) {
            return false;
        }
        // Lấy từ đầu tiên làm họ (theo quy ước tên người Việt)
        String[] parts1 = name1.trim().split("\\s+");
        String[] parts2 = name2.trim().split("\\s+");

        if (parts1.length == 0 || parts2.length == 0) {
            return false;
        }

        // Lấy từ đầu tiên làm họ
        String lastName1 = parts1[0];
        String lastName2 = parts2[0];

        return lastName1.equalsIgnoreCase(lastName2);
    }

    private RelationshipSuggestion scoreSpouse(
            Double ageGap,
            UUID p1,
            UUID p2,
            Map<UUID, Set<UUID>> parentsOf,
            Map<UUID, Set<UUID>> childrenOf,
            Set<String> existing,
            Set<String> existingReverse) {
        double score = 0.0;
        List<String> reasons = new ArrayList<>();

        // Get person details
        Person person1 = personRepository.findById(p1).orElse(null);
        Person person2 = personRepository.findById(p2).orElse(null);

        if (person1 == null || person2 == null) {
            return new RelationshipSuggestion("spouse", 0.0, Collections.singletonList("Person not found"));
        }

        // Check if same person
        if (p1.equals(p2)) {
            return new RelationshipSuggestion("spouse", 0.0, Collections.singletonList("Same person"));
        }

        // 1. Check gender (only suggest spouse for opposite gender in Vietnamese culture)
        if (person1.getGender() != null && person2.getGender() != null) {
            if (!person1.getGender().equals(person2.getGender())) {
                score += 0.3;
                reasons.add("Different gender");
            } else {
                // Same gender - less likely to be spouse in traditional Vietnamese families
                score -= 0.4;
                reasons.add("Same gender");
            }
        }

        // 2. Check last name (in Vietnam, spouses typically have different last names)
        boolean sameLastName = hasSameLastName(person1.getFullName(), person2.getFullName());
        if (sameLastName) {
            score -= 0.5;
            reasons.add("Same last name (less likely to be spouse)");
        } else {
            score += 0.3;
            reasons.add("Different last names");
        }

        // 3. Age gap heuristic
        if (ageGap != null) {
            if (ageGap <= 10) {
                score += 0.4;
                reasons.add("Age gap <= 10 years");
            } else if (ageGap <= 20) {
                score += 0.2;
                reasons.add("Age gap " + ageGap.intValue() + " years");
            } else {
                score -= 0.3;
                reasons.add("Large age gap " + ageGap.intValue() + " years");
            }
        } else {
            reasons.add("Missing birth dates");
        }

        // 4. Check if they already have children together
        Set<UUID> p1Children = childrenOf.getOrDefault(p1, Collections.emptySet());
        Set<UUID> p2Children = childrenOf.getOrDefault(p2, Collections.emptySet());
        p1Children.retainAll(p2Children);
        if (!p1Children.isEmpty()) {
            score += 0.6;
            reasons.add("Share " + p1Children.size() + " children");
        }

        // 5. Conflict: if they are already parent/child or siblings
        if (existing.contains("parent") || existingReverse.contains("parent") ||
                existing.contains("child") || existingReverse.contains("child") ||
                existing.contains("sibling") || existingReverse.contains("sibling")) {
            score -= 1.0;
            reasons.add("Existing family relationship");
        }

        // 6. Check if already spouses
        if (existing.contains("spouse") || existingReverse.contains("spouse")) {
            score = 0.0;
            reasons.add("Already spouses");
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