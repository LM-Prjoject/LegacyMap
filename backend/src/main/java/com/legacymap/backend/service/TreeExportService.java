package com.legacymap.backend.service;

import com.legacymap.backend.dto.export.*;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.RelationshipRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TreeExportService {

    private final FamilyTreeRepository familyTreeRepository;
    private final PersonRepository personRepository;
    private final RelationshipRepository relationshipRepository;
    private final TreePdfService treePdfService;

    public byte[] exportTreeDetailsPdf(UUID treeId, byte[] treeImageBytes) {
        FamilyTree tree = familyTreeRepository.findById(treeId)
                .orElseThrow(() -> new AppException(ErrorCode.TREE_NOT_FOUND));

        List<Person> persons = personRepository.findAllByFamilyTree_Id(treeId);
        List<Relationship> relationships = relationshipRepository.findByFamilyTreeId(treeId);

        TreeExportDTO dto = buildDto(tree, persons, relationships);
        return treePdfService.generateDetailsPdf(dto, treeImageBytes);
    }

    private TreeExportDTO buildDto(
            FamilyTree tree,
            List<Person> persons,
            List<Relationship> relationships
    ) {
        long maleCount = persons.stream()
                .filter(p -> "male".equalsIgnoreCase(p.getGender()))
                .count();
        long femaleCount = persons.stream()
                .filter(p -> "female".equalsIgnoreCase(p.getGender()))
                .count();
        long otherCount = persons.size() - maleCount - femaleCount;

        long aliveCount = persons.stream()
                .filter(p -> p.getDeathDate() == null)
                .count();
        long deceasedCount = persons.size() - aliveCount;

        int generationCount = computeGenerationCount(persons, relationships);

        var owner = tree.getCreatedBy();

        TreeExportSummary summary = new TreeExportSummary(
                tree.getId().toString(),
                tree.getName(),
                tree.getDescription(),
                owner != null ? owner.getUsername() : null,
                owner != null ? owner.getEmail() : null,
                tree.getCreatedAt(),
                persons.size(),
                generationCount,
                maleCount,
                femaleCount,
                otherCount,
                aliveCount,
                deceasedCount
        );

        List<MemberExportRow> memberRows = buildMemberRows(persons, relationships);
        return new TreeExportDTO(summary, memberRows);
    }


    private List<MemberExportRow> buildMemberRows(
            List<Person> persons,
            List<Relationship> relationships
    ) {
        Map<UUID, Integer> generationMap = computeGenerationMap(persons, relationships);

        List<Person> sorted = new ArrayList<>(persons);
        sorted.sort(
                Comparator.comparing(
                                (Person p) -> generationMap.getOrDefault(p.getId(), Integer.MAX_VALUE))
                        .thenComparing(Person::getFullName, String.CASE_INSENSITIVE_ORDER)
        );

        List<MemberExportRow> rows = new ArrayList<>();
        int i = 1;
        for (Person p : sorted) {
            String birthYear = extractYear(p.getBirthDate());
            String deathYear = extractYear(p.getDeathDate());

            Integer gen = generationMap.getOrDefault(p.getId(), null);
            StringBuilder role = new StringBuilder();
            if (gen != null) {
                role.append("Đời ").append(gen);
            }

            rows.add(new MemberExportRow(
                    i++,
                    p.getFullName(),
                    normalizeGender(p.getGender()),
                    birthYear,
                    deathYear,
                    role.toString()
            ));
        }
        return rows;
    }

    private int computeGenerationCount(List<Person> persons, List<Relationship> relationships) {
        Map<UUID, Integer> map = computeGenerationMap(persons, relationships);
        return map.values().stream().mapToInt(Integer::intValue).max().orElse(1);
    }

    private Map<UUID, Integer> computeGenerationMap(
            List<Person> persons,
            List<Relationship> relationships
    ) {
        Map<UUID, List<UUID>> parentToChildren = new HashMap<>();
        Map<UUID, List<UUID>> childToParents = new HashMap<>();

        for (Relationship r : relationships) {
            Object rawType = r.getRelationshipType();
            String type = safeType(rawType != null ? rawType.toString() : null);
            if (!"PARENT".equals(type) && !"CHILD".equals(type)) {
                continue;
            }

            UUID id1 = r.getPerson1().getId();
            UUID id2 = r.getPerson2().getId();

            UUID parentId;
            UUID childId;

            if ("PARENT".equals(type)) {
                parentId = id1;
                childId = id2;
            } else {
                parentId = id2;
                childId = id1;
            }

            parentToChildren
                    .computeIfAbsent(parentId, k -> new ArrayList<>())
                    .add(childId);

            childToParents
                    .computeIfAbsent(childId, k -> new ArrayList<>())
                    .add(parentId);
        }

        Map<UUID, Integer> gen = new HashMap<>();

        Set<UUID> rootIds = new HashSet<>();
        for (Person p : persons) {
            UUID id = p.getId();
            if (!childToParents.containsKey(id)) {
                rootIds.add(id);
            }
        }

        if (rootIds.isEmpty() && !persons.isEmpty()) {
            Person oldest = persons.stream()
                    .filter(p -> p.getBirthDate() != null)
                    .min(Comparator.comparing(Person::getBirthDate))
                    .orElse(persons.get(0));
            rootIds.add(oldest.getId());
        }

        Deque<UUID> queue = new ArrayDeque<>();
        for (UUID root : rootIds) {
            gen.put(root, 1);
            queue.add(root);
        }

        while (!queue.isEmpty()) {
            UUID parent = queue.poll();
            int parentGen = gen.get(parent);
            for (UUID child : parentToChildren.getOrDefault(parent, List.of())) {
                int candidate = parentGen + 1;
                Integer current = gen.get(child);
                if (current == null || candidate < current) {
                    gen.put(child, candidate);
                    queue.add(child);
                }
            }
        }

        for (Person p : persons) {
            gen.putIfAbsent(p.getId(), 1);
        }

        return gen;
    }

    private String extractYear(LocalDate date) {
        if (date == null) return null;
        return String.valueOf(date.getYear());
    }

    private String normalizeGender(String g) {
        if (g == null) return "";
        String s = g.toLowerCase(Locale.ROOT);
        return switch (s) {
            case "male" -> "Nam";
            case "female" -> "Nữ";
            default -> "Khác";
        };
    }

    private String safeType(String raw) {
        if (raw == null) return "";
        return raw.trim().toUpperCase(Locale.ROOT);
    }
}