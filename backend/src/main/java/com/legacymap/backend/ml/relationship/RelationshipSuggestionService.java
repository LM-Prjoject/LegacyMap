package com.legacymap.backend.ml.relationship;

import com.legacymap.backend.entity.Person;
import com.legacymap.backend.enums.RelationshipType;
import com.legacymap.backend.repository.PersonRepository;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
public class RelationshipSuggestionService {

    private final RelationshipModel model;
    private final PersonRepository personRepository;

    public RelationshipSuggestionService(RelationshipModel model, PersonRepository personRepository) {
        this.model = model;
        this.personRepository = personRepository;
    }

    public List<RelationshipSuggestion> suggest(Person newPerson, int topK, double minConfidence) {
        if (newPerson == null || newPerson.getFamilyTree() == null) return List.of();
        UUID treeId = newPerson.getFamilyTree().getId();
        if (treeId == null) return List.of();

        List<Person> candidates = personRepository.findAllByFamilyTree_Id(treeId);
        return rank(newPerson, candidates, topK, minConfidence);
    }

    private List<RelationshipSuggestion> rank(Person source, List<Person> candidates, int topK, double minConfidence) {
        List<RelationshipSuggestion> results = new ArrayList<>();
        for (Person c : candidates) {
            if (source.getId() != null && source.getId().equals(c.getId())) continue;
            EnumMap<RelationshipType, Double> probs = model.predict(source, c);
            Map.Entry<RelationshipType, Double> best = probs.entrySet().stream()
                    .max(Map.Entry.comparingByValue()).orElse(null);
            if (best == null) continue;
            double conf = best.getValue() == null ? 0.0 : best.getValue();
            if (conf >= minConfidence) {
                results.add(new RelationshipSuggestion(c.getId(), best.getKey(), conf));
            }
        }
        return results.stream()
                .sorted(Comparator.comparingDouble(RelationshipSuggestion::getConfidence).reversed())
                .limit(Math.max(1, topK))
                .collect(Collectors.toList());
    }
}
