package com.legacymap.backend.service.ai;

import com.legacymap.backend.dto.response.RelationshipSuggestion;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.Period;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class GroqSuggestClient {

    private final WebClient webClient;

    @Value("${GROQ_API_KEY:}")
    private String apiKey;

    @Value("${groq.model:llama3-8b-8192}")
    private String configuredModel;

    public GroqSuggestClient(WebClient.Builder builder) {
        this.webClient = builder
                .baseUrl("https://api.groq.com")
                .build();
    }

    public Optional<List<RelationshipSuggestion>> suggest(FamilyTree tree,
                                                          Person p1,
                                                          Person p2,
                                                          List<Relationship> rels) {
        try {
            if (apiKey == null || apiKey.isBlank()) {
                log.warn("GROQ_API_KEY is not set; skipping AI suggestions");
                return Optional.empty();
            }

            Map<String, Object> payload = buildRequestPayload(tree, p1, p2, rels);

            Map<?,?> response = this.webClient.post()
                    .uri("/openai/v1/chat/completions")
                    .header("Authorization", "Bearer " + apiKey)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .body(BodyInserters.fromValue(payload))
                    .exchangeToMono(clientResponse -> {
                        if (clientResponse.statusCode().is2xxSuccessful()) {
                            return clientResponse.bodyToMono(Map.class);
                        } else {
                            return clientResponse.bodyToMono(String.class)
                                    .defaultIfEmpty("")
                                    .flatMap(body -> {
                                        log.warn("Groq HTTP {} body: {}", clientResponse.rawStatusCode(), body);
                                        return Mono.empty();
                                    });
                        }
                    })
                    .timeout(java.time.Duration.ofSeconds(15))
                    .onErrorResume(ex -> {
                        log.warn("Groq API call failed: {}", ex.toString());
                        return Mono.empty();
                    })
                    .blockOptional()
                    .orElse(null);

            if (response == null) return Optional.empty();

            List<RelationshipSuggestion> parsed = parseResponse(response);
            if (parsed == null || parsed.isEmpty()) return Optional.empty();

            List<RelationshipSuggestion> filtered = applyPostRules(parsed, p1, p2, rels);
            return filtered.isEmpty() ? Optional.empty() : Optional.of(filtered);
        } catch (Exception e) {
            log.warn("Groq suggestion error: {}", e.toString());
            return Optional.empty();
        }
    }

    public Map<UUID, RelationshipSuggestion> suggestForSourceBatch(
            FamilyTree tree,
            Person source,
            List<Person> candidates,
            List<Relationship> rels
    ) {
        Map<UUID, RelationshipSuggestion> result = new HashMap<>();
        try {
            if (apiKey == null || apiKey.isBlank()) {
                log.warn("GROQ_API_KEY is not set; skipping AI suggestions (batch)");
                return result;
            }

            Map<String, Object> payload = buildBatchRequestPayload(tree, source, candidates, rels);

            Map<?,?> response = this.webClient.post()
                    .uri("/openai/v1/chat/completions")
                    .header("Authorization", "Bearer " + apiKey)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .body(BodyInserters.fromValue(payload))
                    .exchangeToMono(clientResponse -> {
                        if (clientResponse.statusCode().is2xxSuccessful()) {
                            return clientResponse.bodyToMono(Map.class);
                        } else {
                            return clientResponse.bodyToMono(String.class)
                                    .defaultIfEmpty("")
                                    .flatMap(body -> {
                                        log.warn("Groq (batch) HTTP {} body: {}", clientResponse.rawStatusCode(), body);
                                        return Mono.empty();
                                    });
                        }
                    })
                    .timeout(java.time.Duration.ofSeconds(25))
                    .onErrorResume(ex -> {
                        log.warn("Groq API call failed (batch): {}", ex.toString());
                        return Mono.empty();
                    })
                    .blockOptional()
                    .orElse(null);

            if (response == null) return result;

            Map<UUID, RelationshipSuggestion> parsed = parseBatchResponse(response);
            if (parsed == null || parsed.isEmpty()) return result;

            for (Map.Entry<UUID, RelationshipSuggestion> e : parsed.entrySet()) {
                UUID cid = e.getKey();
                Person cand = candidates.stream().filter(p -> Objects.equals(p.getId(), cid)).findFirst().orElse(null);
                if (cand == null) continue;
                List<RelationshipSuggestion> filtered = applyPostRules(Collections.singletonList(e.getValue()), source, cand, rels);
                if (!filtered.isEmpty()) result.put(cid, filtered.get(0));
            }
            return result;
        } catch (Exception e) {
            log.warn("Groq batch suggestion error: {}", e.toString());
            return result;
        }
    }

    private Map<String, Object> buildRequestPayload(FamilyTree tree, Person p1, Person p2, List<Relationship> rels) {
        String prompt = buildPrompt(tree, p1, p2, rels);
        Map<String, Object> msg = new HashMap<>();
        msg.put("role", "user");
        msg.put("content", prompt);
        Map<String, Object> payload = new HashMap<>();
        payload.put("model", configuredModel);
        payload.put("messages", List.of(msg));
        payload.put("temperature", 0.1);
        return payload;
    }

    private Map<String, Object> buildBatchRequestPayload(FamilyTree tree, Person source, List<Person> candidates, List<Relationship> rels) {
        String prompt = buildBatchPrompt(tree, source, candidates, rels);
        Map<String, Object> msg = new HashMap<>();
        msg.put("role", "user");
        msg.put("content", prompt);
        Map<String, Object> payload = new HashMap<>();
        payload.put("model", configuredModel);
        payload.put("messages", List.of(msg));
        payload.put("temperature", 0.1);
        return payload;
    }

    private String buildPrompt(FamilyTree tree, Person p1, Person p2, List<Relationship> rels) {
        String p1Info = formatPerson(p1);
        String p2Info = formatPerson(p2);
        List<String> edges = rels.stream()
                .filter(r -> Objects.equals(r.getPerson1().getId(), p1.getId())
                        || Objects.equals(r.getPerson2().getId(), p1.getId())
                        || Objects.equals(r.getPerson1().getId(), p2.getId())
                        || Objects.equals(r.getPerson2().getId(), p2.getId()))
                .map(r -> String.format("%s --(%s)--> %s",
                        safeName(r.getPerson1()), r.getRelationshipType(), safeName(r.getPerson2())))
                .collect(Collectors.toList());

        String instructions = "You are an assistant that infers likely family relationships in a Vietnamese family tree.\n\n" +
                "Follow the SAME RULES as before (age, surname, existing relations).\n" +
                "Evaluate the relationship BETWEEN the SOURCE and EACH CANDIDATE.\n" +
                "OUTPUT strictly JSON array, each item: {\"candidateId\": string, \"type\": \"parent|child|spouse|sibling\", \"confidence\": 0..1, \"reasons\": [string]}.\n" +
                "Return at most 1 best item per candidate. If none valid for a candidate, omit it.";

        String context = "Tree: " + (tree.getName() == null ? "(unnamed)" : tree.getName()) + "\n" +
                "Person1: " + p1Info + "\n" +
                "Person2: " + p2Info + "\n" +
                "Existing edges near them:\n" + String.join("\n", edges) + "\n" +
                "Return JSON only, no prose.";

        return instructions + "\n\n" + context;
    }

    private String buildBatchPrompt(FamilyTree tree, Person source, List<Person> candidates, List<Relationship> rels) {
        String srcInfo = formatPerson(source);
        List<String> catalog = candidates.stream().map(this::formatPerson).collect(Collectors.toList());
        Set<UUID> ids = new HashSet<>();
        ids.add(source.getId());
        for (Person c : candidates) ids.add(c.getId());
        List<String> edges = rels.stream()
                .filter(r -> ids.contains(r.getPerson1().getId()) || ids.contains(r.getPerson2().getId()))
                .map(r -> String.format("%s --(%s)--> %s",
                        safeName(r.getPerson1()), r.getRelationshipType(), safeName(r.getPerson2())))
                .collect(Collectors.toList());

        String instructions = "You are an assistant that infers likely family relationships in a Vietnamese family tree.\n\n" +
                "Follow the SAME RULES as before (age, surname, existing relations).\n" +
                "Evaluate the relationship BETWEEN the SOURCE and EACH CANDIDATE.\n" +
                "OUTPUT strictly JSON array, each item: {\"candidateId\": string, \"type\": \"parent|child|spouse|sibling\", \"confidence\": 0..1, \"reasons\": [string]}.\n" +
                "Return at most 1 best item per candidate. If none valid for a candidate, omit it.";

        String context = "Tree: " + (tree.getName() == null ? "(unnamed)" : tree.getName()) + "\n" +
                "Source: " + srcInfo + "\n" +
                "Candidates (id, name, gender, birthDate):\n- " + String.join("\n- ", catalog) + "\n" +
                "Existing edges near them:\n" + String.join("\n", edges) + "\n" +
                "Return JSON only, no prose.";

        return instructions + "\n\n" + context;
    }

    @SuppressWarnings("unchecked")
    private List<RelationshipSuggestion> parseResponse(Map<?,?> response) {
        try {
            Object choices = response.get("choices");
            if (!(choices instanceof List) || ((List<?>) choices).isEmpty()) return Collections.emptyList();
            Map<?,?> first = (Map<?,?>) ((List<?>) choices).get(0);
            Map<?,?> message = (Map<?,?>) first.get("message");
            String content = Objects.toString(message == null ? null : message.get("content"), "").trim();
            if (content.isEmpty()) return Collections.emptyList();
            String jsonText = extractJsonArray(content);
            com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
            List<Map<String, Object>> arr = om.readValue(jsonText, om.getTypeFactory().constructCollectionType(List.class, Map.class));
            List<RelationshipSuggestion> out = new ArrayList<>();
            for (Map<String, Object> item : arr) {
                String type = Objects.toString(item.get("type"), null);
                Object confObj = item.get("confidence");
                double conf = confObj instanceof Number ? ((Number) confObj).doubleValue() : 0.0;
                List<String> reasons = new ArrayList<>();
                Object rs = item.get("reasons");
                if (rs instanceof List) for (Object r : ((List<?>) rs)) reasons.add(Objects.toString(r));
                if (type != null && (type.equals("parent") || type.equals("child") || type.equals("spouse") || type.equals("sibling"))) {
                    out.add(new RelationshipSuggestion(type, Math.max(0.0, Math.min(1.0, conf)), reasons));
                }
            }
            return out;
        } catch (Exception e) {
            log.warn("Failed to parse Groq response: {}", e.toString());
            return Collections.emptyList();
        }
    }

    @SuppressWarnings("unchecked")
    private Map<UUID, RelationshipSuggestion> parseBatchResponse(Map<?,?> response) {
        Map<UUID, RelationshipSuggestion> out = new HashMap<>();
        try {
            Object choices = response.get("choices");
            if (!(choices instanceof List) || ((List<?>) choices).isEmpty()) return out;
            Map<?,?> first = (Map<?,?>) ((List<?>) choices).get(0);
            Map<?,?> message = (Map<?,?>) first.get("message");
            String content = Objects.toString(message == null ? null : message.get("content"), "").trim();
            if (content.isEmpty()) return out;
            String jsonText = extractJsonArray(content);
            com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
            List<Map<String, Object>> arr = om.readValue(jsonText, om.getTypeFactory().constructCollectionType(List.class, Map.class));
            for (Map<String, Object> item : arr) {
                String idStr = Objects.toString(item.get("candidateId"), null);
                if (idStr == null) continue;
                UUID cid;
                try { cid = UUID.fromString(idStr); } catch (Exception e) { continue; }
                String type = Objects.toString(item.get("type"), null);
                Object confObj = item.get("confidence");
                double conf = confObj instanceof Number ? ((Number) confObj).doubleValue() : 0.0;
                List<String> reasons = new ArrayList<>();
                Object rs = item.get("reasons");
                if (rs instanceof List) for (Object r : ((List<?>) rs)) reasons.add(Objects.toString(r));
                if (type != null && (type.equals("parent") || type.equals("child") || type.equals("spouse") || type.equals("sibling"))) {
                    out.put(cid, new RelationshipSuggestion(type, Math.max(0.0, Math.min(1.0, conf)), reasons));
                }
            }
        } catch (Exception e) {
            log.warn("Failed to parse Groq batch response: {}", e.toString());
        }
        return out;
    }

    private String extractJsonArray(String text) {
        int start = text.indexOf('[');
        int end = text.lastIndexOf(']');
        if (start >= 0 && end > start) return text.substring(start, end + 1);
        return text;
    }

    private String safeName(Person p) {
        try { return p.getFullName() + "(" + p.getId() + ")"; }
        catch (Exception e) { return String.valueOf(p.getId()); }
    }

    private String formatPerson(Person p) {
        return String.format("id=%s, name=%s, gender=%s, birthDate=%s",
                p.getId(), nullToEmpty(p.getFullName()), nullToEmpty(p.getGender()),
                p.getBirthDate() == null ? "" : p.getBirthDate().toString());
    }

    private String nullToEmpty(String s) { return s == null ? "" : s; }

    private List<RelationshipSuggestion> applyPostRules(List<RelationshipSuggestion> in,
                                                        Person p1,
                                                        Person p2,
                                                        List<Relationship> rels) {
        int age1 = estimateAgeYears(p1);
        int age2 = estimateAgeYears(p2);
        Integer diff = (age1 >= 0 && age2 >= 0) ? Math.abs(age1 - age2) : null;
        String s1 = surnameOf(p1.getFullName());
        String s2 = surnameOf(p2.getFullName());
        String g1 = nullToEmpty(p1.getGender()).toLowerCase(Locale.ROOT);
        String g2 = nullToEmpty(p2.getGender()).toLowerCase(Locale.ROOT);

        boolean p1HasParent = hasExistingParent(p1, rels);
        boolean p2HasParent = hasExistingParent(p2, rels);
        boolean pairHasParentLink = hasExistingParentLinkBetweenPair(p1, p2, rels);
        boolean p1HasSpouse = hasExistingSpouse(p1, rels);
        boolean p2HasSpouse = hasExistingSpouse(p2, rels);

        List<RelationshipSuggestion> out = new ArrayList<>();
        for (RelationshipSuggestion rs : in) {
            String type = rs.getType();
            double conf = rs.getConfidence();
            List<String> reasons = new ArrayList<>(rs.getReasons() == null ? List.of() : rs.getReasons());

            boolean allow = true;

            if ("parent".equals(type) || "child".equals(type)) {
                if (diff != null) {
                    if (diff > 100) { allow = false; reasons.add("age difference > 100 => reject parent/child"); }
                    else if (diff < 18 || diff > 80) {
                        allow = false;
                        reasons.add("age gap outside 18..80 for parent/child");
                        boolean canConvertToSibling = !isEmpty(s1) && !isEmpty(s2) && s1.equalsIgnoreCase(s2) && !pairHasParentLink;
                        if (canConvertToSibling) {
                            double sibConf = Math.max(0.0, Math.min(1.0, conf * 0.6));
                            List<String> sibReasons = new ArrayList<>(reasons);
                            sibReasons.add("converted to sibling due to insufficient parent/child age gap");
                            out.add(new RelationshipSuggestion("sibling", sibConf, sibReasons));
                        }
                    }
                }
                if (allow && !isEmpty(s1) && !isEmpty(s2) && !s1.equalsIgnoreCase(s2)) {
                    conf = Math.max(0.0, Math.min(1.0, conf * 0.5));
                    reasons.add("reduced confidence: different surnames for parent/child");
                }
                if (allow) {
                    if ("parent".equals(type) && p2HasParent) allow = false;
                    if ("child".equals(type) && p1HasParent) allow = false;
                }
            } else if ("sibling".equals(type)) {
                if (!isEmpty(s1) && !isEmpty(s2) && !s1.equalsIgnoreCase(s2)) allow = false;
                if (allow && diff != null && Math.abs(diff) >= 18) {
                    boolean ageWithinParentRange = Math.abs(diff) <= 80;
                    boolean sameSurname = !isEmpty(s1) && !isEmpty(s2) && s1.equalsIgnoreCase(s2);
                    if (ageWithinParentRange) {
                        if (sameSurname) {
                            double pcConf = Math.max(0.0, Math.min(1.0, conf * 0.7));
                            List<String> pcReasons = new ArrayList<>(reasons);
                            pcReasons.add("converted from sibling due to large age gap (>=18) suggesting parent/child");
                            if (!p1HasParent) out.add(new RelationshipSuggestion("child", pcConf, pcReasons));
                            if (!p2HasParent) out.add(new RelationshipSuggestion("parent", pcConf, pcReasons));
                            allow = false;
                        } else { allow = false; }
                    } else { allow = false; }
                }
            } else if ("spouse".equals(type)) {
                // Existing spouse constraint
                if (p1HasSpouse || p2HasSpouse) {
                    conf = Math.max(0.0, Math.min(1.0, conf * 0.6));
                    reasons.add("reduced confidence: one or both already has spouse");
                }
                // Apply spouse heuristics consistency (match Gemini logic)
                if (allow) {
                    boolean differentSurname = !isEmpty(s1) && !isEmpty(s2) && !s1.equalsIgnoreCase(s2);
                    boolean oppositeGender = !isEmpty(g1) && !isEmpty(g2) && !g1.equals(g2);
                    boolean agesKnown = (age1 >= 0 && age2 >= 0) && (diff != null);
                    boolean bothAdults = agesKnown && age1 >= 18 && age2 >= 18;
                    boolean ageGapOk = agesKnown && Math.abs(diff) <= 30;
                    if (!(differentSurname && oppositeGender && bothAdults && ageGapOk)) {
                        conf = Math.max(0.0, Math.min(1.0, conf * 0.7));
                        reasons.add("reduced confidence: spouse heuristics not strongly met");
                    }
                }
            }

            if (allow) out.add(new RelationshipSuggestion(type, conf, reasons));
        }

        out.sort(Comparator.comparing(RelationshipSuggestion::getConfidence).reversed());
        return out;
    }

    private int estimateAgeYears(Person p) {
        try {
            LocalDate dob = p.getBirthDate();
            if (dob == null) return -1;
            return Math.max(0, Period.between(dob, LocalDate.now()).getYears());
        } catch (Exception e) { return -1; }
    }

    private String surnameOf(String fullName) {
        String s = nullToEmpty(fullName).trim();
        if (s.isEmpty()) return "";
        int sp = s.indexOf(' ');
        return sp > 0 ? s.substring(0, sp) : s;
    }

    private boolean hasExistingParent(Person target, List<Relationship> rels) {
        UUID tid = target.getId();
        for (Relationship r : rels) {
            String t = nullToEmpty(r.getRelationshipType()).toLowerCase(Locale.ROOT);
            if ("parent".equals(t)) { if (Objects.equals(r.getPerson2().getId(), tid)) return true; }
            if ("child".equals(t)) { if (Objects.equals(r.getPerson1().getId(), tid)) return true; }
        }
        return false;
    }

    private boolean hasExistingParentLinkBetweenPair(Person a, Person b, List<Relationship> rels) {
        UUID aid = a.getId(); UUID bid = b.getId();
        for (Relationship r : rels) {
            String t = nullToEmpty(r.getRelationshipType()).toLowerCase(Locale.ROOT);
            if ("parent".equals(t)) {
                if (Objects.equals(r.getPerson1().getId(), aid) && Objects.equals(r.getPerson2().getId(), bid)) return true;
                if (Objects.equals(r.getPerson1().getId(), bid) && Objects.equals(r.getPerson2().getId(), aid)) return true;
            }
            if ("child".equals(t)) {
                if (Objects.equals(r.getPerson2().getId(), aid) && Objects.equals(r.getPerson1().getId(), bid)) return true;
                if (Objects.equals(r.getPerson2().getId(), bid) && Objects.equals(r.getPerson1().getId(), aid)) return true;
            }
        }
        return false;
    }

    private boolean hasExistingSpouse(Person a, List<Relationship> rels) {
        UUID aid = a.getId();
        for (Relationship r : rels) {
            String t = nullToEmpty(r.getRelationshipType()).toLowerCase(Locale.ROOT);
            if ("spouse".equals(t)) {
                if (Objects.equals(r.getPerson1().getId(), aid) || Objects.equals(r.getPerson2().getId(), aid)) return true;
            }
        }
        return false;
    }

    private boolean isEmpty(String s) { return s == null || s.isEmpty(); }
}
