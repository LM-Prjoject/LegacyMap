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

import java.util.*;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.time.Period;

/**
 * Calls Google Gemini to suggest relationships between two persons.
 */
@Service
@Slf4j
public class GeminiSuggestClient {

    private final WebClient webClient;

    @Value("${GEMINI_API_KEY:}")
    private String apiKey;

    @Value("${gemini.model:gemini-2.0-flash}")
    private String configuredModel;

    public GeminiSuggestClient(WebClient.Builder builder) {
        this.webClient = builder
                .baseUrl("https://generativelanguage.googleapis.com")
                .build();
    }

    private String apiPath(String version, String model) {
        return "/" + version + "/models/" + model + ":generateContent";
    }

    public Optional<List<RelationshipSuggestion>> suggest(FamilyTree tree,
                                                          Person p1,
                                                          Person p2,
                                                          List<Relationship> rels) {
        try {
            if (apiKey == null || apiKey.isBlank()) {
                log.warn("GEMINI_API_KEY is not set; skipping AI suggestions");
                return Optional.empty();
            }

            Map<String, Object> payload = buildRequestPayload(tree, p1, p2, rels);

            // Try configured model first, then fallbacks if 404
            List<String> modelsToTry = new ArrayList<>();
            // Prioritize 2.0 first if not already configured
            modelsToTry.add(configuredModel);
            if (!"gemini-2.0-flash".equals(configuredModel)) modelsToTry.add("gemini-2.0-flash");

            for (String model : modelsToTry) {
                // Try v1 first
                Map<?,?> response = this.webClient.post()
                        .uri(uriBuilder -> uriBuilder
                                .path(apiPath("v1", model))
                                .build())
                        .header("X-Goog-Api-Key", apiKey)
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
                                            log.warn("Gemini HTTP {} on v1 model {} body: {}", clientResponse.rawStatusCode(), model, body);
                                            return Mono.empty();
                                        });
                            }
                        })
                        .timeout(java.time.Duration.ofSeconds(12))
                        .onErrorResume(ex -> {
                            log.warn("Gemini API call failed (v1 model {}): {}", model, ex.toString());
                            return Mono.empty();
                        })
                        .blockOptional()
                        .orElse(null);

                if (response != null) {
                    List<RelationshipSuggestion> parsed = parseResponse(response);
                    if (parsed != null && !parsed.isEmpty()) {
                        List<RelationshipSuggestion> filtered = applyPostRules(parsed, p1, p2, rels);
                        if (!filtered.isEmpty()) return Optional.of(filtered);
                    }
                }

                // If v1 failed, try v1beta
                response = this.webClient.post()
                        .uri(uriBuilder -> uriBuilder
                                .path(apiPath("v1beta", model))
                                .build())
                        .header("X-Goog-Api-Key", apiKey)
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
                                            log.warn("Gemini HTTP {} on v1beta model {} body: {}", clientResponse.rawStatusCode(), model, body);
                                            return Mono.empty();
                                        });
                            }
                        })
                        .timeout(java.time.Duration.ofSeconds(12))
                        .onErrorResume(ex -> {
                            log.warn("Gemini API call failed (v1beta model {}): {}", model, ex.toString());
                            return Mono.empty();
                        })
                        .blockOptional()
                        .orElse(null);

                if (response != null) {
                    List<RelationshipSuggestion> parsed = parseResponse(response);
                    if (parsed != null && !parsed.isEmpty()) {
                        List<RelationshipSuggestion> filtered = applyPostRules(parsed, p1, p2, rels);
                        if (!filtered.isEmpty()) return Optional.of(filtered);
                    }
                }
            }

            return Optional.empty();
        } catch (Exception e) {
            log.warn("Gemini suggestion error: {}", e.toString());
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
                log.warn("GEMINI_API_KEY is not set; skipping AI suggestions (batch)");
                return result;
            }

            Map<String, Object> payload = buildBatchRequestPayload(tree, source, candidates, rels);

            List<String> modelsToTry = new ArrayList<>();
            modelsToTry.add(configuredModel);
            if (!"gemini-2.0-flash".equals(configuredModel)) modelsToTry.add("gemini-2.0-flash");

            Map<?,?> response = null;
            for (String model : modelsToTry) {
                response = this.webClient.post()
                        .uri(uriBuilder -> uriBuilder
                                .path(apiPath("v1", model))
                                .build())
                        .header("X-Goog-Api-Key", apiKey)
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
                                            log.warn("Gemini (batch) HTTP {} on v1 model {} body: {}", clientResponse.rawStatusCode(), model, body);
                                            return Mono.empty();
                                        });
                            }
                        })
                        .timeout(java.time.Duration.ofSeconds(18))
                        .onErrorResume(ex -> {
                            log.warn("Gemini API call failed (batch v1 model {}): {}", model, ex.toString());
                            return Mono.empty();
                        })
                        .blockOptional()
                        .orElse(null);

                if (response != null) break;

                response = this.webClient.post()
                        .uri(uriBuilder -> uriBuilder
                                .path(apiPath("v1beta", model))
                                .build())
                        .header("X-Goog-Api-Key", apiKey)
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
                                            log.warn("Gemini (batch) HTTP {} on v1beta model {} body: {}", clientResponse.rawStatusCode(), model, body);
                                            return Mono.empty();
                                        });
                            }
                        })
                        .timeout(java.time.Duration.ofSeconds(18))
                        .onErrorResume(ex -> {
                            log.warn("Gemini API call failed (batch v1beta model {}): {}", model, ex.toString());
                            return Mono.empty();
                        })
                        .blockOptional()
                        .orElse(null);

                if (response != null) break;
            }

            if (response == null) return result;

            Map<UUID, RelationshipSuggestion> parsed = parseBatchResponse(response);
            // Apply post rules per pair
            for (Map.Entry<UUID, RelationshipSuggestion> e : parsed.entrySet()) {
                UUID candId = e.getKey();
                Person cand = candidates.stream().filter(p -> Objects.equals(p.getId(), candId)).findFirst().orElse(null);
                if (cand == null) continue;
                List<RelationshipSuggestion> filtered = applyPostRules(Collections.singletonList(e.getValue()), source, cand, rels);
                if (!filtered.isEmpty()) {
                    result.put(candId, filtered.get(0));
                }
            }
            return result;
        } catch (Exception ex) {
            log.warn("Gemini batch suggestion error: {}", ex.toString());
            return result;
        }
    }

    private Map<String, Object> buildBatchRequestPayload(FamilyTree tree, Person source, List<Person> candidates, List<Relationship> rels) {
        Map<String, Object> userPart = new HashMap<>();
        userPart.put("text", buildBatchPrompt(tree, source, candidates, rels));

        Map<String, Object> content = new HashMap<>();
        content.put("role", "user");
        content.put("parts", List.of(userPart));

        Map<String, Object> payload = new HashMap<>();
        payload.put("contents", List.of(content));
        payload.put("generationConfig", Map.of(
                "temperature", 0.1
        ));
        return payload;
    }

    private String buildBatchPrompt(FamilyTree tree, Person source, List<Person> candidates, List<Relationship> rels) {
        String srcInfo = formatPerson(source);
        List<String> catalog = candidates.stream()
                .map(this::formatPerson)
                .collect(Collectors.toList());

        // Only include edges near source or candidates
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
    private Map<UUID, RelationshipSuggestion> parseBatchResponse(Map<?,?> response) {
        Map<UUID, RelationshipSuggestion> out = new HashMap<>();
        try {
            Object candidatesObj = response.get("candidates");
            if (!(candidatesObj instanceof List)) return out;
            List<?> candidatesList = (List<?>) candidatesObj;
            if (candidatesList.isEmpty()) return out;
            Map<?,?> first = (Map<?,?>) candidatesList.get(0);
            Map<?,?> content = (Map<?,?>) first.get("content");
            List<?> parts = (List<?>) content.get("parts");
            if (parts == null || parts.isEmpty()) return out;
            Map<?,?> part0 = (Map<?,?>) parts.get(0);
            String jsonText = Objects.toString(part0.get("text"), "").trim();
            if (jsonText.isEmpty()) return out;
            jsonText = extractJsonArray(jsonText);
            com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
            List<Map<String, Object>> arr = om.readValue(jsonText, om.getTypeFactory().constructCollectionType(List.class, Map.class));
            for (Map<String, Object> item : arr) {
                String idStr = Objects.toString(item.get("candidateId"), null);
                if (idStr == null) continue;
                UUID cid;
                try { cid = UUID.fromString(idStr); } catch (Exception e) { continue; }
                String type = Objects.toString(item.get("type"), null);
                Object confObj = item.get("confidence");
                double conf = 0.0;
                if (confObj instanceof Number) conf = ((Number) confObj).doubleValue();
                List<String> reasons = new ArrayList<>();
                Object rs = item.get("reasons");
                if (rs instanceof List) {
                    for (Object r : ((List<?>) rs)) reasons.add(Objects.toString(r));
                }
                if (type != null && (type.equals("parent") || type.equals("child") || type.equals("spouse") || type.equals("sibling"))) {
                    out.put(cid, new RelationshipSuggestion(type, Math.max(0.0, Math.min(1.0, conf)), reasons));
                }
            }
        } catch (Exception e) {
            log.warn("Failed to parse Gemini batch response: {}", e.toString());
        }
        return out;
    }

    private Map<String, Object> buildRequestPayload(FamilyTree tree, Person p1, Person p2, List<Relationship> rels) {
        Map<String, Object> userPart = new HashMap<>();
        userPart.put("text", buildPrompt(tree, p1, p2, rels));

        Map<String, Object> content = new HashMap<>();
        content.put("role", "user");
        content.put("parts", List.of(userPart));

        Map<String, Object> payload = new HashMap<>();
        payload.put("contents", List.of(content));
        // Keep generationConfig minimal for compatibility across API versions
        payload.put("generationConfig", Map.of(
                "temperature", 0.1
        ));
        return payload;
    }

    private String buildPrompt(FamilyTree tree, Person p1, Person p2, List<Relationship> rels) {
        String p1Info = formatPerson(p1);
        String p2Info = formatPerson(p2);

        // Only include relationships relevant to p1/p2 to keep prompt small
        List<String> edges = rels.stream()
                .filter(r -> Objects.equals(r.getPerson1().getId(), p1.getId())
                        || Objects.equals(r.getPerson2().getId(), p1.getId())
                        || Objects.equals(r.getPerson1().getId(), p2.getId())
                        || Objects.equals(r.getPerson2().getId(), p2.getId()))
                .map(r -> String.format("%s --(%s)--> %s",
                        safeName(r.getPerson1()), r.getRelationshipType(), safeName(r.getPerson2())))
                .collect(Collectors.toList());

        String instructions = "You are an assistant that infers likely family relationships in a Vietnamese family tree.\n\n" +
                "You MUST strictly follow these rules:\n\n" +
                "AGE RULES:\n" +
                "- A parent must be 18 to 80 years older than a child.\n" +
                "- If age difference is < 18 or > 80 → parent/child is impossible (confidence = 0).\n" +
                "- If a person is older by more than 100 years → reject parent/child completely.\n\n" +
                "SURNAME RULES:\n" +
                "- If two persons have DIFFERENT Vietnamese surnames → parent/child is NOT allowed unless the context already shows an adoption.\n" +
                "- If different surnames → sibling is NOT allowed.\n" +
                "- If different surnames → spouse is allowed.\n\n" +
                "GENDER RULES:\n" +
                "- Parent/child, siblings, and spouses may be any gender.\n\n" +
                "EXISTING RELATIONSHIP RULES:\n" +
                "- If a person already has a father or mother → do NOT suggest another parent.\n" +
                "- If a person already has a spouse → do NOT suggest another spouse.\n\n" +
                "OUTPUT RULES:\n" +
                "- Return ONLY a JSON array (no markdown, no text).\n" +
                "- Each item must be: {\"type\": \"parent|child|spouse|sibling\", \"confidence\": 0..1, \"reasons\": [string]}.\n" +
                "- Sort by confidence desc.\n" +
                "- If all relationships violate the rules → return [].";

        String context = "Tree: " + (tree.getName() == null ? "(unnamed)" : tree.getName()) + "\n" +
                "Person1: " + p1Info + "\n" +
                "Person2: " + p2Info + "\n" +
                "Existing edges near them:\n" + String.join("\n", edges) + "\n" +
                "Return JSON only, no prose.";

        return instructions + "\n\n" + context;
    }

    private String safeName(Person p) {
        try {
            return p.getFullName() + "(" + p.getId() + ")";
        } catch (Exception e) {
            return String.valueOf(p.getId());
        }
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

        if (log.isInfoEnabled()) {
            log.info("PostFilter context: age1={}, age2={}, diff={}, s1='{}', s2='{}', g1='{}', g2='{}', p1HasParent={}, p2HasParent={}, pairHasParentLink={}, p1HasSpouse={}, p2HasSpouse={}",
                    age1, age2, diff, s1, s2, g1, g2, p1HasParent, p2HasParent, pairHasParentLink, p1HasSpouse, p2HasSpouse);
        }

        List<RelationshipSuggestion> out = new ArrayList<>();
        for (RelationshipSuggestion rs : in) {
            String type = rs.getType();
            double conf = rs.getConfidence();
            List<String> reasons = new ArrayList<>(rs.getReasons() == null ? List.of() : rs.getReasons());

            boolean allow = true;
            StringBuilder reasonDbg = new StringBuilder();

            if ("parent".equals(type) || "child".equals(type)) {
                // Age hard rules
                if (diff != null) {
                    if (diff > 100) {
                        allow = false;
                        reasons.add("age difference > 100 => reject parent/child");
                    } else if (diff < 18 || diff > 80) {
                        // Reject P/C by age, but consider converting to sibling if surnames match
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
                // Surname rule (no adoption context available → enforce)
                if (allow && !isEmpty(s1) && !isEmpty(s2) && !s1.equalsIgnoreCase(s2)) {
                    allow = false;
                }
                // Gender is not restrictive for parent/child
                // Existing parent constraints
                if (allow) {
                    if ("parent".equals(type) && p2HasParent) allow = false; // p1 as parent of p2
                    if ("child".equals(type) && p1HasParent) allow = false;  // p2 as parent of p1
                }
            } else if ("sibling".equals(type)) {
                // Different surnames cannot be siblings
                if (!isEmpty(s1) && !isEmpty(s2) && !s1.equalsIgnoreCase(s2)) {
                    allow = false;
                }
                // Prefer parent/child when age gap indicates different generation
                if (allow && diff != null && Math.abs(diff) >= 18) {
                    boolean ageWithinParentRange = Math.abs(diff) <= 80;
                    boolean sameSurname = !isEmpty(s1) && !isEmpty(s2) && s1.equalsIgnoreCase(s2);
                    if (ageWithinParentRange) {
                        if (sameSurname) {
                            // Convert sibling -> parent/child depending on who is older
                            double pcConf = Math.max(0.0, Math.min(1.0, conf * 0.7));
                            List<String> pcReasons = new ArrayList<>(reasons);
                            pcReasons.add("converted from sibling due to large age gap (>=18) suggesting parent/child");
                            if (diff > 0) {
                                // p2 older than p1 => p1 is child of p2
                                if (!p1HasParent) {
                                    out.add(new RelationshipSuggestion("child", pcConf, pcReasons));
                                }
                            } else if (diff < 0) {
                                // p1 older than p2 => p1 is parent of p2
                                if (!p2HasParent) {
                                    out.add(new RelationshipSuggestion("parent", pcConf, pcReasons));
                                }
                            }
                            // In any case, don't keep sibling for large age gap
                            allow = false;
                        } else {
                            // Different surnames and large gap: do not keep sibling
                            allow = false;
                        }
                    } else {
                        // Very large gap (>80): siblings not plausible
                        allow = false;
                    }
                }
            } else if ("spouse".equals(type)) {
                // Existing spouse constraint
                if (p1HasSpouse || p2HasSpouse) allow = false;
            }

            if (allow) {
                out.add(new RelationshipSuggestion(type, conf, reasons));
            }
        }

        // Only drop sibling if there exists a strong parent/child suggestion (confidence >= 0.7)
        boolean hasStrongParentChild = out.stream()
                .filter(r -> "parent".equals(r.getType()) || "child".equals(r.getType()))
                .anyMatch(r -> r.getConfidence() >= 0.7);
        if (hasStrongParentChild) {
            out.removeIf(r -> "sibling".equals(r.getType()));
        }

        // Fallback: if nothing left, consider spouse when different surnames and both are single with plausible age gap
        if (out.isEmpty()) {
            boolean differentSurname = !isEmpty(s1) && !isEmpty(s2) && !s1.equalsIgnoreCase(s2);
            boolean bothSingle = !p1HasSpouse && !p2HasSpouse;
            boolean plausibleAgeForSpouse = (diff == null) || Math.abs(diff) <= 40; // allow unknown or within 40 years
            if (differentSurname && bothSingle && plausibleAgeForSpouse) {
                out.add(new RelationshipSuggestion(
                        "spouse",
                        0.6,
                        java.util.List.of("fallback spouse: different surnames and plausible age gap")
                ));
            }
        }

        // Sort desc as safety
        out.sort(Comparator.comparing(RelationshipSuggestion::getConfidence).reversed());
        if (log.isInfoEnabled()) {
            String summary = out.stream()
                    .map(r -> r.getType() + "(" + String.format(java.util.Locale.ROOT, "%.3f", r.getConfidence()) + ")")
                    .collect(Collectors.joining(", "));
            log.info("Final suggestions (sorted): {}", summary);
        }
        return out;
    }

    private int estimateAgeYears(Person p) {
        try {
            LocalDate dob = p.getBirthDate();
            if (dob == null) return -1;
            return Math.max(0, Period.between(dob, LocalDate.now()).getYears());
        } catch (Exception e) {
            return -1;
        }
    }

    private String surnameOf(String fullName) {
        String s = nullToEmpty(fullName).trim();
        if (s.isEmpty()) return "";
        int sp = s.indexOf(' ');
        return sp > 0 ? s.substring(0, sp) : s; // first token as Vietnamese surname
    }

    private boolean hasExistingParent(Person target, List<Relationship> rels) {
        UUID tid = target.getId();
        for (Relationship r : rels) {
            String t = nullToEmpty(r.getRelationshipType()).toLowerCase(Locale.ROOT);
            if ("parent".equals(t)) {
                // r.person1 --(parent)--> r.person2 means person1 is parent of person2
                if (Objects.equals(r.getPerson2().getId(), tid)) return true;
            }
            if ("child".equals(t)) {
                // r.person1 --(child)--> r.person2 means person1 is child of person2 → r.person2 is parent of r.person1
                if (Objects.equals(r.getPerson1().getId(), tid)) return true;
            }
        }
        return false;
    }

    private boolean hasExistingParentLinkBetweenPair(Person a, Person b, List<Relationship> rels) {
        UUID aid = a.getId();
        UUID bid = b.getId();
        for (Relationship r : rels) {
            String t = nullToEmpty(r.getRelationshipType()).toLowerCase(Locale.ROOT);
            UUID p1 = r.getPerson1().getId();
            UUID p2 = r.getPerson2().getId();
            if ("parent".equals(t)) {
                // parent link directly between pair regardless of direction
                if ((Objects.equals(p1, aid) && Objects.equals(p2, bid)) ||
                        (Objects.equals(p1, bid) && Objects.equals(p2, aid))) return true;
            }
            if ("child".equals(t)) {
                // child also implies a parent/child relation between the pair
                if ((Objects.equals(p1, aid) && Objects.equals(p2, bid)) ||
                        (Objects.equals(p1, bid) && Objects.equals(p2, aid))) return true;
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

    private boolean isEmpty(String s) { return s == null || s.isBlank(); }

    @SuppressWarnings("unchecked")
    private List<RelationshipSuggestion> parseResponse(Map<?,?> response) {
        try {
            // For Gemini v1beta generateContent, text lives under candidates[0].content.parts[0].text
            Object candidatesObj = response.get("candidates");
            if (!(candidatesObj instanceof List)) return Collections.emptyList();
            List<?> candidates = (List<?>) candidatesObj;
            if (candidates.isEmpty()) {
                log.warn("Gemini returned empty candidates: {}", response);
                return Collections.emptyList();
            }
            Map<?,?> first = (Map<?,?>) candidates.get(0);
            Map<?,?> content = (Map<?,?>) first.get("content");
            List<?> parts = (List<?>) content.get("parts");
            if (parts == null || parts.isEmpty()) {
                log.warn("Gemini content.parts is empty: {}", first);
                return Collections.emptyList();
            }
            Map<?,?> part0 = (Map<?,?>) parts.get(0);
            String jsonText = Objects.toString(part0.get("text"), "").trim();
            if (jsonText.isEmpty()) return Collections.emptyList();
            jsonText = extractJsonArray(jsonText);
            if (log.isInfoEnabled()) {
                String preview = jsonText.length() > 600 ? jsonText.substring(0, 600) + "..." : jsonText;
                log.info("Gemini raw JSON text preview: {}", preview);
            }

            // naive JSON parse via Jackson-less approach is hard; rely on WebFlux default mapper available via Mono to map. Here we do a minimal parse using org.springframework mapping
            // But since we didn't include a JSON parser explicitly, try to coerce using WebFlux's ObjectMapper through readValue would require injecting. Simpler: restrict model to output a Java-like map isn't feasible.
            // Instead, attempt very small parsing: if it starts with '[' and contains type/confidence.
            // For robustness, fallback empty if parsing fails.

            // Use Jackson if present on classpath (spring-boot-starter-web includes it). Try dynamic parse:
            com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
            List<Map<String, Object>> arr = om.readValue(jsonText, om.getTypeFactory().constructCollectionType(List.class, Map.class));

            List<RelationshipSuggestion> out = new ArrayList<>();
            for (Map<String, Object> item : arr) {
                String type = Objects.toString(item.get("type"), null);
                Object confObj = item.get("confidence");
                double conf = 0.0;
                if (confObj instanceof Number) conf = ((Number) confObj).doubleValue();
                List<String> reasons = new ArrayList<>();
                Object rs = item.get("reasons");
                if (rs instanceof List) {
                    for (Object r : ((List<?>) rs)) reasons.add(Objects.toString(r));
                }
                if (type != null && (type.equals("parent") || type.equals("child") || type.equals("spouse") || type.equals("sibling"))) {
                    out.add(new RelationshipSuggestion(type, Math.max(0.0, Math.min(1.0, conf)), reasons));
                }
            }
            // sort desc
            out.sort(Comparator.comparing(RelationshipSuggestion::getConfidence).reversed());
            return out;
        } catch (Exception e) {
            log.warn("Failed to parse Gemini response: {}", e.toString());
            return Collections.emptyList();
        }
    }

    private String extractJsonArray(String text) {
        // Remove markdown code fences if present
        String t = text.trim();
        if (t.startsWith("```")) {
            // strip first line and trailing fence
            int firstNewline = t.indexOf('\n');
            if (firstNewline > 0) {
                t = t.substring(firstNewline + 1);
            }
            if (t.endsWith("```")) {
                t = t.substring(0, t.length() - 3);
            }
            t = t.trim();
        }
        // Find first '[' and last ']' to isolate array
        int start = t.indexOf('[');
        int end = t.lastIndexOf(']');
        if (start >= 0 && end > start) {
            return t.substring(start, end + 1).trim();
        }
        // If it's already an object with key, try to pull its array value (simple heuristic)
        // Otherwise, return original text; JSON parse will fail and caller will handle.
        return t;
    }
}
