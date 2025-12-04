// src/main/java/com/legacymap/backend/service/VoiceTranscriptionService.java
package com.legacymap.backend.service;

import com.fasterxml.jackson.databind.JsonNode;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.client.RestTemplate;

@Service
public class VoiceTranscriptionService {

    @Value("${groq.api.key}")
    private String groqApiKey;

    private final RestTemplate restTemplate = new RestTemplate();

    public String transcribe(byte[] audioBytes, String filename) {
        var body = new LinkedMultiValueMap<String, Object>();
        body.add("file", new ByteArrayResource(audioBytes) {
            @Override public String getFilename() { return filename; }
        });
        body.add("model", "whisper-large-v3");
        body.add("language", "vi");
        body.add("response_format", "json");

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.setBearerAuth(groqApiKey);

        try {
            ResponseEntity<JsonNode> response = restTemplate.exchange(
                    "https://api.groq.com/openai/v1/audio/transcriptions",
                    HttpMethod.POST,
                    new HttpEntity<>(body, headers),
                    JsonNode.class
            );
            return response.getBody().get("text").asText();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}