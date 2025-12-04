// src/main/java/com/legacymap/backend/service/TtsService.java
package com.legacymap.backend.service;

import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

@Service
public class TtsService {

    private final RestTemplate restTemplate = new RestTemplate();

    public byte[] synthesize(String text) {
        try {
            // Ưu tiên: ElevenLabs miễn phí (nếu bạn có key)
            String elevenKey = System.getenv("ELEVENLABS_API_KEY");
            if (elevenKey != null && !elevenKey.isBlank()) {
                // bạn có thể thêm ElevenLabs ở đây nếu muốn
            }

            // Fallback: Google TTS miễn phí vô hạn + tiếng Việt cực chuẩn
            String encoded = URLEncoder.encode(text, StandardCharsets.UTF_8);
            String url = "https://translate.google.com/translate_tts?ie=UTF-8&tl=vi&client=tw-ob&ttsspeed=1&q=" + encoded;

            return restTemplate.getForObject(url, byte[].class);
        } catch (Exception e) {
            return new byte[0];
        }
    }
}