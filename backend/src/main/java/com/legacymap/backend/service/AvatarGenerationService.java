package com.legacymap.backend.service;

import okhttp3.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.Period;
import java.util.concurrent.TimeUnit;

@Service
public class AvatarGenerationService {

    private static final Logger log = LoggerFactory.getLogger(AvatarGenerationService.class);

    private final SupabaseStorageService storageService;
    private final OkHttpClient http;

    @Value("${huggingface.token:}")
    private String hfToken;

    @Value("${avatar.hf.endpoint}")
    private String hfEndpoint;

    public AvatarGenerationService(SupabaseStorageService storageService) {
        this.storageService = storageService;
        this.http = new OkHttpClient.Builder()
                .connectTimeout(10, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .writeTimeout(10, TimeUnit.SECONDS)
                .build();
    }

    /**
     * fullName chỉ dùng để log, KHÔNG đưa vào prompt để tránh lộ dữ liệu cá nhân.
     */
    public String generateAvatar(String fullName,
                                 String gender,
                                 LocalDate birthDate,
                                 LocalDate deathDate) {
        if (hfToken == null || hfToken.isBlank()) {
            log.warn("HuggingFace token is not configured, skip AI avatar for '{}'", fullName);
            return null;
        }

        try {
            String prompt = buildPrompt(gender, birthDate, deathDate);
            log.debug("Generating HF avatar for '{}' with prompt: {}", fullName, prompt);

            String jsonPayload = toJson(prompt);

            RequestBody body = RequestBody.create(
                    jsonPayload,
                    MediaType.parse("application/json")
            );

            Request request = new Request.Builder()
                    .url(hfEndpoint)
                    .addHeader("Accept", "image/png")                 // trả về PNG
                    .addHeader("Authorization", "Bearer " + hfToken)  // HF token
                    .post(body)
                    .build();

            try (Response response = http.newCall(request).execute()) {
                if (!response.isSuccessful() || response.body() == null) {
                    String err = response.body() != null ? response.body().string() : "(no body)";
                    log.warn("HF API error: code={} msg={} body={}",
                            response.code(), response.message(), err);
                    return null; // FE dùng default avatar
                }

                byte[] imageBytes = response.body().bytes();

                String fileName = "member-ai-" + System.currentTimeMillis() + ".png";
                String url = storageService.uploadAvatar(imageBytes, fileName, "image/png");

                log.info("Avatar generated for '{}' - uploaded: {}", fullName, url);
                return url;
            }

        } catch (Exception e) {
            log.error("Avatar generation failed for '{}'", fullName, e);
            return null;
        }
    }

    /**
     * Build prompt từ giới tính + nhóm tuổi (suy luận từ birth/death date).
     */
    private String buildPrompt(String gender, LocalDate birthDate, LocalDate deathDate) {
        String genderText = switch (gender == null ? "" : gender.toLowerCase()) {
            case "male"   -> "Vietnamese man";
            case "female" -> "Vietnamese woman";
            default       -> "Vietnamese person";
        };

        String ageGroup = toAgeGroup(birthDate, deathDate);

        return "ultra-realistic portrait photo of " + ageGroup + " " + genderText +
                ", natural skin texture, detailed facial features, cinematic lighting, " +
                "soft pastel background, portrait photography, 8k realism, " +
                "no cartoon, no vector, no illustration, no watermark";
    }

    private String toAgeGroup(LocalDate birthDate, LocalDate deathDate) {
        if (birthDate == null) {
            return "an adult";
        }

        LocalDate ref = (deathDate != null) ? deathDate : LocalDate.now();
        int years = Period.between(birthDate, ref).getYears();
        if (years < 0) years = 0;
        if (years > 120) years = 120;

        if (years < 12) {
            return "a young child";
        } else if (years < 20) {
            return "a teenager";
        } else if (years < 35) {
            return "a young adult";
        } else if (years < 60) {
            return "a middle-aged";
        } else {
            return "an elderly";
        }
    }

    private String toJson(String prompt) {
        String esc = prompt
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", " ")
                .replace("\r", " ");
        return "{ \"inputs\": \"" + esc + "\" }";
    }
}
