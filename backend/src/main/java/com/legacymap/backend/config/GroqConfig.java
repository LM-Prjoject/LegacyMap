// src/main/java/com/legacymap/backend/config/GroqConfig.java
package com.legacymap.backend.config;

import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.openai.OpenAiStreamingChatModel;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class GroqConfig {

    @Value("${groq.base.url}")
    private String baseUrl;

    @Value("${groq.api.key}")
    private String apiKey;

    @Value("${groq.model}")
    private String modelName;

    @Bean
    public StreamingChatLanguageModel streamingChatModel() {
        return OpenAiStreamingChatModel.builder()
                .baseUrl(baseUrl) // https://api.groq.com/openai/v1
                .apiKey(apiKey)
                .modelName(modelName)
                .temperature(0.7)
                .maxTokens(1024)
                // Thêm timeout để tránh treo mãi mãi
                .timeout(java.time.Duration.ofSeconds(60))
                .logRequests(false)
                .logResponses(false)
                .build();
    }
}