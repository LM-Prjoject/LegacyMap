package com.legacymap.backend.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.config.SupabaseProperties;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class SupabaseAuthService {

    private final SupabaseProperties props;
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final HttpClient http = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(10))
            .build();

    public void sendPasswordResetEmail(@NotBlank String email, @NotBlank String redirectTo) {
        try {
            String url = props.url() + "/auth/v1/recover";
            String body = objectMapper.writeValueAsString(Map.of(
                    "email", email,
                    "redirect_to", redirectTo
            ));
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(Duration.ofSeconds(15))
                    .header("apikey", props.anonKey())
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + props.anonKey())
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .POST(HttpRequest.BodyPublishers.ofString(body))
                    .build();
            HttpResponse<String> resp = http.send(request, HttpResponse.BodyHandlers.ofString());
            if (resp.statusCode() >= 300) {
                throw new RuntimeException("Supabase recover failed: " + resp.statusCode() + " - " + resp.body());
            }
        } catch (Exception e) {
            throw new RuntimeException("Failed to call Supabase recover", e);
        }
    }

    public void updatePasswordWithAccessToken(@NotBlank String accessToken, @NotBlank String newPassword) {
        try {
            String url = props.url() + "/auth/v1/user";
            String body = objectMapper.writeValueAsString(Map.of("password", newPassword));
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(Duration.ofSeconds(15))
                    .header("apikey", props.anonKey())
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + accessToken)
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .PUT(HttpRequest.BodyPublishers.ofString(body))
                    .build();
            HttpResponse<String> resp = http.send(request, HttpResponse.BodyHandlers.ofString());
            if (resp.statusCode() >= 300) {
                throw new RuntimeException("Supabase password update failed: " + resp.statusCode() + " - " + resp.body());
            }
        } catch (Exception e) {
            throw new RuntimeException("Failed to call Supabase update password", e);
        }
    }
}
