package com.legacymap.backend.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;

import java.time.Duration;

@Configuration
public class SupabaseConfig {

    @Bean
    public WebClient supabaseAuthWebClient(SupabaseProperties props) {
        HttpClient httpClient = HttpClient.create()
                .responseTimeout(Duration.ofSeconds(15));

        return WebClient.builder()
                .baseUrl(props.authBaseUrl())
                .clientConnector(new ReactorClientHttpConnector(httpClient))
                .defaultHeader("apikey", props.effectiveApiKey())
                .build();
    }
}


