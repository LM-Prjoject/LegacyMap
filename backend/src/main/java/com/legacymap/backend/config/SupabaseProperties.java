package com.legacymap.backend.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "supabase")
public record SupabaseProperties(
        String url,
        String anonKey,
        String serviceRoleKey
) {
    public String authBaseUrl() {
        if (url == null || url.isBlank()) return null;
        return url.endsWith("/") ? url + "auth/v1" : url + "/auth/v1";
    }

    public String effectiveApiKey() {
        return (serviceRoleKey != null && !serviceRoleKey.isBlank())
                ? serviceRoleKey
                : anonKey;
    }
}
