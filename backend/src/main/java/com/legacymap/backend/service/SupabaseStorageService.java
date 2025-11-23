package com.legacymap.backend.service;

import okhttp3.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class SupabaseStorageService {

    @Value("${supabase.url}")
    private String supabaseUrl;

    @Value("${supabase.key}")
    private String supabaseKey;

    @Value("${supabase.bucket}")
    private String bucket;

    private final OkHttpClient client = new OkHttpClient();

    public String uploadAvatar(byte[] bytes, String fileName, String contentType) throws Exception {

        String path = "ai_avatars/" + fileName;

        RequestBody body = RequestBody.create(
                bytes,
                MediaType.parse(contentType)
        );
        Request request = new Request.Builder()
                .url(supabaseUrl + "/storage/v1/object/" + bucket + "/" + path)
                .header("apikey", supabaseKey)
                .header("Authorization", "Bearer " + supabaseKey)
                .put(body)
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new RuntimeException("Upload failed: " + response.code() + " - " + response.message());
            }
        }
        return supabaseUrl + "/storage/v1/object/public/" + bucket + "/" + path;
    }
}