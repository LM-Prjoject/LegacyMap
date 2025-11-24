package com.legacymap.backend.service;

import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.UUID;

@Slf4j
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

    public StoredFile uploadChatFile(MultipartFile file, UUID roomId) throws IOException {
        if (file == null || file.isEmpty()) {
            throw new IllegalArgumentException("File cannot be empty");
        }

        String originalName = file.getOriginalFilename();
        String extension = "";
        int dotIndex = originalName != null ? originalName.lastIndexOf(".") : -1;
        if (dotIndex > 0) {
            extension = originalName.substring(dotIndex);
        }

        String generatedFileName = UUID.randomUUID().toString() + extension;
        String path = "chat/" + roomId + "/" + generatedFileName;

        RequestBody requestBody = RequestBody.create(
                file.getBytes(),
                MediaType.get(file.getContentType() != null ? file.getContentType() : "application/octet-stream")
        );

        Request request = new Request.Builder()
                .url(supabaseUrl + "/storage/v1/object/" + bucket + "/" + path)
                .header("apikey", supabaseKey)
                .header("Authorization", "Bearer " + supabaseKey)
                .post(requestBody)
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                String err = response.body() != null ? response.body().string() : "";
                log.error("Supabase upload failed: {} {}", response.code(), err);
                throw new IOException("Upload to Supabase failed: " + response.code());
            }
        }

        String publicUrl = supabaseUrl + "/storage/v1/object/public/" + bucket + "/" + path;
        boolean isImage = file.getContentType() != null && file.getContentType().startsWith("image/");

        return new StoredFile(
                publicUrl,
                originalName != null ? originalName : generatedFileName,
                file.getContentType(),
                file.getSize(),
                isImage,
                generatedFileName
        );
    }

    public record StoredFile(
            String url,
            String originalName,
            String contentType,
            long size,
            boolean isImage,
            String generatedFileName
    ) {}
}