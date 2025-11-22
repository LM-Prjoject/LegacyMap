package com.legacymap.backend.service;

import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatFileStorageService {

    @Value("${app.chat.upload-dir:uploads/chat}")
    private String uploadDir;

    public StoredFile store(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new AppException(ErrorCode.BAD_REQUEST);
        }
        try {
            Path root = Path.of(uploadDir).toAbsolutePath().normalize();
            Files.createDirectories(root);

            String extension = StringUtils.getFilenameExtension(file.getOriginalFilename());
            String generatedName = UUID.randomUUID() + (extension != null ? "." + extension : "");

            Path target = root.resolve(generatedName);
            Files.copy(file.getInputStream(), target, StandardCopyOption.REPLACE_EXISTING);

            String contentType = file.getContentType();
            boolean isImage = contentType != null && contentType.startsWith("image/");
            String urlPath = "/uploads/chat/" + generatedName;

            return new StoredFile(
                    urlPath,
                    target,
                    file.getOriginalFilename(),
                    contentType,
                    file.getSize(),
                    isImage
            );
        } catch (IOException e) {
            log.error("Upload file thất bại", e);
            throw new AppException(ErrorCode.INTERNAL_ERROR);
        }
    }

    public record StoredFile(
            String url,
            Path absolutePath,
            String originalName,
            String contentType,
            long size,
            boolean image
    ) {
    }
}

