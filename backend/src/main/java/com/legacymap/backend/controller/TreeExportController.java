package com.legacymap.backend.controller;

import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.service.TreeExportService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.UUID;

@RestController
@RequestMapping("/api/trees")
@RequiredArgsConstructor
public class TreeExportController {

    private final TreeExportService treeExportService;

    @PostMapping(
            value = "/{treeId}/export/pdf",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ResponseEntity<byte[]> exportTreePdf(
            @PathVariable UUID treeId,
            @RequestPart("treeImage") MultipartFile treeImage
    ) {

        if (treeImage == null || treeImage.isEmpty()) {
            throw new AppException(ErrorCode.BAD_REQUEST);
        }

        byte[] pdfBytes;
        try {
            pdfBytes = treeExportService.exportTreeDetailsPdf(
                    treeId,
                    treeImage.getBytes()
            );
        } catch (IOException e) {
            throw new AppException(ErrorCode.INTERNAL_ERROR);
        }

        String filename = "tree-" + treeId + "-details.pdf";

        return ResponseEntity
                .ok()
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_PDF_VALUE)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .body(pdfBytes);
    }
}
