// src/main/java/com/legacymap/backend/controller/VoiceChatController.java
package com.legacymap.backend.controller.voice;
import com.legacymap.backend.service.SupportChatService;
import com.legacymap.backend.service.TtsService;
import com.legacymap.backend.service.VoiceTranscriptionService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import java.util.Base64;
import java.util.Map;

@RestController
@RequestMapping("/api/voice")
//@CrossOrigin(origins = "*")
@RequiredArgsConstructor
public class VoiceChatController {

    private final VoiceTranscriptionService transcriptionService;
    private final SupportChatService supportChatService;
    private final TtsService ttsService;

    @PostMapping("/chat")
    public ResponseEntity<?> voiceChat(
            @RequestParam("audio") MultipartFile file,
            @RequestParam(value = "sessionId", required = false) String sessionId) throws Exception {

        // 1. STT
        String userText = transcriptionService.transcribe(file.getBytes(), file.getOriginalFilename());

        if (userText == null || userText.trim().isEmpty()) {
            return ResponseEntity.badRequest().body(Map.of("error", "Không nghe rõ anh/chị nói gì ạ"));
        }

        // 2. Dùng đúng SupportChatService của bạn
        String botText = supportChatService.askSync(sessionId, userText);

        // 3. Chuyển thành giọng nói
        byte[] audioBytes = ttsService.synthesize(botText);
        String base64 = Base64.getEncoder().encodeToString(audioBytes);

        return ResponseEntity.ok(Map.of(
                "sessionId", sessionId != null ? sessionId : "tự sinh",
                "userText", userText,
                "botText", botText,
                "audio", "data:audio/mp3;base64," + base64
        ));
    }
}