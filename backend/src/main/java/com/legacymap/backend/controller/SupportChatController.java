package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.ChatRequest;
import com.legacymap.backend.service.SupportChatService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import io.github.resilience4j.ratelimiter.annotation.RateLimiter;

import java.util.UUID;

@RestController
@RequestMapping("/api/support")
@RequiredArgsConstructor

public class SupportChatController {

    private final SupportChatService chatService;

    // Cách 1: GET (test nhanh bằng Postman, curl, browser)
    @GetMapping(value = "/chat", produces = "text/event-stream;charset=UTF-8")
    public Flux<String> chatGet(
            @RequestParam(required = false, defaultValue = "") String sessionId,
            @RequestParam String message) {
        if (sessionId == null || sessionId.trim().isEmpty()) {
            sessionId = "guest_" + UUID.randomUUID().toString().substring(0, 8);
        }
        return chatService.streamResponse(sessionId, message)
                .collectList()                    // gom hết tất cả token lại
                .map(tokens -> String.join("", tokens))  // nối thành 1 chuỗi hoàn chỉnh
                .flux()                           // chuyển lại thành Flux để stream
                .concatWith(Flux.just("[DONE]")); // báo frontend biết kết thúc
    }

    // Trong SupportChatController.java – THÊM ĐOẠN NÀY
    @PostMapping(value = "/chat",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = "text/event-stream;charset=UTF-8")
    @RateLimiter(name = "chat-limit", fallbackMethod = "chatFallback")
    public Flux<String> chatPost(@RequestBody ChatRequest request) {
        String sid = request.getSessionId();
        if (sid == null || sid.trim().isEmpty()) {
            sid = "guest_" + UUID.randomUUID().toString().substring(0, 8);
        }
        return chatService.streamResponse(sid, request.getMessage())
                .collectList()
                .map(tokens -> String.join("", tokens))
                .flux()
                .concatWith(Flux.just("[DONE]"));
    }

    public Flux<String> chatFallback(ChatRequest request, Throwable t) {
        return Flux.just("Too many requests. Please try again later.");
    }

    @PostMapping("/clear")
    public String clear(@RequestParam String sessionId) {
        chatService.clearSession(sessionId);
        return "Đã xóa lịch sử!";
    }
}