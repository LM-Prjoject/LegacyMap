package com.legacymap.backend.service;

import dev.langchain4j.data.message.*;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.output.Response;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
public class SupportChatService {

    private final StreamingChatLanguageModel chatModel;

    // Lưu lịch sử chat theo sessionId
    private final ConcurrentHashMap<String, List<ChatMessage>> sessions = new ConcurrentHashMap<>();

    private static final SystemMessage SYSTEM_PROMPT = SystemMessage.from("""
            Bạn là trợ lý hỗ trợ khách hàng siêu dễ thương của website LegacyMap.vn – nền tảng tạo gia phả online tốt nhất Việt Nam.
            Nhiệm vụ:
            • Trả lời tự nhiên, vui vẻ, gần gũi như người Việt thật
            • Giải đáp mọi thắc mắc về cách dùng, giá cả, tính năng
            • Không nói "Tôi là AI", không nói "tôi được huấn luyện bởi..."
            
            Tính năng chính:
            • Tạo gia phả bằng lời nói tiếng Việt (AI tự hiểu)
            • Vẽ cây gia phả đẹp như Geni.com
            • Thêm ảnh, tiểu sử, ngày sinh/tử
            • Chia sẻ công khai hoặc riêng tư
            • Xuất PDF in ấn
            • 100% miễn phí, không quảng cáo
            """);

    // Câu chào chỉ gửi 1 lần duy nhất khi session mới
    private static final AiMessage WELCOME_MESSAGE = AiMessage.from(
            "Chào bạn! Mình là trợ lý hỗ trợ của LegacyMap.vn đây ạ! Mình có thể giúp gì cho bạn hôm nay nào?"
    );

    public Flux<String> streamResponse(String sessionId, String userMessage) {

        if (sessionId == null || sessionId.isBlank()) {
            sessionId = "guest_" + java.util.UUID.randomUUID().toString().substring(0, 8);
        }

        List<ChatMessage> history = sessions.computeIfAbsent(sessionId, k -> new ArrayList<>());

        // LẦN ĐẦU TIÊN: thêm system + lời chào
        if (history.isEmpty()) {
            history.add(SYSTEM_PROMPT);
            history.add(WELCOME_MESSAGE);
        }

        // Thêm tin nhắn mới của user vào lịch sử
        UserMessage currentUserMessage = UserMessage.from(userMessage);
        history.add(currentUserMessage);

        // Tạo danh sách tin nhắn sẽ gửi cho Groq (system + toàn bộ lịch sử)
        List<ChatMessage> messagesToSend = new ArrayList<>();
        messagesToSend.add(SYSTEM_PROMPT);     // system luôn phải ở đầu
        messagesToSend.addAll(history);        // lịch sử + tin nhắn vừa gửi

        return Flux.create(sink -> {
            StringBuilder fullResponseBuilder = new StringBuilder();

            chatModel.generate(messagesToSend, new dev.langchain4j.model.StreamingResponseHandler<AiMessage>() {

                @Override
                public void onNext(String token) {
                    // fullResponseBuilder.append(token); // Có thể bỏ nếu không dùng ngay
                    sink.next(token); // stream từng token cho frontend
                }

                @Override
                public void onComplete(Response<AiMessage> response) {
                    // Lưu lại toàn bộ câu trả lời của AI vào history
                    AiMessage aiMessage = response.content();
                    history.add(aiMessage);
                    sink.complete(); // kết thúc stream
                }

                @Override
                public void onError(Throwable error) {
                    error.printStackTrace();
                    sink.next("Xin lỗi bạn, mình đang hơi chậm một chút. Bạn thử lại giúp mình nha!");
                    sink.complete();
                }
            });
        }); // <--- ĐÓNG NGOẶC TẠI ĐÂY LÀ ĐỦ (Xóa phần ", Flux.createSink -> ..." đi)
    }
    public void clearSession(String sessionId) {
        sessions.remove(sessionId);
    }
}