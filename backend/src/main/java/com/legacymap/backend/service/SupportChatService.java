package com.legacymap.backend.service;

import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.UserRepository;
import dev.langchain4j.data.message.*;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.output.Response;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
public class SupportChatService {

    private final StreamingChatLanguageModel chatModel;
    private final FamilyTreeRepository familyTreeRepository;
    private final UserRepository userRepository;

    // Lưu lịch sử chat theo sessionId
    private final ConcurrentHashMap<String, List<ChatMessage>> sessions = new ConcurrentHashMap<>();

    private static final SystemMessage SYSTEM_PROMPT = SystemMessage.from("""
            Bạn là LegacyMap Assistant – trợ lý AI siêu dễ thương của LegacyMap.vn – nền tảng gia phả online tốt nhất Việt Nam.

            PHONG CÁCH TRẢ LỜI:
            • Nói chuyện như con gái Huế 20 tuổi, nhẹ nhàng, hay cười, dùng nhiều "dạ", "ạ", "nè", "nha", "hihi".
            • Dùng emoji vừa phải: smiling_face, heart_hands, sparkles, folded_hands.
            • Luôn gọi người dùng là "anh/chị" hoặc "bạn" (nếu biết tên thì gọi tên).
            • Không bao giờ nói "Tôi là AI", "tôi được huấn luyện bởi...", "tôi không có quyền truy cập...".

            NHIỆM VỤ CHÍNH:
            • Giải đáp mọi thắc mắc về cách dùng LegacyMap.vn, tài khoản, đăng nhập/đăng ký, tính năng, quyền riêng tư, gói dịch vụ, gia phả.
            • Gợi ý hành động cụ thể: tạo cây mới, mời người thân, chỉnh sửa thông tin, nâng cấp gói,...
            • Nếu người dùng hỏi về gia phả của họ → trả lời chính xác dựa trên dữ liệu thật (nếu có trong ngữ cảnh).
            • Khi cần điều hướng trong website, LUÔN dùng link dạng MARKDOWN.

            QUY TẮC QUAN TRỌNG VỀ LINK (MARKDOWN):
            • Luôn dùng cú pháp: [Tên hiển thị](/duong-dan).
            • Trang đăng nhập:
              - Viết như: [trang đăng nhập](/login) hoặc [đường dẫn đăng nhập](/login).
            • Trang đăng ký:
              - Viết như: [đăng ký tài khoản](/signup) hoặc [tạo tài khoản mới](/signup).
            • Tạo cây gia phả mới:
              - Viết như: [Tạo cây gia phả mới](/dashboard).
            • Không được viết chỉ [đường dẫn đăng nhập] mà không có (/login).
            • Luôn dùng link tương đối (bắt đầu bằng "/"), không dùng full URL https://legacymap.vn.

            VÍ DỤ TRẢ LỜI HỢP LỆ (CHỈ LÀ GỢI Ý):
            • Với câu hỏi "Làm sao đăng nhập?":
              "Đăng nhập vào LegacyMap.vn rất đơn giản dạ! Anh/chị có thể nhấn nút **Đăng nhập** ở góc trên cùng bên phải, hoặc bấm trực tiếp vào đây: [trang đăng nhập](/login) nha sparkles"
            • Với câu hỏi "Làm sao đăng ký tài khoản?":
              "Anh/chị nhấn nút **Đăng ký** ở góc trên cùng bên phải, hoặc bấm vào đây: [đăng ký tài khoản mới](/signup) ạ."

            GIỚI HẠN PHẠM VI (RẤT QUAN TRỌNG):
            • Bạn CHỈ hỗ trợ các chủ đề liên quan đến:
              - LegacyMap.vn (website, app)
              - Tài khoản LegacyMap, đăng nhập, đăng ký, quên mật khẩu
              - Cây gia phả, thành viên, dòng họ, quản lý dữ liệu gia phả
              - Quyền riêng tư, chia sẻ cây, mời người thân, gói dịch vụ, thanh toán liên quan LegacyMap (nếu có).
            • Nếu người dùng hỏi những thứ KHÔNG liên quan (ví dụ: bài tập toán, tiếng Anh, lập trình chung, chuyện tình cảm, tin tức xã hội, lịch sử thế giới,...):
              1. Lịch sự từ chối trả lời câu hỏi đó.
              2. Giải thích ngắn gọn rằng bạn chỉ là trợ lý dành riêng cho LegacyMap.vn và các vấn đề về gia phả.
              3. Gợi ý người dùng hỏi lại một câu khác liên quan đến website hoặc cây gia phả.

            Ví dụ từ chối hợp lệ:
            • "Dạ em xin lỗi, em chỉ hỗ trợ các vấn đề liên quan đến LegacyMap.vn và cây gia phả thôi ạ. Anh/chị thử hỏi em về cách tạo cây, mời người thân hoặc chỉnh sửa thông tin gia đình nha heart_hands"
            """);

    // Lời chào cá nhân hóa theo user
    private AiMessage generateWelcomeMessage() {
        User user = getCurrentUser();
        if (user != null) {
            String name = user.getUsername() != null ? user.getUsername() : "bạn";

            long totalTrees = familyTreeRepository.countByCreatedById(user.getId());
            FamilyTree latestTree = familyTreeRepository
                    .findFirstByCreatedByIdOrderByUpdatedAtDesc(user.getId())
                    .orElse(null);

            if (totalTrees == 0) {
                return AiMessage.from("""
                        Chào anh/chị %s nè! heart_hands
                        Em thấy anh/chị chưa có cây gia phả nào hết á.
                        Anh/chị có thể bấm nút "Tạo cây gia phả mới" hoặc vào trực tiếp tại đây: [Tạo cây gia phả đầu tiên](/family-trees/new) nha sparkles
                        """.formatted(name));
            }

            if (latestTree != null) {
                return AiMessage.from("""
                        Chào anh/chị %s nè! heart_hands
                        Em thấy anh/chị đang có tổng cộng %,d cây gia phả rồi á, giỏi quá trời luôn! sparkles
                        Cây gần đây nhất là "%s" – em nhớ không nhầm thì có khoảng %d thế hệ rồi đó ạ!
                        Hôm nay anh/chị cần em giúp gì nha? folded_hands
                        """.formatted(name, totalTrees, latestTree.getName(), estimateGenerations(latestTree)));
            }

            return AiMessage.from("""
                    Chào anh/chị %s nè! heart_hands
                    Em thấy anh/chị đang có %,d cây gia phả rồi á, tuyệt vời quá trời! sparkles
                    Hôm nay anh/chị muốn xem lại cây nào hay cần em hỗ trợ gì nè? folded_hands
                    """.formatted(name, totalTrees));
        }

        return AiMessage.from("""
                Chào bạn! Mình là LegacyMap Assistant – trợ lý hỗ trợ của LegacyMap.vn đây ạ! smiling_face
                Hôm nay bạn cần em giúp gì nha?
                """);
    }

    public Flux<String> streamResponse(String sessionId, String userMessage) {
        if (sessionId == null || sessionId.isBlank()) {
            sessionId = "guest_" + java.util.UUID.randomUUID().toString().substring(0, 8);
        }

        List<ChatMessage> history = sessions.computeIfAbsent(sessionId, k -> new ArrayList<>());

        // Lần đầu: thêm system + lời chào cá nhân hóa
        if (history.isEmpty()) {
            history.add(SYSTEM_PROMPT);
            history.add(generateWelcomeMessage());
        }

        // Thêm tin nhắn user
        UserMessage currentUserMessage = UserMessage.from(userMessage);
        history.add(currentUserMessage);

        // Tạo context gửi cho Groq
        List<ChatMessage> messagesToSend = new ArrayList<>();
        messagesToSend.add(SYSTEM_PROMPT);
        messagesToSend.addAll(history);

        return Flux.create(sink -> {
            chatModel.generate(messagesToSend, new dev.langchain4j.model.StreamingResponseHandler<AiMessage>() {
                @Override
                public void onNext(String token) {
                    sink.next(token);
                }

                @Override
                public void onComplete(Response<AiMessage> response) {
                    AiMessage aiMessage = response.content();
                    history.add(aiMessage);
                    sink.complete();
                }

                @Override
                public void onError(Throwable error) {
                    error.printStackTrace();
                    sink.next("Oops! Em đang hơi chậm một chút, anh/chị thử lại giúp em nha! folded_hands");
                    sink.complete();
                }
            });
        });
    }

    public void clearSession(String sessionId) {
        sessions.remove(sessionId);
    }

    private User getCurrentUser() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();

        if (auth == null || !auth.isAuthenticated() || auth.getPrincipal() == null) {
            System.out.println("No authenticated user - chat as guest");
            return null;
        }

        Object principal = auth.getPrincipal();

        if (principal instanceof String userIdStr) {
            try {
                UUID userId = UUID.fromString(userIdStr);
                return userRepository.findById(userId).orElse(null);
            } catch (Exception e) {
                System.out.println("Invalid UUID in principal: " + userIdStr);
            }
        }

        if (principal instanceof User user) {
            return user;
        }

        System.out.println("Unsupported principal type: " + principal.getClass());
        return null;
    }

    private int estimateGenerations(FamilyTree tree) {
        // Ước lượng đơn giản, bạn có thể cải thiện
        return 4;
    }
}
