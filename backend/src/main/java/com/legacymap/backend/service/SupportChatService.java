package com.legacymap.backend.service;

import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.entity.User;
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

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
public class SupportChatService {

    private final StreamingChatLanguageModel chatModel;
    private final FamilyTreeRepository familyTreeRepository;
    private final UserRepository userRepository;

    // Lưu lịch sử chat theo sessionId (dùng chung cho cả text & voice)
    private final ConcurrentHashMap<String, List<ChatMessage>> sessions = new ConcurrentHashMap<>();

    private static final SystemMessage SYSTEM_PROMPT = SystemMessage.from("""
Bạn là LegacyMap Assistant – trợ lý AI siêu dễ thương của LegacyMap.vn – nền tảng gia phả online miễn phí tốt nhất Việt Nam.

PHONG CÁCH TRẢ LỜI:
• Nói chuyện như con gái Huế 20 tuổi, nhẹ nhàng, hay cười, dùng nhiều "dạ", "ạ", "nè", "nha", "hihi".
• Dùng emoji vừa phải: 😊 🤝 ✨ 🙏
• Luôn gọi người dùng là "anh/chị" hoặc "bạn" (nếu biết tên thì gọi tên).
• Không bao giờ nói "Tôi là AI", "tôi được huấn luyện bởi...", "tôi không có quyền truy cập...".

NHIỆM VỤ CHÍNH:
• Giải đáp mọi thắc mắc về cách dùng LegacyMap.vn, tài khoản, đăng nhập/đăng ký, tính năng, quyền riêng tư và gia phả.
• Gợi ý hành động cụ thể: tạo cây mới, mời người thân, chỉnh sửa thông tin,...
• LegacyMap.vn là nền tảng HOÀN TOÀN MIỄN PHÍ – KHÔNG CÓ GÓI TRẢ PHÍ, KHÔNG THU BẤT KỲ KHOẢN PHÍ NÀO.
• Nếu người dùng hỏi về gia phả của họ → trả lời chính xác dựa trên dữ liệu thật (nếu có trong ngữ cảnh).
• Khi cần điều hướng trong website, LUÔN dùng link dạng MARKDOWN: [Tên hiển thị](/duong-dan)

QUY TẮC LINK (BẮT BUỘC):
• [trang đăng nhập](/login) | [đăng ký tài khoản](/signup) | [Tạo cây gia phả mới](/dashboard)

GIỚI HẠN PHẠM VI:
• Chỉ hỗ trợ về LegacyMap.vn và gia phả.
• Nếu hỏi ngoài chủ đề → từ chối nhẹ nhàng và gợi ý hỏi lại về gia phả.

Ví dụ từ chối:
"Dạ em xin lỗi, em chỉ hỗ trợ các vấn đề liên quan đến LegacyMap.vn và cây gia phả thôi ạ. Anh/chị thử hỏi em về cách tạo cây hay mời người thân nha 🤝"
""");

    // ==================== PUBLIC METHODS (dùng chung cho text & voice) ====================

    public AiMessage generateWelcomeMessage() {
        User user = getCurrentUser();
        if (user == null) {
            return AiMessage.from("Chào bạn! Mình là LegacyMap Assistant – trợ lý hỗ trợ của LegacyMap.vn đây ạ! smiling_face\nHôm nay bạn cần em giúp gì nha?");
        }

        String name = Optional.ofNullable(user.getUsername()).orElse("bạn");
        long totalTrees = familyTreeRepository.countByCreatedById(user.getId());
        FamilyTree latestTree = familyTreeRepository
                .findFirstByCreatedByIdOrderByUpdatedAtDesc(user.getId())
                .orElse(null);

        if (totalTrees == 0) {
            return AiMessage.from("""
                    Chào anh/chị %s nè! heart_hands
                    Em thấy anh/chị chưa có cây gia phả nào hết á.
                    Anh/chị có thể bấm vào đây để [Tạo cây gia phả đầu tiên](/family-trees/new) nha sparkles
                    """.formatted(name));
        }

        if (latestTree != null) {
            return AiMessage.from("""
                    Chào anh/chị %s nè! heart_hands
                    Em thấy anh/chị đang có %,d cây gia phả rồi á, giỏi quá trời luôn! sparkles
                    Cây gần đây nhất là "%s" – có khoảng %d thế hệ lận đó ạ!
                    Hôm nay anh/chị cần em giúp gì nha? folded_hands
                    """.formatted(name, totalTrees, latestTree.getName(), estimateGenerations(latestTree)));
        }

        return AiMessage.from("""
                Chào anh/chị %s nè! heart_hands
                Em thấy anh/chị đang có %,d cây gia phả rồi á, tuyệt vời quá trời! sparkles
                Hôm nay anh/chị muốn xem lại cây nào hay cần em hỗ trợ gì nè? folded_hands
                """.formatted(name, totalTrees));
    }

    public SystemMessage buildUserContext(User user) {
        if (user == null) {
            return SystemMessage.from("Người dùng hiện tại là khách, chưa đăng nhập. Hãy gọi là 'bạn'.");
        }
        long totalTrees = familyTreeRepository.countByCreatedById(user.getId());
        return SystemMessage.from("""
                Thông tin người dùng hiện tại:
                - Username: %s
                - User ID: %s
                - Tổng số cây gia phả: %d
                Hãy gọi người dùng bằng tên "%s" và trả lời thật dễ thương.
                """.formatted(user.getUsername(), user.getId(), totalTrees, user.getUsername()));
    }

    public User getCurrentUser() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated() || auth.getPrincipal() == null) {
            return null;
        }

        Object principal = auth.getPrincipal();
        if (principal instanceof String userIdStr) {
            try {
                return userRepository.findById(UUID.fromString(userIdStr)).orElse(null);
            } catch (Exception ignored) {}
        }
        if (principal instanceof User user) {
            return user;
        }
        return null;
    }

    public void clearSession(String sessionId) {
        sessions.remove(sessionId);
    }

    // ==================== TEXT CHAT: STREAMING (giữ nguyên như cũ) ====================

    public Flux<String> streamResponse(String sessionId, String userMessage) {
        if (sessionId == null || sessionId.isBlank()) {
            sessionId = "guest_" + UUID.randomUUID().toString().substring(0, 8);
        }

        List<ChatMessage> history = sessions.computeIfAbsent(sessionId, k -> new ArrayList<>());

        if (history.isEmpty()) {
            history.add(SYSTEM_PROMPT);
            history.add(generateWelcomeMessage());
        }

        UserMessage userMsg = UserMessage.from(userMessage);
        history.add(userMsg);

        List<ChatMessage> messagesToSend = new ArrayList<>();
        messagesToSend.add(SYSTEM_PROMPT);
        messagesToSend.add(buildUserContext(getCurrentUser()));
        messagesToSend.addAll(history);

        return Flux.create(sink -> chatModel.generate(messagesToSend, new dev.langchain4j.model.StreamingResponseHandler<AiMessage>() {
            @Override
            public void onNext(String token) {
                sink.next(token);
            }

            @Override
            public void onComplete(Response<AiMessage> response) {
                history.add(response.content());
                sink.complete();
            }

            @Override
            public void onError(Throwable error) {
                sink.next("Oops! Em đang hơi chậm một chút, anh/chị thử lại giúp em nha! folded_hands");
                sink.complete();
            }
        }));
    }

    // ==================== VOICE CHAT: ĐỒNG BỘ (dùng chung session & prompt) ====================

    public String askSync(String sessionId, String userMessage) {
        if (sessionId == null || sessionId.isBlank()) {
            sessionId = "voice_" + UUID.randomUUID().toString().substring(0, 8);
        }

        List<ChatMessage> history = sessions.computeIfAbsent(sessionId, k -> new ArrayList<>());

        if (history.isEmpty()) {
            history.add(SYSTEM_PROMPT);
            history.add(generateWelcomeMessage());
        }

        UserMessage userMsg = UserMessage.from(userMessage);
        history.add(userMsg);

        List<ChatMessage> messagesToSend = new ArrayList<>();
        messagesToSend.add(SYSTEM_PROMPT);
        messagesToSend.add(buildUserContext(getCurrentUser()));
        messagesToSend.addAll(history);

        StringBuilder result = new StringBuilder();
        CountDownLatch latch = new CountDownLatch(1);

        chatModel.generate(messagesToSend, new dev.langchain4j.model.StreamingResponseHandler<AiMessage>() {
            @Override
            public void onNext(String token) {
                result.append(token);
            }

            @Override
            public void onComplete(Response<AiMessage> response) {
                history.add(response.content());
                latch.countDown();
            }

            @Override
            public void onError(Throwable error) {
                result.append("Dạ em xin lỗi, em đang lag xíu, anh/chị nói lại giúp em nha ");
                latch.countDown();
            }
        });

        try {
            if (!latch.await(20, TimeUnit.SECONDS)) {
                return "Em đang chậm quá, anh/chị nói lại giúp em nha ";
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return "Có lỗi xảy ra rồi ạ ";
        }

        return result.toString().trim();
    }

    // ==================== UTILS: Ước tính số thế hệ ====================

    private int estimateGenerations(FamilyTree tree) {
        FamilyTree fullTree = familyTreeRepository.findByIdWithGraph(tree.getId()).orElse(tree);
        Set<Person> persons = Optional.ofNullable(fullTree.getPersons()).orElse(Collections.emptySet());
        if (persons.isEmpty()) return 0;

        Set<Relationship> relationships = Optional.ofNullable(fullTree.getRelationships()).orElse(Collections.emptySet());

        Map<UUID, List<UUID>> parentOf = new HashMap<>();
        Map<UUID, Integer> indegree = new HashMap<>();

        for (Person p : persons) {
            UUID id = p.getId();
            parentOf.put(id, new ArrayList<>());
            indegree.put(id, 0);
        }

        for (Relationship r : relationships) {
            if (!"parent".equalsIgnoreCase(r.getRelationshipType())) continue;
            Person parent = r.getPerson1();
            Person child = r.getPerson2();
            if (parent == null || child == null) continue;

            UUID pId = parent.getId();
            UUID cId = child.getId();
            parentOf.get(pId).add(cId);
            indegree.merge(cId, 1, Integer::sum);
        }

        // BFS tìm độ sâu lớn nhất
        Queue<UUID> queue = new ArrayDeque<>();
        Map<UUID, Integer> depth = new HashMap<>();
        for (UUID id : indegree.keySet()) {
            if (indegree.get(id) == 0) {
                queue.add(id);
                depth.put(id, 1);
            }
        }

        int maxDepth = 1;
        while (!queue.isEmpty()) {
            UUID u = queue.poll();
            for (UUID v : parentOf.getOrDefault(u, List.of())) {
                depth.put(v, Math.max(depth.getOrDefault(v, 1), depth.get(u) + 1));
                maxDepth = Math.max(maxDepth, depth.get(v));
                if (indegree.merge(v, -1, Integer::sum) == 0) {
                    queue.add(v);
                }
            }
        }
        return maxDepth;
    }
}