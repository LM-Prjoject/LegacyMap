package com.legacymap.backend.service;

import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.Relationship;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.enums.RelationshipType;
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
    private SystemMessage buildUserContext(User user) {
        if (user == null) {
            return SystemMessage.from("""
            Người dùng hiện tại là khách, chưa đăng nhập.
            Hãy gọi là "bạn".
        """);
        }

        long totalTrees = familyTreeRepository.countByCreatedById(user.getId());

        return SystemMessage.from("""
        Thông tin người dùng hiện tại:
        - Username: %s
        - User ID: %s
        - Tổng số cây gia phả: %d

        Hãy sử dụng thông tin trên để cá nhân hóa câu trả lời.
        Gọi họ bằng tên "%s".
    """.formatted(
                user.getUsername(),
                user.getId().toString(),
                totalTrees,
                user.getUsername()
        ));
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
        User currentUser = getCurrentUser();
        // Tạo context gửi cho Groq
        List<ChatMessage> messagesToSend = new ArrayList<>();
        messagesToSend.add(SYSTEM_PROMPT);
        messagesToSend.add(buildUserContext(currentUser));  // ⭐ chèn thêm
        messagesToSend.addAll(history);
        messagesToSend.add(currentUserMessage);

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
        // Load lại tree kèm đầy đủ persons + relationships (nếu bạn có EntityGraph)
        FamilyTree fullTree = familyTreeRepository
                .findByIdWithGraph(tree.getId())
                .orElse(tree);

        var persons = fullTree.getPersons();
        if (persons == null || persons.isEmpty()) {
            return 0;
        }

        var relationships = fullTree.getRelationships();
        if (relationships == null) {
            relationships = java.util.Collections.emptySet();
        }

        // ===== Map giống bên frontend =====
        // parentOf[u] = danh sách con của u
        java.util.Map<UUID, java.util.List<UUID>> parentOf = new java.util.HashMap<>();
        // indeg[u] = số lượng cha/mẹ của u
        java.util.Map<UUID, Integer> indeg = new java.util.HashMap<>();
        java.util.List<UUID> ids = new java.util.ArrayList<>();

        // Khởi tạo id & indegree
        for (Person p : persons) {
            UUID id = p.getId();
            ids.add(id);
            indeg.put(id, 0);
            parentOf.put(id, new java.util.ArrayList<>());
        }

        // ===== SỬA CHỖ NÀY: dùng person1 / person2 & relationshipType string =====
        for (Relationship r : relationships) {
            String type = r.getRelationshipType();
            if (type == null || !type.equalsIgnoreCase("parent")) {
                // Chỉ quan tâm quan hệ cha/mẹ
                continue;
            }

            Person p1 = r.getPerson1(); // giả sử: p1 là cha/mẹ
            Person p2 = r.getPerson2(); //         p2 là con

            if (p1 == null || p2 == null) {
                continue;
            }

            UUID fromId = p1.getId(); // cha/mẹ
            UUID toId   = p2.getId(); // con

            parentOf.computeIfAbsent(fromId, k -> new java.util.ArrayList<>()).add(toId);
            parentOf.computeIfAbsent(toId, k -> new java.util.ArrayList<>());

            indeg.put(toId, indeg.getOrDefault(toId, 0) + 1);
        }

        // ===== Phần 1: nếu có root (indeg = 0) -> dùng BFS topo =====
        java.util.List<UUID> roots = new java.util.ArrayList<>();
        for (UUID id : ids) {
            if (indeg.getOrDefault(id, 0) == 0) {
                roots.add(id);
            }
        }

        if (!roots.isEmpty()) {
            java.util.Map<UUID, Integer> depth = new java.util.HashMap<>();
            java.util.ArrayDeque<UUID> q = new java.util.ArrayDeque<>();

            for (UUID r : roots) {
                depth.put(r, 1);
                q.add(r);
            }

            while (!q.isEmpty()) {
                UUID u = q.poll();
                int du = depth.getOrDefault(u, 1);
                for (UUID v : parentOf.getOrDefault(u, java.util.Collections.emptyList())) {
                    indeg.put(v, indeg.getOrDefault(v, 0) - 1);
                    int dv = depth.getOrDefault(v, 0);
                    if (dv < du + 1) {
                        depth.put(v, du + 1);
                    }
                    if (indeg.getOrDefault(v, 0) == 0) {
                        q.add(v);
                    }
                }
            }

            int ans = 1;
            for (UUID id : ids) {
                ans = Math.max(ans, depth.getOrDefault(id, 1));
            }
            return ans;
        }

        // ===== Phần 2: không có root -> dùng DFS + memo + detect cycle =====
        java.util.Map<UUID, Integer> memo = new java.util.HashMap<>();
        java.util.Set<UUID> visiting = new java.util.HashSet<>();

        java.util.function.Function<UUID, Integer> dfs = new java.util.function.Function<UUID, Integer>() {
            @Override
            public Integer apply(UUID u) {
                if (memo.containsKey(u)) return memo.get(u);
                if (visiting.contains(u)) {
                    // Phát hiện cycle, tránh infinite loop
                    return 1;
                }
                visiting.add(u);
                int best = 1;
                for (UUID v : parentOf.getOrDefault(u, java.util.Collections.emptyList())) {
                    best = Math.max(best, 1 + this.apply(v));
                }
                visiting.remove(u);
                memo.put(u, best);
                return best;
            }
        };

        int ans = 1;
        for (UUID id : ids) {
            ans = Math.max(ans, dfs.apply(id));
        }
        return ans;
    }

}
