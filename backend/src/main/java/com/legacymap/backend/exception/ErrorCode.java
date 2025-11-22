package com.legacymap.backend.exception;

import lombok.Getter;

@Getter
public enum ErrorCode {
    USER_EXISTED("Tên người dùng đã tồn tại", 1001),
    EMAIL_EXISTED("Email đã được sử dụng", 1002),
    SEND_EMAIL_FAILED("Gửi email thất bại", 1003),
    USER_NOT_FOUND("Không tìm thấy người dùng", 1004),
    INVALID_CREDENTIALS("Tên đăng nhập hoặc mật khẩu không hợp lệ", 1005),
    ACCOUNT_NOT_VERIFIED("Tài khoản chưa được xác minh", 1006),
    ACCOUNT_DISABLED("Tài khoản đã bị vô hiệu hóa", 1007),
    VALIDATION_FAILED("Dữ liệu không hợp lệ", 1008),
    INTERNAL_ERROR("Lỗi hệ thống nội bộ", 1009),
    INTERNAL_SERVER_ERROR("Lỗi máy chủ nội bộ", 5000), // ✅ THÊM: Error code mới
    INVALID_TOKEN("Mã xác thực không hợp lệ", 1010),
    TOKEN_EXPIRED("Mã xác thực đã hết hạn", 1011),
    TOKEN_ALREADY_USED("Mã xác thực đã được sử dụng", 1012),
    INVALID_PURPOSE("Mục đích của mã xác thực không hợp lệ", 1013),
    INVALID_PASSWORD("Mật khẩu không đúng", 1024),
    BAD_REQUEST("Yêu cầu không hợp lệ", 1025),
    NOT_FOUND("Không tìm thấy tài nguyên", 1026),

    // Authentication & Authorization
    UNAUTHENTICATED("Bạn chưa đăng nhập", 1014),
    UNAUTHORIZED("Bạn không có quyền truy cập tài nguyên này", 1015),
    OAUTH_GOOGLE_ONLY("Tài khoản này chỉ được đăng nhập bằng Google", 1021),

    // Family Tree related
    FAMILY_TREE_NOT_FOUND("Không tìm thấy gia phả hoặc bạn không có quyền truy cập", 1100),
    PERSON_NOT_FOUND("Không tìm thấy người trong gia phả", 1101),
    RELATIONSHIP_INVALID_TYPE("Kiểu quan hệ không hợp lệ", 1102),
    RELATIONSHIP_ALREADY_EXISTS("Mối quan hệ này đã tồn tại", 1103),
    RELATIONSHIP_NOT_SAME_TREE("Hai người không thuộc cùng một gia phả", 1104),
    RELATIONSHIP_NOT_FOUND("Không tìm thấy mối quan hệ", 1105),
    RELATIONSHIP_SELF_LINK("Không thể tạo quan hệ với chính bản thân", 1106),
    TREE_NOT_FOUND("Không thể tìm thấy gia phả", 1107),

    // Admin related
    USER_BANNED("Tài khoản của bạn đã bị khóa. Vui lòng liên hệ hỗ trợ", 1110),
    USER_ALREADY_BANNED("Người dùng này đã bị khóa", 1111),
    USER_NOT_BANNED("Người dùng này chưa bị khóa", 1112),
    CANNOT_BAN_ADMIN("Không thể khóa tài khoản admin", 1113),
    ADMIN_ACTION_FORBIDDEN("Hành động bị cấm với tài khoản admin", 1114),

    // Event related
    EVENT_NOT_FOUND("Event not found", 1120),
    INVALID_INPUT_DATA("Invalid input data", 1121),

    // Notification related
    NOTIFICATION_NOT_FOUND("Notification not found", 1113),

    // Sharing related
    PERMISSION_DENIED("Bạn không có quyền thực hiện hành động này", 4030),
    USER_ALREADY_HAS_ACCESS("Người dùng đã có quyền truy cập vào gia phả này", 4031),
    CANNOT_SHARE_TO_SELF("Không thể chia sẻ gia phả cho chính mình", 4032);

    private final int code;
    private final String message;

    ErrorCode(String message, int code) {
        this.message = message;
        this.code = code;
    }
}
