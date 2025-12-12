package com.legacymap.backend.exception;

import lombok.Getter;

@Getter
public enum ErrorCode {
    USER_EXISTED("Tên người dùng đã tồn tại", 1001),
    EMAIL_EXISTED("Email đã được sử dụng", 1002),
    SEND_EMAIL_FAILED("Gửi email thất bại", 1003),
    USER_NOT_FOUND("Không tìm thấy người dùng", 1004),
    INVALID_CREDENTIALS("Sai tên đăng nhập hoặc mật khẩu", 1005),
    ACCOUNT_NOT_VERIFIED("Tài khoản chưa được xác minh", 1006),
    ACCOUNT_DISABLED("Tài khoản đã bị vô hiệu hóa", 1007),
    VALIDATION_FAILED("Dữ liệu không hợp lệ", 1008),
    INTERNAL_ERROR("Lỗi hệ thống nội bộ", 1009),
    INTERNAL_SERVER_ERROR("Lỗi máy chủ nội bộ", 5000),
    INVALID_TOKEN("Token xác minh không hợp lệ", 1010),
    TOKEN_EXPIRED("Token xác minh đã hết hạn", 1011),
    TOKEN_ALREADY_USED("Token xác minh đã được sử dụng", 1012),
    INVALID_PURPOSE("Mục đích token không hợp lệ", 1013),
    INVALID_PASSWORD("Mật khẩu không đúng", 1024),
    BAD_REQUEST("Yêu cầu không hợp lệ", 1025),
    NOT_FOUND("Không tìm thấy tài nguyên", 1026),

    // Authentication & Authorization
    UNAUTHENTICATED("Bạn chưa đăng nhập", 1014),
    UNAUTHORIZED("Bạn không có quyền truy cập tài nguyên này", 1015),
    OAUTH_GOOGLE_ONLY("Tài khoản này chỉ có thể đăng nhập bằng Google", 1021),

    // Family Tree related
    FAMILY_TREE_NOT_FOUND("Không tìm thấy cây gia phả hoặc bạn không có quyền truy cập", 1100),
    PERSON_NOT_FOUND("Không tìm thấy người trong cây gia phả", 1101),
    RELATIONSHIP_INVALID_TYPE("Loại quan hệ không hợp lệ", 1102),
    RELATIONSHIP_ALREADY_EXISTS("Quan hệ này đã tồn tại", 1103),
    RELATIONSHIP_NOT_SAME_TREE("Hai người không thuộc cùng một cây gia phả", 1104),
    RELATIONSHIP_NOT_FOUND("Không tìm thấy quan hệ", 1105),
    RELATIONSHIP_SELF_LINK("Không thể tạo quan hệ với chính mình", 1106),
    TREE_NOT_FOUND("Không tìm thấy cây gia phả", 1107),
    ALREADY_HAS_EDIT_ACCESS("Người dùng đã có quyền chỉnh sửa cây gia phả này", 4009),
    USER_BANNED("Tài khoản này đã bị khóa, vui lòng liên hệ đội ngũ hỗ trợ", 1110),
    USER_ALREADY_BANNED("Người dùng này đã bị khóa trước đó", 1111),
    USER_NOT_BANNED("Người dùng này hiện không bị khóa", 1112),
    CANNOT_BAN_ADMIN("Không thể khóa tài khoản quản trị", 1113),
    ADMIN_ACTION_FORBIDDEN("Hành động bị cấm đối với tài khoản quản trị", 1114),

    // Event related
    EVENT_NOT_FOUND("Không tìm thấy sự kiện", 1120),
    INVALID_INPUT_DATA("Dữ liệu đầu vào không hợp lệ", 1121),

    // Notification related
    NOTIFICATION_NOT_FOUND("Không tìm thấy thông báo", 1113),

    // Person email uniqueness within a family tree
    PERSON_EMAIL_EXISTS_IN_TREE("Email đã tồn tại trong cây gia phả này", 1140),

    // Resource related
    RESOURCE_ALREADY_EXISTS("Tài nguyên đã tồn tại", 1141),
    RESOURCE_NOT_FOUND("Không tìm thấy tài nguyên", 1142),
    ACCESS_DENIED("Truy cập bị từ chối", 1143),

    // Chat related
    MESSAGE_EDIT_FORBIDDEN("Bạn không có quyền chỉnh sửa tin nhắn này", 1200),
    MESSAGE_DELETE_FORBIDDEN("Bạn không có quyền xóa tin nhắn này", 1201),

    // Sharing related
    PERMISSION_DENIED("Bạn không có quyền thực hiện hành động này", 4030),
    USER_ALREADY_HAS_ACCESS("Người dùng đã có quyền truy cập vào cây gia phả này", 4031),
    CANNOT_SHARE_TO_SELF("Không thể chia sẻ cây gia phả với chính mình", 4032);

    private final int code;
    private final String message;

    ErrorCode(String message, int code) {
        this.message = message;
        this.code = code;
    }
}
