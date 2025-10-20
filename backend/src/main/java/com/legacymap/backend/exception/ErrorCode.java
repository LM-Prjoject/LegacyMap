package com.legacymap.backend.exception;

public enum ErrorCode {
    USER_EXISTED("username already existed", 1001),
    EMAIL_EXISTED("email already existed", 1002),
    SEND_EMAIL_FAILED("send email failed", 1003),
    USER_NOT_FOUND("User not found", 1004),
    INVALID_CREDENTIALS("Invalid username/email or password", 1005),
    ACCOUNT_NOT_VERIFIED("Account is not verified", 1006),
    ACCOUNT_DISABLED("Account is deactivated", 1007),
    VALIDATION_FAILED("Validation failed", 1008),
    INTERNAL_ERROR("Internal error", 1009),
    INVALID_TOKEN("Invalid token", 1010),
    TOKEN_EXPIRED("Token has expired", 1011),
    TOKEN_ALREADY_USED("Token already used", 1012),
    INVALID_PURPOSE("Invalid token purpose", 1013),

    // 🔥 THÊM MỚI - Authentication & Authorization
    UNAUTHENTICATED("You are not authenticated", 1014),
    UNAUTHORIZED("You do not have permission to access this resource", 1015),

    // Family Tree related
    FAMILY_TREE_NOT_FOUND("Family tree not found or not accessible", 1100),
    PERSON_NOT_FOUND("Person not found", 1101),
    RELATIONSHIP_INVALID_TYPE("Invalid relationship type", 1102),
    RELATIONSHIP_ALREADY_EXISTS("Relationship already exists", 1103),
    RELATIONSHIP_NOT_SAME_TREE("Persons are not in the same family tree", 1104),
    RELATIONSHIP_NOT_FOUND("Relationship not found", 1105),
    RELATIONSHIP_SELF_LINK("Cannot create relationship to the same person", 1106),
    ;

    private final int code;
    private final String message;

    ErrorCode(String message, int code) {
        this.message = message;
        this.code = code;
    }

    public int getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }
}