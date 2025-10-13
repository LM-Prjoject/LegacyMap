package com.legacymap.backend.exception;

public enum ErrorCode {
    USER_EXISTED ("username already existed",1001),
    EMAIL_EXISTED ( " email already existed",1002),
    SEND_EMAIL_FAILED ( "send email failed",1003),
    USER_NOT_FOUND("User not found",1004),
    INVALID_CREDENTIALS("Invalid username/email or password",1005),
    ACCOUNT_NOT_VERIFIED("Account is not verified",1006),
    ACCOUNT_DISABLED("Account is deactivated",1007),
    VALIDATION_FAILED("Validation failed", 1008),
    INTERNAL_ERROR("Internal error", 1009),
    INVALID_TOKEN("Invalid token", 1010),
    TOKEN_EXPIRED("Token has expired", 1011),
    TOKEN_ALREADY_USED("Token already used", 1012),
    INVALID_PURPOSE("Invalid token purpose", 1013),
    ;
    ErrorCode(String message, int code) {
        this.message = message;
        this.code = code;
    }
    private int code;
    private String message;

    public int getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }
}
