package com.legacymap.backend.exception;

public enum ErrorCode {
    USER_EXISTED ("username already existed",1001),
    EMAIL_EXISTED ( " email already existed",1002),
    SEND_EMAIL_FAILED ( "send email failed",1003),
    USER_NOT_FOUND("User not found",1004),
    INVALID_CREDENTIALS("Invalid username/email or password",1005),
    ACCOUNT_NOT_VERIFIED("Account is not verified",1006),
    ACCOUNT_DISABLED("Account is deactivated",1007);
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
