package com.legacymap.backend.exception;

public enum ErrorCode {
    USER_EXISTED ("username already existed",1001),
    EMAIL_EXISTED ( " email already existed",1002),
    SEND_EMAIL_FAILED ( "send email failed",1003),
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
