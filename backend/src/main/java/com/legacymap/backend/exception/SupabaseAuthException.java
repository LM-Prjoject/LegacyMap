package com.legacymap.backend.exception;

/**
 * Exception dùng để wrap lỗi từ Supabase Auth (HTTP status + body).
 * - statusCode: mã HTTP trả về từ Supabase (nếu có). Nếu = 0 coi như chưa xác định.
 * - responseBody: nội dung trả về từ Supabase để debug hoặc hiển thị chi tiết (cân nhắc log thay vì trả ra client).
 */
public class SupabaseAuthException extends RuntimeException {
    private final int statusCode;
    private final String responseBody;

    public SupabaseAuthException(String message, int statusCode, String responseBody) {
        super(message);
        this.statusCode = statusCode;
        this.responseBody = responseBody;
    }

    public SupabaseAuthException(String message, int statusCode, String responseBody, Throwable cause) {
        super(message, cause);
        this.statusCode = statusCode;
        this.responseBody = responseBody;
    }

    public int getStatusCode() { return statusCode; }

    public String getResponseBody() { return responseBody; }
}


