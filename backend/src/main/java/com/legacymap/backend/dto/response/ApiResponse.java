package com.legacymap.backend.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.legacymap.backend.exception.ErrorCode;
import lombok.*;
import lombok.experimental.FieldDefaults;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
@JsonInclude(JsonInclude.Include.NON_NULL)

public class ApiResponse<T> {
    private boolean success;
    private int code = 1000;
    private String message;
    private T result;

    public static <T> ApiResponse<T> success(T result, String message) {
        ApiResponse<T> res = new ApiResponse<>();
        res.setSuccess(true);
        res.setCode(1000);
        res.setMessage(message);
        res.setResult(result);
        return res;
    }

    public static <T> ApiResponse<T> error(int code, String message) {
        ApiResponse<T> response = new ApiResponse<>();
        response.setSuccess(false);
        response.setCode(code);
        response.setMessage(message);
        return response;
    }

    public static <T> ApiResponse<T> error(ErrorCode errorCode, String overrideMessage) {
        ApiResponse<T> res = new ApiResponse<>();
        res.success = false;
        res.code = errorCode.getCode();
        res.message = overrideMessage;
        return res;
    }
}