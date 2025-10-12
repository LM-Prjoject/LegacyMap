package com.legacymap.backend.exception;

import com.legacymap.backend.dto.response.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.util.stream.Collectors;

@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(value = AppException.class)
    ResponseEntity<ApiResponse> handlingAppException(AppException exception){
        ErrorCode errorCode = exception.getErrorCode();
        ApiResponse apiResponse = new ApiResponse();

        apiResponse.setCode(errorCode.getCode());
        apiResponse.setMessage(errorCode.getMessage());

        return ResponseEntity.badRequest().body(apiResponse);
    }

    // GHI CHÚ: Handler cho lỗi validate @Valid (DTO request)
    // - Gộp các lỗi field thành một chuỗi ngắn gọn.
    // - Nếu bạn có ErrorCode riêng cho validation (ví dụ VALIDATION_FAILED), hãy map vào apiResponse.setCode(...) cho đồng nhất.
    @ExceptionHandler(MethodArgumentNotValidException.class)
    ResponseEntity<ApiResponse> handleValidation(MethodArgumentNotValidException ex) {
        String combined = ex.getBindingResult()
                .getAllErrors()
                .stream()
                .map(err -> {
                    if (err instanceof FieldError fe) {
                        return fe.getField() + ": " + err.getDefaultMessage();
                    }
                    return err.getObjectName() + ": " + err.getDefaultMessage();
                })
                .collect(Collectors.joining("; "));

        ApiResponse api = new ApiResponse();
        api.setCode(HttpStatus.BAD_REQUEST.value()); // GHI CHÚ: thay bằng errorCode.getCode() nếu bạn có ErrorCode.VALIDATION_FAILED
        api.setMessage(combined);

        return ResponseEntity.badRequest().body(api);
    }

    // GHI CHÚ: Handler cho lỗi từ Supabase Auth
    // - httpStatus dùng statusCode trả về từ Supabase nếu có, mặc định 502 (BAD_GATEWAY).
    // - api.setCode hiện đang dùng httpStatus cho đơn giản. Nếu bạn muốn dùng mã nội bộ (ErrorCode), map lại ở đây.
    // - Tránh lộ responseBody ra client ở môi trường production; nên log thay vì trả về.
    @ExceptionHandler(SupabaseAuthException.class)
    ResponseEntity<ApiResponse> handleSupabaseAuth(SupabaseAuthException ex) {
        int httpStatus = ex.getStatusCode() > 0 ? ex.getStatusCode() : HttpStatus.BAD_GATEWAY.value();

        ApiResponse api = new ApiResponse();
        api.setCode(httpStatus); // GHI CHÚ: thay bằng mã nội bộ nếu dùng ErrorCode (ví dụ ErrorCode.SUPABASE_AUTH_ERROR.getCode()).
        String message = ex.getMessage();
        if (ex.getResponseBody() != null && !ex.getResponseBody().isBlank()) {
            message = message + " - " + ex.getResponseBody(); // GHI CHÚ: cân nhắc bỏ phần body ở production.
        }
        api.setMessage(message);

        return ResponseEntity.status(httpStatus).body(api);
    }

    // GHI CHÚ: Handler tổng cho lỗi không lường trước.
    // - Bạn có thể thêm log tại đây.
    @ExceptionHandler(Exception.class)
    ResponseEntity<ApiResponse> handleUnexpected(Exception ex) {
        ApiResponse api = new ApiResponse();
        api.setCode(HttpStatus.INTERNAL_SERVER_ERROR.value()); // GHI CHÚ: hoặc map sang ErrorCode.INTERNAL_ERROR nếu có.
        api.setMessage("Unexpected error");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(api);
    }
}

