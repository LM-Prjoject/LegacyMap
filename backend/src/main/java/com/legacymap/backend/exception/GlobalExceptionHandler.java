package com.legacymap.backend.exception;

import com.legacymap.backend.dto.response.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(AppException.class)
    public ResponseEntity<ApiResponse<Object>> handleAppException(AppException e) {
        return ResponseEntity.badRequest()
                .body(ApiResponse.error(e.getErrorCode().getCode(), e.getMessage()));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ApiResponse<Object>> handleValidationExceptions(MethodArgumentNotValidException ex) {
        Map<String, String> errors = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
                errors.put(error.getField(), error.getDefaultMessage())
        );
        return ResponseEntity.badRequest()
                .body(ApiResponse.error(1001, "Validation failed: " + errors.toString()));
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ApiResponse<Object>> handleGenericException(Exception e) {
        return ResponseEntity.internalServerError()
                .body(ApiResponse.error(500, "Internal server error: " + e.getMessage()));
    }
}

// @RestControllerAdvice
// public class GlobalExceptionHandler {

//     @ExceptionHandler(value = AppException.class)
//     ResponseEntity<ApiResponse> handlingAppException(AppException exception){
//         ErrorCode ec = exception.getErrorCode();

//         ApiResponse<?> body = ApiResponse.builder()
//                 .code(ec.getCode())
//                 .message(ec.getMessage())
//                 .build();

//         return ResponseEntity.badRequest().body(body);
//     }

//     @ExceptionHandler(MethodArgumentNotValidException.class)
//     public ResponseEntity<ApiResponse<Map<String, String>>> handleValidation(MethodArgumentNotValidException ex) {
//         Map<String, String> errors = new HashMap<>();
//         ex.getBindingResult().getFieldErrors()
//                 .forEach(err -> errors.put(err.getField(), err.getDefaultMessage()));

//         ApiResponse<Map<String, String>> body = ApiResponse.<Map<String, String>>builder()
//                 .code(ErrorCode.VALIDATION_FAILED.getCode())
//                 .message(ErrorCode.VALIDATION_FAILED.getMessage())
//                 .result(errors)
//                 .build();

//         return ResponseEntity.badRequest().body(body);
//     }

//     @ExceptionHandler(Exception.class)
//     public ResponseEntity<ApiResponse<Void>> handleOther(Exception ex) {
//         ApiResponse<Void> body = ApiResponse.<Void>builder()
//                 .code(ErrorCode.INTERNAL_ERROR.getCode())
//                 .message(ErrorCode.INTERNAL_ERROR.getMessage())
//                 .build();

//         return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(body);