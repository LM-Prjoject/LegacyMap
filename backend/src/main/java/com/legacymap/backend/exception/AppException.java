package com.legacymap.backend.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
public class AppException extends RuntimeException{

  public AppException(ErrorCode errorCode) {
    super(errorCode.getMessage());
    this.errorCode = errorCode;
  }

  public AppException(ErrorCode errorCode, String message) {
    super(message);
    this.errorCode = errorCode;
  }

  private ErrorCode errorCode;
}
