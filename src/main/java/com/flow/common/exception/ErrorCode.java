package com.flow.common.exception;

import lombok.Getter;

@Getter
public enum ErrorCode {
    // 확장자 관련
    ALREADY_EXTENSION(500, "이미 존재하는 확장자입니다."),
    NOT_FOUND_EXTENSION(500, "존재하지 않는 확장자입니다.")
    ;

    private final int code;
    private final String message;

    ErrorCode(int code, String message) {
        this.code = code;
        this.message = message;
    }
}