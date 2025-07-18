package com.flow.common.exception;

import lombok.Getter;

@Getter
public enum ErrorCode {
    // 확장자 관련
    BLANK_EXTENSION(400, "값을 입력해주세요."),
    INVALID_EXTENSION(500, "확장자는 영어로 입력해주세요."),
    ALREADY_EXTENSION(500, "이미 존재하는 확장자입니다."),
    NOT_FOUND_EXTENSION(500, "존재하지 않는 확장자입니다."),
    FULL_EXTENSION(500, "확장자는 최대 200개까지 등록 가능합니다."),
    DEFAULT_EXTENSION_INPUT(500, "기본 확장자입니다.")
    ;

    private final int code;
    private final String message;

    ErrorCode(int code, String message) {
        this.code = code;
        this.message = message;
    }
}