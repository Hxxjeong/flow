package com.flow.domain.fileReject.dto.request;

import jakarta.validation.constraints.Max;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class FileRejectReqDto {
    @Max(value = 20, message = "확장자 최대 입력 길이는 20자입니다.")
    private String extension;
}
