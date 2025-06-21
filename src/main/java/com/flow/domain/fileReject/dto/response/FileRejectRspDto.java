package com.flow.domain.fileReject.dto.response;

import com.flow.domain.fileReject.entity.FileReject;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class FileRejectRspDto {
    private String extension;
    private Boolean checked;

    public static FileRejectRspDto from(FileReject fileReject) {
        return FileRejectRspDto.builder()
                .extension(fileReject.getExtension())
                .checked(fileReject.getChecked())
                .build();
    }
}
