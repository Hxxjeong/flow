package com.flow.domain.fileReject.dto.response;

import com.flow.domain.fileReject.entity.FileReject;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class FileRejectRspDto {
    private Long id;
    private String extension;
    private Boolean checked;
    private Boolean isDefault;

    public static FileRejectRspDto from(FileReject fileReject) {
        return FileRejectRspDto.builder()
                .id(fileReject.getId())
                .extension(fileReject.getExtension())
                .checked(fileReject.getChecked())
                .isDefault(fileReject.getIsDefault())
                .build();
    }
}
