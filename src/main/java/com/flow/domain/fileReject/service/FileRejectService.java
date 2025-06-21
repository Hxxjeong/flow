package com.flow.domain.fileReject.service;

import com.flow.common.exception.BusinessException;
import com.flow.common.exception.ErrorCode;
import com.flow.domain.fileReject.dto.request.FileRejectReqDto;
import com.flow.domain.fileReject.dto.response.FileRejectRspDto;
import com.flow.domain.fileReject.entity.FileReject;
import com.flow.domain.fileReject.repository.FileRejectRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class FileRejectService {
    private final FileRejectRepository fileRejectRepository;

    // 확장자 등록
    @Transactional
    public FileRejectRspDto createFileReject(FileRejectReqDto dto) {
        String ext = dto.getExtension().toLowerCase();  // 소문자 통일
        FileReject fileReject = fileRejectRepository.findByExtension(ext).orElse(null);

        // 새로운 확장자 등록
        if(fileReject == null) {
            // 최대 200개 제한
            List<FileReject> allByCheckedTrue = fileRejectRepository.findAllByCheckedTrue();
            if (allByCheckedTrue.size() > 208)
                throw new BusinessException(ErrorCode.FULL_EXTENSION);

            FileReject newFileReject = FileReject.builder()
                    .extension(ext)
                    .checked(true)
                    .isDefault(false)
                    .build();
            fileRejectRepository.save(newFileReject);
            return FileRejectRspDto.from(newFileReject);
        }

        // 기존 값 토글
        if(Boolean.TRUE.equals(fileReject.getIsDefault())) {
            if(Boolean.TRUE.equals(fileReject.getChecked())) fileReject.uncheck();
            else fileReject.check();
            return FileRejectRspDto.from(fileReject);
        }

        if(Boolean.FALSE.equals(fileReject.getChecked())) {
            fileReject.check();
            return FileRejectRspDto.from(fileReject);
        }

        throw new BusinessException(ErrorCode.ALREADY_EXTENSION);
    }

    // 확장자 조회
    public List<FileRejectRspDto> getAllFileRejects() {
        return fileRejectRepository.findAllByCheckedTrue().stream()
                .map(FileRejectRspDto::from)
                .toList();
    }

    // 확장자 삭제
    @Transactional
    public void deleteFileReject(long id) {
        FileReject fileReject = fileRejectRepository.findById(id)
                .orElseThrow(() -> new BusinessException(ErrorCode.NOT_FOUND_EXTENSION));
        fileReject.uncheck();
    }
}
