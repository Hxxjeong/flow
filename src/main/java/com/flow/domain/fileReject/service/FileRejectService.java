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

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class FileRejectService {
    private final FileRejectRepository fileRejectRepository;

    // 확장자 등록
    @Transactional
    public List<FileRejectRspDto> createFileReject(FileRejectReqDto dto) {
        String ext = dto.getExtension().toLowerCase();  // 소문자 통일

        // 입력값 없는 경우
        if(ext == null || ext.isBlank())
            throw new BusinessException(ErrorCode.BLANK_EXTENSION);

        // 여러 개 입력하는 경우 공백으로 구분
        String[] extensions = ext.trim().toLowerCase().split("\\s+");
        List<FileReject> allByCheckedTrue = fileRejectRepository.findAllByCheckedTrue();

        // 최대 200개 제한
        if(allByCheckedTrue.size() + extensions.length > 208)
            throw new BusinessException(ErrorCode.FULL_EXTENSION);

        List<FileRejectRspDto> savedDtos = new ArrayList<>();

        for(String e: extensions) {
            if(e.isBlank()) continue;

            // 영어만 허용
            if(!e.matches("^[a-z]+$"))
                throw new BusinessException(ErrorCode.INVALID_EXTENSION);

            FileReject fileReject = fileRejectRepository.findByExtension(e).orElse(null);

            // 새로운 확장자 등록
            if(fileReject == null) {
                FileReject newFileReject = FileReject.builder()
                        .extension(e)
                        .checked(true)
                        .isDefault(false)
                        .build();
                fileRejectRepository.save(newFileReject);
                savedDtos.add(FileRejectRspDto.from(newFileReject));
                continue;
            }

            // 기본 확장자인 경우 toggle
            if (Boolean.TRUE.equals(fileReject.getIsDefault())) {
                if (Boolean.TRUE.equals(fileReject.getChecked())) fileReject.uncheck();
                else fileReject.check();
                savedDtos.add(FileRejectRspDto.from(fileReject));
                continue;
            }

            // 비활성화 되어 있던 커스텀 확장자
            if (Boolean.FALSE.equals(fileReject.getChecked())) {
                fileReject.check();
                savedDtos.add(FileRejectRspDto.from(fileReject));
                continue;
            }

            // 이미 존재하는 커스텀 확장자인 경우 예외 발생
            throw new BusinessException(ErrorCode.ALREADY_EXTENSION);
        }

        return savedDtos;
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
