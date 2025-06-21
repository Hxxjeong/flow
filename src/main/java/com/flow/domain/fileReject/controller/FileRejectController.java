package com.flow.domain.fileReject.controller;

import com.flow.common.RspTemplate;
import com.flow.domain.fileReject.dto.request.FileRejectReqDto;
import com.flow.domain.fileReject.dto.response.FileRejectRspDto;
import com.flow.domain.fileReject.service.FileRejectService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/file-reject")
public class FileRejectController {
    private final FileRejectService fileRejectService;

    // 확장자 등록
    @PostMapping
    public RspTemplate<List<FileRejectRspDto>> createFileReject(@RequestBody FileRejectReqDto reqDTO) {
        return new RspTemplate<>(HttpStatus.CREATED, "파일 확장자가 등록되었습니다.", fileRejectService.createFileReject(reqDTO));
    }

    // 확장자 조회
    @GetMapping
    public RspTemplate<List<FileRejectRspDto>> getAllFileRejects() {
        return new RspTemplate<>(HttpStatus.OK, "현재 등록되어 있는 확장자", fileRejectService.getAllFileRejects());
    }

    // 확장자 삭제
    @DeleteMapping("/{id}")
    public RspTemplate<Void> deleteFileReject(@PathVariable Long id) {
        fileRejectService.deleteFileReject(id);
        return new RspTemplate<>(HttpStatus.OK, "확장자가 삭제되었습니다.");
    }
}
