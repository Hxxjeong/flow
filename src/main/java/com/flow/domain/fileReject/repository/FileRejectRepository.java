package com.flow.domain.fileReject.repository;

import com.flow.domain.fileReject.entity.FileReject;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface FileRejectRepository extends JpaRepository<FileReject, Long> {
    Optional<FileReject> findByExtension(String extension);

    List<FileReject> findAllByCheckedTrue();
}
