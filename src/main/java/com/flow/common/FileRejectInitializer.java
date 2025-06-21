package com.flow.common;

import com.flow.domain.fileReject.entity.FileReject;
import com.flow.domain.fileReject.repository.FileRejectRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class FileRejectInitializer implements CommandLineRunner {
    private final FileRejectRepository fileRejectRepository;

    private static final List<String> DEFAULT_EXTENSIONS = List.of("bat", "cmd", "com", "cpl", "exe", "scr", "js");

    @Override
    public void run(String... args) {
        for (String ext : DEFAULT_EXTENSIONS) {
            fileRejectRepository.findByExtension(ext)
                    .or(() -> Optional.of(fileRejectRepository.save(
                            FileReject.builder().extension(ext).checked(false).build()
                    )));
        }
    }
}
