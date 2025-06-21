package com.flow.domain.fileReject.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "file_reject")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FileReject {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    @Column(name = "extension", unique = true, nullable = false)
    private String extension;

    @Column(name = "checked")
    private Boolean checked;

    @Builder
    private FileReject(String extension, Boolean checked) {
        this.extension = extension;
        this.checked = checked;
    }

    public void check() {
        this.checked = true;
    }

    public void uncheck() {
        this.checked = false;
    }
}
