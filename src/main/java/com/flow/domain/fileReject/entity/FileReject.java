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
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "extension", unique = true, nullable = false)
    private String extension;

    @Column(name = "checked")
    private Boolean checked;

    @Column(name = "is_default", nullable = false)
    private Boolean isDefault = false;

    @Builder
    private FileReject(String extension, Boolean checked, Boolean isDefault) {
        this.extension = extension;
        this.checked = checked;
        this.isDefault = isDefault;
    }

    public void check() {
        this.checked = true;
    }

    public void uncheck() {
        this.checked = false;
    }
}
