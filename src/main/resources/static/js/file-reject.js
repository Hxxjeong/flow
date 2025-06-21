const defaultExtensionsSet = new Set(["bat", "cmd", "com", "cpl", "exe", "scr", "js"]);

document.addEventListener("DOMContentLoaded", () => {
    loadExtensions();

    const input = document.getElementById("custom-extension");
    input.addEventListener("keydown", (event) => {
        if (event.key === "Enter") {
            event.preventDefault();
            addCustomExtension();
        }
    });
});


function loadExtensions() {
    fetch("/file-reject")
        .then(res => res.json())
        .then(data => {
            const extensions = data.data;
            renderDefaultExtensions(extensions);
            renderCustomExtensions(extensions);
        });
}

function renderDefaultExtensions(extensions) {
    const container = document.getElementById("default-extensions");
    container.innerHTML = "";

    defaultExtensionsSet.forEach(ext => {
        const matched = extensions.find(e => e.extension === ext);
        const isChecked = matched?.checked === true;

        const label = document.createElement("label");
        label.style.marginRight = "10px";

        const checkbox = document.createElement("input");
        checkbox.type = "checkbox";
        checkbox.checked = isChecked;
        checkbox.onchange = () => toggleExtension(ext);

        label.appendChild(checkbox);
        label.appendChild(document.createTextNode(" " + ext));
        container.appendChild(label);
    });
}


function renderCustomExtensions(extensions) {
    const container = document.getElementById("custom-extensions");
    container.innerHTML = "";

    extensions
        .filter(ext => !defaultExtensionsSet.has(ext.extension) && ext.checked)
        .forEach(ext => {
            const span = document.createElement("span");
            span.style.marginRight = "10px";

            const name = document.createTextNode(ext.extension + " ");
            const removeBtn = document.createElement("button");
            removeBtn.textContent = "x";
            removeBtn.onclick = () => deleteExtension(ext.id);

            span.appendChild(name);
            span.appendChild(removeBtn);
            container.appendChild(span);
        });
}

function toggleExtension(extension) {
    fetch("/file-reject", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ extension })
    })
        .then(res => res.json())
        .then(() => loadExtensions())
        .catch(err => console.error(err));
}

function addCustomExtension() {
    const input = document.getElementById("custom-extension");
    const error = document.getElementById("error-message");
    const ext = input.value.trim().toLowerCase();

    // 에러 메시지 출력 함수
    const setError = (msg) => {
        error.textContent = msg;
    };

    // 입력값 유효성 검사
    if (!ext) {
        setError("확장자를 입력하세요.");
        return;
    }

    if (defaultExtensionsSet.has(ext)) {
        setError("기본 확장자입니다. 활성화 상태를 변경해주세요.");
        return;
    }

    if (ext.length > 20) {
        setError("확장자 최대 입력 길이는 20자입니다.");
        return;
    }

    // POST 요청
    fetch("/file-reject", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ extension: ext })
    })
        .then(res => res.json())
        .then(data => {
            if (data.code >= 400) {
                // 서버에서 내려준 에러 메시지 사용
                setError(data.errorMessage || "이미 존재하는 확장자입니다.");
            } else {
                setError(""); // 에러 메시지 제거
                input.value = "";
                loadExtensions(); // 확장자 목록 다시 불러오기
            }
        })
        .catch(err => {
            setError("서버 요청 중 오류가 발생했습니다.");
            console.error(err);
        });
}

function deleteExtension(id) {
    const confirmed = confirm("확장자를 삭제하시겠습니까?");
    if (!confirmed) return;

    fetch(`/file-reject/${id}`, {
        method: "DELETE"
    })
        .then(res => res.json())
        .then(() => loadExtensions())
        .catch(err => console.error(err));
}
