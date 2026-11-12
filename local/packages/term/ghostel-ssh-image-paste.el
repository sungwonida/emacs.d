;;; ghostel-ssh-image-paste.el --- C-v로 SSH 너머 Claude Code에 클립보드 이미지 붙여넣기  -*- lexical-binding: t; -*-

;;; Commentary:

;; 문제
;; ----
;; Claude Code의 C-v(chat:imagePaste)는 "Claude Code가 돌고 있는 머신"의
;; 클립보드를 읽는다.  ssh 너머(tmux 유무 무관)에서는 그게 원격 머신의
;; 클립보드라 로컬 Mac의 스크린샷이 보일 리 없고, "no image over SSH · copy
;; the file over, paste its path" 안내만 뜬다.  터미널 프로토콜(OSC 52 등)로
;; 클립보드를 물어보는 경로는 Claude Code에 없으므로 tmux 설정
;; (allow-passthrough, set-clipboard)으로는 고칠 수 없다.
;;
;; 해법
;; ----
;; Claude Code는 "붙여넣기 이벤트(bracketed paste)로 들어온, 이미지 확장자
;; (.png/.jpg/.jpeg/.gif/.webp)로 끝나는 절대 경로"를 이미지 첨부로 바꿔 준다.
;; 그래서 ghostel 버퍼의 C-v를 다음처럼 바꾼다:
;;
;;   1. 버퍼 tty의 포그라운드 프로세스가 `ssh DEST' 이고
;;   2. macOS 클립보드에 PNG 이미지가 있으면
;;      → 이미지를 ssh(ControlMaster 재사용, ProxyJump는 ssh config가 처리)로
;;        DEST:~/.cache/claude-paste/ 에 올리고, 그 원격 절대 경로를
;;        bracketed paste로 보낸다.  → Claude Code에 [Image #N] 으로 붙는다.
;;   3. 그 외(로컬 셸이거나 클립보드에 이미지가 없음)에는 원래대로 raw ^V 를
;;      보낸다.  로컬 Claude Code의 이미지 붙여넣기는 그대로 동작한다.
;;
;; 제약
;; ----
;; - 경로는 반드시 paste 이벤트 + 절대 경로 + 경로만 단독이어야 첨부된다
;;   (Claude Code 2.1.322 바이너리에서 확인).  원격에 Read deny 규칙이 걸려
;;   있으면 경로가 그냥 텍스트로 들어간다(그래도 Claude가 Read로 열 수 있다).
;; - 호스트는 "ghostel tty의 포그라운드 ssh" 의 첫 목적지 인자다.  로컬 tmux
;;   안에서 ssh를 띄우거나, 원격에서 다시 ssh로 건너간 경우는 알 수 없다
;;   (후자는 첫 홉에 올라간다).  `ssh -p PORT -l USER host' 처럼 명령줄
;;   옵션으로 준 접속 정보는 업로드에 승계되지 않는다 — ssh config 별칭 권장.
;; - semi-char / char 입력 모드에서만 동작(copy 모드의 C-v는 스크롤).
;; - 원격 파일은 0600, 7일 지난 것은 다음 업로드 때 지운다.

;;; Code:

(require 'ghostel)
(require 'cl-lib)

(defgroup my/ghostel-image-paste nil
  "ssh 너머 Claude Code로 클립보드 이미지 붙여넣기."
  :group 'ghostel)

(defcustom my/ghostel-image-remote-subdir ".cache/claude-paste"
  "원격 $HOME 기준, 붙여넣은 이미지를 올릴 디렉터리."
  :type 'string)

(defcustom my/ghostel-image-keep-days 7
  "원격에 올린 이미지를 보관할 일수.  nil이면 지우지 않는다."
  :type '(choice (const :tag "지우지 않음" nil) integer))

(defconst my/ghostel--ssh-opts-with-arg "BbcDEeFIiJLlmOoPpQRSWw"
  "다음 인자를 값으로 먹는 ssh(1) 옵션 글자들 (OpenSSH 10 기준).")

(defun my/ghostel--parse-ssh-destination (args)
  "ssh 명령 인자 리스트 ARGS 에서 목적지(첫 비옵션 인자)를 돌려준다."
  (let (dest)
    (while (and args (not dest))
      (let ((a (pop args)))
        (cond ((not (string-prefix-p "-" a)) (setq dest a))
              ;; `-p 22' 처럼 값이 따로 오는 옵션만 다음 인자를 건너뛴다.
              ;; (`-p22', `-4', `-tt' 는 한 토큰으로 끝)
              ((and (= (length a) 2)
                    (cl-find (aref a 1) my/ghostel--ssh-opts-with-arg))
               (pop args)))))
    dest))

(defun my/ghostel--ssh-destination ()
  "이 버퍼 tty의 포그라운드 `ssh' 목적지, 없으면 nil."
  (when-let* ((proc ghostel--process)
              ((process-live-p proc))
              (tty (process-tty-name proc))
              (out (with-temp-buffer
                     (when (eq 0 (call-process "ps" nil t nil "-t"
                                               (file-name-nondirectory tty)
                                               "-o" "pid=,tpgid=,command="))
                       (buffer-string)))))
    (cl-loop
     for line in (split-string out "\n" t)
     for f = (split-string line)
     ;; 포그라운드 프로세스 그룹 리더만 (ProxyJump용 자식 ssh -W 는 제외)
     when (and (equal (nth 0 f) (nth 1 f))
               (equal (file-name-nondirectory (or (nth 2 f) "")) "ssh"))
     return (my/ghostel--parse-ssh-destination (nthcdr 3 f)))))

(defun my/ghostel--clipboard-png ()
  "클립보드 이미지를 unibyte PNG 문자열로, 없으면 nil."
  (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
    ;; TARGETS 확인 없이 image/png 를 요청하면 텍스트 클립보드가 그대로 온다.
    (when (and (vectorp targets) (cl-find 'image/png targets))
      (let ((data (ignore-errors (gui-get-selection 'CLIPBOARD 'image/png))))
        (and (stringp data)
             (> (length data) 8)
             (string-prefix-p "\x89PNG" (string-to-unibyte data))
             (string-to-unibyte data))))))

(defun my/ghostel--remote-store-command (name)
  "stdin을 원격 NAME 파일로 저장하고 절대 경로를 출력하는 원격 셸 명령."
  (let ((script
         (concat
          "umask 077; d=\"$HOME/" my/ghostel-image-remote-subdir "\"; "
          "mkdir -p \"$d\" || exit 1; "
          (if my/ghostel-image-keep-days
              (format "find \"$d\" -name \"clip-*.png\" -mtime +%d -delete 2>/dev/null; "
                      my/ghostel-image-keep-days)
            "")
          "f=\"$d/" name "\"; cat > \"$f\" && printf %s \"$f\"")))
    ;; 원격 로그인 셸이 무엇이든 sh 로 해석시킨다.
    (concat "sh -c " (shell-quote-argument script))))

(defun my/ghostel-send-raw-ctrl-v ()
  "터미널에 raw ^V (0x16) 를 보낸다 — ghostel 기본 C-v 동작."
  (ghostel-send-string "\x16"))

(defun my/ghostel-paste-image-dwim ()
  "ssh 버퍼 + 클립보드 이미지면 원격에 올리고 경로를 붙여넣는다.
그 외에는 raw ^V 를 보낸다."
  (interactive)
  (let ((dest (my/ghostel--ssh-destination))
        (png nil))
    (if (not (and dest (setq png (my/ghostel--clipboard-png))))
        (my/ghostel-send-raw-ctrl-v)
      (let* ((name (format "clip-%s-%04x.png"
                           (format-time-string "%Y%m%d-%H%M%S") (random 65536)))
             (buf (current-buffer))
             (out (generate-new-buffer " *ghostel-image-upload*"))
             (err (generate-new-buffer " *ghostel-image-upload-err*"))
             (proc
              (make-process
               :name "ghostel-image-upload"
               :buffer out
               :stderr err
               :noquery t
               :connection-type 'pipe
               :coding 'no-conversion
               :command (list "ssh" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10"
                              "-T" dest (my/ghostel--remote-store-command name))
               :sentinel
               (lambda (p _event)
                 (when (memq (process-status p) '(exit signal))
                   (let ((path (with-current-buffer out (string-trim (buffer-string))))
                         (errs (if (buffer-live-p err)
                                   (with-current-buffer err (string-trim (buffer-string)))
                                 "")))
                     (cond
                      ((not (and (eq (process-exit-status p) 0)
                                 (file-name-absolute-p path)
                                 (string-suffix-p ".png" path)))
                       (message "이미지 업로드 실패 (%s): %s" dest
                                (if (string-empty-p errs) path errs)))
                      ((not (buffer-live-p buf))
                       (message "이미지는 올렸지만 터미널 버퍼가 사라짐: %s:%s" dest path))
                      (t
                       (with-current-buffer buf (ghostel-paste-string path))
                       (message "이미지 붙여넣음 → %s:%s" dest path))))
                   (when (buffer-live-p out) (kill-buffer out))
                   (when (buffer-live-p err)
                     (when-let* ((ep (get-buffer-process err))) (delete-process ep))
                     (kill-buffer err)))))))
        (message "클립보드 이미지(%d KB)를 %s 로 올리는 중..."
                 (/ (length png) 1024) dest)
        (process-send-string proc png)
        (process-send-eof proc)))))

;; semi-char 맵은 `ghostel--rebuild-semi-char-keymap' 이 C-<letter> 를 전부
;; raw 전송으로 다시 깔기 때문에(예: `ghostel-keymap-exceptions' 변경 시)
;; rebuild 뒤마다 다시 바인딩한다.  char 모드 맵은 재생성되지 않는다.
(defun my/ghostel--bind-image-paste (&rest _)
  "C-v 를 `my/ghostel-paste-image-dwim' 으로 (재)바인딩."
  (define-key ghostel-semi-char-mode-map (kbd "C-v") #'my/ghostel-paste-image-dwim)
  (define-key ghostel-char-mode-map (kbd "C-v") #'my/ghostel-paste-image-dwim))

(my/ghostel--bind-image-paste)
(advice-add 'ghostel--rebuild-semi-char-keymap :after #'my/ghostel--bind-image-paste)

(provide 'ghostel-ssh-image-paste)
;;; ghostel-ssh-image-paste.el ends here
