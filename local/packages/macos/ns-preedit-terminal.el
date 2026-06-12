;;; ns-preedit-terminal.el --- macOS NS IME preedit를 터미널(vterm/ghostel)에서 올바르게 표시 -*- lexical-binding: t; -*-

;;; Commentary:
;; Purpose
;; -------
;; macOS NS 포트는 IME 조합 중 글자(preedit, "working text")를 point 위치의
;; zero-width 오버레이(`ns-working-overlay'의 'after-string)로 표시한다.
;; vterm/ghostel 같은 터미널 에뮬레이터 버퍼는 네이티브 모듈이 줄을 통째로
;; delete+insert로 다시 그리는 구조라 이 오버레이가 깨진다.  세 가지 수정:
;;
;;   1. (1차) re-anchor: 줄 재삽입 때 zero-width 오버레이가 Emacs 마커 규칙에
;;      따라 삭제 시작점(줄 맨 앞)으로 끌려가 preedit가 0열에 그려지고 줄
;;      전체가 좌우로 흔들린다.  → 두 터미널의 redraw 단일 진입점에 :after
;;      advice로 오버레이를 터미널 커서 위치로 되돌린다.
;;   2. (2차) 'cursor 속성: preedit 오른쪽에 버퍼 글자가 있으면(Claude Code
;;      placeholder 등) 커서 박스가 조합 글자가 아닌 그 다음 글자 위에
;;      그려진다(xdisp exact-match 규칙).  → preedit 첫 글자에 정수 'cursor 1.
;;   3. (2차) 폭 보존 cover: after-string은 줄에 표시폭을 "삽입"하므로 TUI
;;      입력줄에서 커서 오른쪽 내용이 음절마다 밀렸다 돌아온다.  → 터미널
;;      버퍼에서는 커서 오른쪽 셀을 오버레이 'display로 "덮어서" 그린다
;;      (네이티브 IME가 화면 위에 덧그리는 방식과 동일).  줄 끝은 기존
;;      after-string 폴백.
;;
;; Requirements
;; ------------
;; - macOS NS 포트 (GNU Emacs 30.x).  init.el에서
;;   `:if (eq system-type \'darwin)' 가드 + 모듈 내부 (featurep \'ns) 가드.
;;   Linux/WSL은 통째로 no-op.
;; - vterm / ghostel은 있으면 적용(with-eval-after-load), 없어도 무해.
;;
;; Limitations / 업그레이드 체크리스트
;; -----------------------------------
;; - 내부 심볼 의존: vterm--delayed-redraw, vterm--get-cursor-point,
;;   vterm--window-adjust-process-window-size, ghostel--redraw-now(0.34에서
;;   ghostel--delayed-redraw에서 리네임 — fboundp로 양쪽 대응),
;;   ghostel-cursor-point, ghostel--buffer-editable-p,
;;   ns-working-overlay/ns-insert-working-text(ns-win.el).  패키지/Emacs
;;   업그레이드 후 한글 조합이 이상하면 이 심볼들 존재부터 확인할 것.
;;   (advice는 ignore-errors 래핑이라 깨져도 에러 없이 원래 증상으로 복귀.)
;; - 조합 확정분이 pty 왕복(평시 ~10ms)을 거치는 동안의 미세 지연은 IME
;;   구조상 한계.  조합 중 커서 오른쪽 2칸이 잠깐 가려지는 것도 의도된 동작.
;;
;; History: 2026-06 Claude Code(Ink TUI) 한글 입력 이슈 3건을 배치/GUI
;; 실측으로 진단·검증해 작성.  상세 검증 수치는 아래 각 블록 주석 참고.

;;; Code:

;;; --- macOS(NS) 한글 IME preedit가 vterm/ghostel에서 0열로 튀는 문제 수정 ---
;;; 원인: NS 포트는 조합 중 글자를 point 위치의 zero-width 오버레이
;;; (ns-working-overlay, 'after-string)로 표시한다. vterm/ghostel은 화면 갱신 때
;;; 커서 줄을 통째로 delete-region 후 재삽입하는데, 삭제 범위 안의 zero-width
;;; 오버레이는 Emacs 마커 규칙에 따라 삭제 시작점(=줄 맨 앞)으로 끌려간다.
;;; 그 뒤 preedit가 0열에 그려져 줄 전체가 한 칸 밀려 보인다.
;;; (front/rear-advance 어느 조합도 살아남지 못함 — batch 실험으로 확인)
;;; 해법: 두 터미널의 redraw 단일 진입점 직후, 살아있는 ns-working-overlay를
;;; 터미널 커서 위치로 되돌린다. 확정 텍스트 경로(키 이벤트->pty)는 불변.
;;; 주의: 내부 심볼(vterm--delayed-redraw 등)에 의존 — 패키지 업데이트로 이름이
;;; 바뀌면 에러 없이 조용히 원래 증상으로 돌아간다.
(when (featurep 'ns)                ; NS(Cocoa) 빌드에서만. Linux/WSL은 no-op.
  (defun my/ns-preedit--reanchor (buffer cursor-pt-fn editable-p-fn)
    "BUFFER 안에 살아있는 NS preedit 오버레이를 터미널 커서로 옮긴다.
CURSOR-PT-FN: 터미널 커서 위치(없으면 nil)를 돌려주는 함수.
EDITABLE-P-FN: 키 입력이 pty로 전달되는 상태인지 알려주는 함수."
    ;; process-filter 꼬리에서 실행되므로 어떤 에러도 출력 처리를 끊으면 안 됨.
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay) ; echo-area 표시 중엔 정수 -> skip
                 (buffer-live-p buffer)
                 (eq (overlay-buffer ns-working-overlay) buffer))
        (with-current-buffer buffer
          (when (funcall editable-p-fn)
            (let* ((pt (or (funcall cursor-pt-fn) (point)))
                   (pt (and (integerp pt)
                            (max (point-min) (min pt (point-max))))))
              (when pt
                (move-overlay ns-working-overlay pt pt buffer))))))))

  ;; --- vterm: 모든 lisp 경유 redraw는 vterm--delayed-redraw 하나를 거친다 ---
  (with-eval-after-load 'vterm
    (defun my/ns-preedit--vterm-reanchor (buffer)
      (my/ns-preedit--reanchor
       buffer #'vterm--get-cursor-point
       (lambda () (not (bound-and-true-p vterm-copy-mode)))))
    (advice-add 'vterm--delayed-redraw :after #'my/ns-preedit--vterm-reanchor)
    ;; 창 리사이즈는 C(Fvterm_set_size)에서 직접 term_redraw를 부르므로
    ;; vterm--delayed-redraw를 안 거친다. 리사이즈 직후에도 한 번 재고정.
    (defun my/ns-preedit--vterm-reanchor-after-resize (process &rest _)
      (when (processp process)
        (my/ns-preedit--vterm-reanchor (process-buffer process))))
    (advice-add 'vterm--window-adjust-process-window-size :after
                #'my/ns-preedit--vterm-reanchor-after-resize))

  ;; --- ghostel: 모든 redraw는 단일 진입점 하나를 거친다 ---
  ;; (v0.34.0에서 ghostel--delayed-redraw → ghostel--redraw-now로 리네임.
  ;;  시그니처(buffer)와 단일 진입점 역할은 동일 — 리사이즈 경로
  ;;  ghostel--window-adjust-process-window-size도 이 함수를 부름)
  (with-eval-after-load 'ghostel
    (defun my/ns-preedit--ghostel-reanchor (buffer)
      (my/ns-preedit--reanchor
       buffer #'ghostel-cursor-point
       ;; semi-char/char에서만. copy/emacs/line 모드는 커서≠조합 위치라 제외.
       #'ghostel--buffer-editable-p))
    (advice-add (if (fboundp 'ghostel--redraw-now)
                    'ghostel--redraw-now
                  'ghostel--delayed-redraw)
                :after #'my/ns-preedit--ghostel-reanchor)))

;;; --- (2차) 조합 중 Emacs 커서(녹색 박스)를 조합 글자 위에 고정 ---
;;; Claude Code(2.1.84+)는 IME 인라인 조합을 위해 하드웨어 커서를 입력 캐럿에
;;; 보이게 유지한다. preedit after-string엔 'cursor 속성이 없어서, 커서 오른쪽에
;;; 버퍼 글자가 있으면(Claude placeholder 등) Emacs가 박스를 preedit "다음" 글자
;;; 위에 그린다(xdisp exact-match 규칙) → 조합 글자와 박스가 겹쳐 보임.
;;; 해법: preedit 첫 글자에 정수 'cursor 1 — exact-match 규칙을 override해서
;;; 박스를 조합 글자 위로 가져온다(일반 버퍼의 한글 조합과 같은 모양).
;;; 주의: 아래 "폭 보존" 블록보다 *먼저* 둘 것 — 같은 함수의 :after advice는
;;; 추가한 순서대로 실행되므로, 'cursor가 붙은 문자열을 cover가 가져가야 한다.
(when (featurep 'ns)
  (defun my/ns-preedit--cursor-on-preedit (&rest _)
    "vterm/ghostel에서 preedit 첫 글자에 'cursor 1을 부여."
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay)
                 (derived-mode-p '(vterm-mode ghostel-mode)))
        (let ((str (overlay-get ns-working-overlay 'after-string)))
          (when (and (stringp str) (> (length str) 0))
            (put-text-property 0 1 'cursor 1 str))))))
  (with-eval-after-load 'term/ns-win
    (advice-add 'ns-insert-working-text :after
                #'my/ns-preedit--cursor-on-preedit)))

;;; --- (2차) Claude Code 등 TUI 안 한글 preedit 폭 밀림(출렁임) 수정 ---
;;; 원인: NS preedit는 zero-width 오버레이의 'after-string이라 그 줄에 표시폭
;;; 2칸을 "삽입"한다. zsh 프롬프트에선 커서 오른쪽이 비어 있어 무해하지만,
;;; Claude Code 입력줄에선 커서 오른쪽에 placeholder("Try ...")나 뒷글자가
;;; 있어서 음절을 조합할 때마다 그 내용이 2칸씩 밀렸다 당겨졌다 한다.
;;; 해법: vterm/ghostel 버퍼에서는 after-string 대신 커서 오른쪽 버퍼 셀을
;;; preedit 표시폭만큼 오버레이 'display로 "덮어서" 그린다(폭 보존 →
;;; 레이아웃이 전혀 안 움직임. 네이티브 IME가 화면 위에 덧그리는 방식과 동일).
;;; 줄 끝이라 덮을 셀이 없으면 기존 after-string 폴백 — zsh 프롬프트(EOL)는
;;; 기존과 동일. 줄 "중간" 편집은 동작이 바뀐다: 뒷글자가 밀리는 대신 조합 중
;;; 잠깐 가려졌다가 커밋 시 돌아온다.
;;; 합성: 위의 my/ns-preedit--reanchor가 redraw 후 오버레이를 zero-width로
;;; 재고정하므로, 그 함수에 :after를 걸어 덮는 범위를 매번 재계산한다.
(when (featurep 'ns)
  (defun my/ns-preedit--coverable-p ()
    "현재 버퍼가 폭 보존 덮기를 해도 되는 상태인가(키 입력이 pty로 가는 모드)."
    (or (and (derived-mode-p 'vterm-mode)
             (not (bound-and-true-p vterm-copy-mode)))
        (and (derived-mode-p 'ghostel-mode)
             (fboundp 'ghostel--buffer-editable-p)
             (ghostel--buffer-editable-p))))

  (defun my/ns-preedit--cover (ov)
    "ns-working-overlay OV를 폭 보존형으로 (재)설정한다.
overlay-start(=터미널 커서)부터 preedit 표시폭 이상이 될 때까지 같은 줄의
버퍼 문자를 덮고 'display로 preedit를 그린다. 덮은 폭이 남으면 공백 패딩.
덮을 문자가 없으면(줄 끝) after-string으로 폴백한다."
    (let ((str (overlay-get ov 'my/ns-preedit-str)))
      (when (and (stringp str) (overlay-buffer ov))
        (with-current-buffer (overlay-buffer ov)
          (let* ((beg (overlay-start ov))
                 (need (string-width str))
                 (eol (save-excursion (goto-char beg) (line-end-position)))
                 (end beg) (got 0))
            (while (and (< end eol) (< got need))
              (setq got (+ got (char-width (char-after end)))
                    end (1+ end)))
            (cond
             ((= end beg)            ; 줄 끝: 덮을 셀 없음 → 기존 방식 폴백
              (overlay-put ov 'display nil)
              (overlay-put ov 'after-string str))
             (t
              (overlay-put ov 'after-string nil)
              (move-overlay ov beg end)
              (overlay-put ov 'display
                           (if (> got need) ; 마지막 덮은 글자가 wide면 패딩
                               (concat str (propertize
                                            (make-string (- got need) ?\s)
                                            'face 'ns-working-text-face))
                             str)))))))))

  (defun my/ns-preedit--maybe-cover (&rest _)
    "조합 시작/갱신 직후(ns-insert-working-text 후): 터미널 버퍼면 전환."
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay)
                 (my/ns-preedit--coverable-p))
        (let ((str (overlay-get ns-working-overlay 'after-string)))
          (when (stringp str)
            (overlay-put ns-working-overlay 'my/ns-preedit-str str)))
        (my/ns-preedit--cover ns-working-overlay))))
  (with-eval-after-load 'term/ns-win
    (advice-add 'ns-insert-working-text :after #'my/ns-preedit--maybe-cover))

  ;; reanchor(터미널 redraw 후 재고정)가 zero-width로 만든 직후 다시 덮는다.
  ;; my/ns-preedit--reanchor 자체에 :after — vterm/ghostel/리사이즈 경로
  ;; 전부 자동으로 따라가고, 기존 코드는 한 줄도 수정하지 않는다.
  ;; (advice-add는 대상이 아직 미정의여도 합법 — 정의 시점에 활성화)
  (defun my/ns-preedit--recover-after-reanchor (buffer &rest _)
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay)
                 (buffer-live-p buffer)
                 (eq (overlay-buffer ns-working-overlay) buffer))
        (with-current-buffer buffer
          (when (my/ns-preedit--coverable-p)
            (my/ns-preedit--cover ns-working-overlay))))))
  (advice-add 'my/ns-preedit--reanchor :after
              #'my/ns-preedit--recover-after-reanchor))

(provide 'ns-preedit-terminal)
;;; ns-preedit-terminal.el ends here
