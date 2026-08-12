;;; ghostel-fixes.el --- ghostel 업스트림 버그 워크어라운드 모음 -*- lexical-binding: t; -*-

;;; Commentary:
;; Purpose
;; -------
;; ghostel의 알려진 업스트림 버그에 대한 init 레벨 워크어라운드.  각 항목은
;; 업스트림 수정이 릴리스되면 제거 가능 — 아래 추적 목록 참고.
;;
;;   1. (3차) 끝공백 커서 클램프 보정: Ink TUI(Claude Code 등)는 끝 공백을
;;      그리지 않고 하드웨어 커서만 옮기는데, ghostel <=0.26.0은 trim된 빈 셀
;;      위의 커서를 줄 끝으로 클램프해 스페이스가 "숨고" 커서가 멈췄다.
;;      → redraw 후 raw 커서 칸(ghostel--cursor-pos)과의 차이만큼 실제 공백을
;;      패딩.  upstream PR #346(v0.34.0)으로 근본 수정됨 — 현재는 deficit이
;;      항상 0인 no-op 보험.
;;   2. (4차) 폴백 글리프 스케일 클램프: 0.27 d28f711이 글리프 스케일 상한을
;;      제거해 폴백 글리프(⎿ U+23BF, ⏺ U+23FA 등)가 셀 높이를 넘고, 0.34
;;      2a39536의 픽셀 정밀 bottom-anchor가 그 ±1px 펄럭임을 매 프레임
;;      vscroll로 흡수해 뷰포트 전체가 출렁인다(스피너 동작 중 "꿈틀").
;;      → anchor 직전에 글리프 줄을 줄 전체 실측으로 균일 높이로 클램프.
;;      upstream 추적: https://github.com/dakra/ghostel/issues/399
;;   3. (4차 보강) anchored 판정 엄격화: +1줄 슬랙 때문에 활성 출력 중 한 줄
;;      위로 스크롤하면 바닥으로 스냅백.  → 엄격 위치 판정으로 교체.
;;      upstream 추적: https://github.com/dakra/ghostel/issues/400
;;   4. (보험) break-glass: (4차)가 패키지 업데이트로 무력화될 때만 주석
;;      해제하는 픽셀 anchor 비활성 스위치.
;;   5. (5차) 잔류 터미널 모드 차단·청소: tmux 등 TUI가 비정상 종료해
;;      DECRST를 못 보내면 모드가 남는다.  두 가지 증상이 같은 뿌리다.
;;      (5a) 잔류 *마우스* 모드 → 셸 프롬프트에서 휠을 굴리면 SGR 리포트가
;;           pty로 새어 "65;50;44M…"이 찍히고 스크롤이 죽는다.
;;      (5b) 잔류 *alt-screen* → alt-screen은 VT 설계상 스크롤백이 없어서,
;;           그 뒤에 띄운 프로그램(claude 등, 자기는 alt-screen을 전혀 안 씀)의
;;           출력이 전부 '현재 한 페이지'에만 그려지고 스크롤백에 안 쌓인다.
;;           결과: ghostel copy-mode에서 방향키·휠이 한 화면 밖으로 못 나감.
;;      → pty 포그라운드가 셸 자신이면(=실행 중인 앱 없음) 마우스 포워딩을
;;      막고, 유휴 시점에 이벤트 모드와 잔류 alt-screen을 DECRST.
;;      이미 물린 버퍼는 C-u M-x my/ghostel-reset-mouse-modes 로 즉시 복구.
;;      업스트림 버그는 아니며(0.35~0.49에 관련 이슈·수정 없음) 실제 터미널도
;;      같은 상황에선 같은 증상 — 표준 처방이 `reset'인 영역의 UX 개선.
;;
;; Requirements
;; ------------
;; - ghostel (straight).  모든 블록이 with-eval-after-load 가드라 ghostel이
;;   없으면 무해.  플랫폼 무관(클램프는 graphical frame에서만 의미).
;;
;; Limitations / 업그레이드 체크리스트
;; -----------------------------------
;; - 내부 심볼 의존: ghostel--redraw-now(구 ghostel--delayed-redraw),
;;   ghostel--cursor-pos, ghostel--cursor-char-pos, ghostel--buffer-editable-p,
;;   ghostel--anchor-window, ghostel--window-anchored-p, ghostel--input-mode,
;;   ghostel--pixel-anchor-supported-p, ghostel--forward-scroll-event,
;;   ghostel--mouse-tracking-active-p(>=0.42.0 제거됨 — 없으면 조용히 생략),
;;   ghostel--write-vt(구 ghostel--write-input), ghostel--mode-enabled.
;;   소실 시 lwarn으로 알림.
;; - (5차)는 macOS에서 ghostel이 셸을 /usr/bin/login 래퍼로 띄우는 것에 의존해
;;   fg pgrp 기준선을 잡는다.  기동 방식이 바뀌어도 기준선이 자기교정되므로
;;   무해하지만, 마우스 포워딩이 이상하면 M-x my/ghostel-reset-mouse-modes 로
;;   즉시 복구하고 my/ghostel-mouse-forward-require-command 를 nil로 두면 된다.
;; - 렌더러 display prop 모양 ((min-width (N)) (height S))이 바뀌면 (4차)는
;;   조용히 no-op → 출렁임 재발 시 break-glass 활성화 후 재진단.
;; - 폰트/배율 변경 후엔 M-x my/ghostel-clamp-reset-cache.
;;
;; History: 2026-06 ghostel 0.26→0.34 업그레이드 전후 이슈를 배치/GUI A/B
;; 실측으로 진단·검증해 작성.  상세 검증 수치는 아래 각 블록 주석 참고.

;;; Code:

;;; --- (3차) ghostel: Ink TUI(Claude Code 등)에서 끝 공백이 "숨고" 커서가 멈추는 문제 보정 ---
;;; 원인: Ink는 입력줄을 끝 공백을 잘라낸(trim) 채로 그리고, 캐럿은 하드웨어
;;; 커서 이동(CUF/CUB)으로만 알린다. ghostel(<=0.26.0)은 trim된 빈 셀 위의
;;; 커서를 줄 끝(eol)으로 클램프한다(Renderer.zig positionCursorByCell의 @min).
;;; 해법: redraw 직후, 터미널 raw 커서 칸(ghostel--cursor-pos의 COL)이
;;; 버퍼 줄 폭보다 크면 그 차이만큼 실제 공백을 eol에 채워서 point와
;;; ghostel--cursor-char-pos가 진짜 캐럿 칸에 앉게 한다.
;;; (vterm은 C의 goto_col이 같은 방식으로 이미 패딩하므로 이 버그가 없음.
;;;  ghostel을 PR #346 포함 버전(>=0.34.0)으로 올리면 deficit이 항상 0이라
;;;  이 advice는 자동으로 no-op — 보험으로 남겨 둠.)
(with-eval-after-load 'ghostel
  (defun my/ghostel--pad-hidden-tail (buffer)
    "redraw 후 BUFFER에서 trim된 끝 빈 칸을 실제 공백으로 채워 커서를 복원."
    ;; process-filter 꼬리에서 실행 — 어떤 에러도 출력 처리를 끊으면 안 됨.
    (ignore-errors
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'ghostel-mode)
                     (fboundp 'ghostel--buffer-editable-p)
                     (ghostel--buffer-editable-p) ; copy/emacs/line 모드 제외
                     (consp ghostel--cursor-pos)
                     (integerp ghostel--cursor-char-pos))
            (let* ((cx (car ghostel--cursor-pos)) ; 터미널 raw 커서 칸(0-based)
                   (cpos ghostel--cursor-char-pos)
                   (deficit
                    (save-excursion
                      (goto-char cpos)
                      ;; 클램프는 eol에서만 발생. 줄 중간이면 손대지 않는다.
                      (if (eolp)
                          (- cx (string-width
                                 (buffer-substring-no-properties
                                  (line-beginning-position) (point))))
                        0))))
              (when (> deficit 0)
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      (at-cursor (= (point) cpos))
                      (wins (seq-filter
                             (lambda (w) (= (window-point w) cpos))
                             (get-buffer-window-list buffer nil t))))
                  (save-excursion
                    (goto-char cpos)
                    (insert (make-string deficit ?\s))
                    (setq ghostel--cursor-char-pos (point)))
                  ;; redraw가 point를 커서에 두었던 경우에만 따라가 준다
                  ;; (preedit anchor/emacs-mode 등 예외 위치는 건드리지 않음).
                  (when at-cursor (goto-char ghostel--cursor-char-pos))
                  (dolist (w wins)
                    (set-window-point w ghostel--cursor-char-pos))))))))))
  ;; depth -100: 같은 함수의 :after 중 가장 먼저 실행 →
  ;; 뒤따르는 my/ns-preedit--ghostel-reanchor가 보정된
  ;; ghostel-cursor-point(=ghostel--cursor-char-pos)를 보게 한다.
  (advice-add (if (fboundp 'ghostel--redraw-now)
                  'ghostel--redraw-now
                'ghostel--delayed-redraw)
              :after #'my/ghostel--pad-hidden-tail
              '((depth . -100))))

;;; --- (4차) ghostel 0.34: 폴백 글리프 스케일 클램프 — 스피너 viewport 출렁임 제거 ---
;;; 증상: Claude Code 스피너(✻ Running…) 동작 중 뷰포트 전체가 프레임마다
;;; 1~2px 아래위로 출렁임(0.26에는 없던 회귀). 원인은 두 변경의 합작(실측):
;;;  (a) d28f711(0.27): Renderer.zig adjustGlyph의 claim-loop 개편으로 글리프
;;;      스케일 '상한'이 사라짐 — scale = max(min(scale_w, scale_h), floor)에
;;;      ceiling이 없어 폴백 폰트 글리프가 셀보다 커진다. ⎿ U+23BF(BIZ
;;;      UDGothic 폴백)는 display ((min-width (3)) (height 1.0769…)) →
;;;      14px 그리드(D2Coding-12)에서 15px 줄, ⏺ U+23FA(STIX Two Math)도 동일.
;;;      스피너 글자 자체(· ✢ ✳ ✶ ✻ ✽ *)는 전부 D2Coding 커버 → 항상 14px.
;;;  (b) 2a39536(0.34): 매 redraw 픽셀 정밀 bottom-anchor — "⎿ Running…" 줄이
;;;      프레임마다 그려졌다/지워졌다 하며 줄높이가 ±1px 펄럭이면 anchor가
;;;      그 차이를 vscroll로 흡수해 뷰포트 전체가 같이 움직인다.
;;; 해법: 글리프가 있는 줄의 display height 스케일을 "줄 전체 실측"으로 기본
;;; 줄높이 안에 들어올 때까지 줄여 모든 줄을 균일(14px)하게 만든다.
;;; → 픽셀 anchor는 켠 채 유지(픽셀 정밀 바닥 정렬). anchor를 끄는 우회의
;;; 부작용(바닥 틈 최대 13px, 한 줄 위 스크롤 스냅백)이 아예 생기지 않는다.
;;;
;;; 측정 원리(이전 실패작의 교훈):
;;;  * 글리프 런만 재면 안 된다 — col-0의 ⏺ 같은 배치에서 런 단독 측정은
;;;    13px("맞음")인데 줄 전체는 15px로 실현된다(베이스라인 정렬에 의한 줄
;;;    성장은 런 범위 측정에 안 잡힘). 반드시 줄 전체 bol..min(point-max,1+eol).
;;;  * "맞음" 판정은 절대 캐시하지 않는다(문맥 의존 → 캐시 오염). 캐시에는
;;;    (글리프 . 원본스케일) → 지금까지의 '최소 안전 스케일'만 단조 감소로
;;;    기록하고, 글리프 줄은 매 패스 재실측한다(글리프 줄은 소수 → ~수십 µs).
;;;  * window-text-pixel-size는 창의 현재 vscroll만큼 측정 높이를 깎아
;;;    돌려준다(실측: vscroll=3이면 15px 줄이 12px로 측정). 이 advice는
;;;    anchor 직전에 돌아 직전 redraw의 vscroll이 남아 있으므로 측정 전에
;;;    0으로 리셋한다(anchor가 어차피 바로 다시 계산).
;;; 검증(전부 별도 -Q GUI 프로세스 + 실제 0.34 모듈, 합성 스피너 드라이버):
;;; stock은 vscroll 펄럭임 재현 → 클램프 후 완전 고정(504샘플 비트 동일),
;;; 전 줄 14px. 레이아웃 매트릭스(col-0/단독/들여쓰기/한 줄 2글리프/한글
;;; 인접) 전부 통과, 캐시 오염 없음. 위 preedit·Ink advice와 동시 적용 검증.
;;; 비용 평균 0.33ms/redraw(글리프 빽빽한 스크롤백은 2~3ms — 33ms 주기 대비
;;; 체감 없음).
;;; 주의: 내부 심볼 의존 — ghostel--anchor-window(advice 대상; 없으면 lwarn),
;;; 렌더러 display prop 모양 ((min-width (N)) (height S))(바뀌면 조용히 no-op
;;; → 출렁임 재발, 아래 break-glass 활성화). 폰트/배율 변경 후엔
;;; M-x my/ghostel-clamp-reset-cache.

(defvar my/ghostel--glyph-safe-scale (make-hash-table :test 'equal)
  "(글리프문자열 . 렌더러원본스케일) → 도출된 가장 작은 안전 스케일(수).
\"이 스케일로 맞음\"(t) 판정은 절대 넣지 않는다 — 캐시 오염 교훈.")

(defun my/ghostel-clamp-reset-cache ()
  "글리프 스케일 캐시 초기화 (폰트/배율 변경 후 호출)."
  (interactive)
  (clrhash my/ghostel--glyph-safe-scale))

(defsubst my/ghostel--glyph-run-scale (disp)
  "DISP가 ghostel 렌더러의 글리프 보정 spec이면 그 height 스케일을 반환.
렌더러는 정확히 ((min-width (N)) (height S)) 꼴만 쓴다(Renderer.zig
adjustGlyph). (space :width 0)(claim된 칸)이나 다른 display는 건드리지 않는다."
  (and (consp disp)
       (consp (car-safe disp))          ; spec 리스트 꼴만
       (assq 'min-width disp)
       (let ((spec (assq 'height disp)))
         (and (consp spec) (numberp (cadr spec)) (cadr spec)))))

(defsubst my/ghostel--set-run-scale (start end disp new-scale)
  "START..END 런의 display DISP에서 height만 NEW-SCALE로 바꾼 사본을 적용."
  (let ((new (copy-tree disp)))
    (setcar (cdr (assq 'height new)) new-scale)
    (put-text-property start end 'display new)
    new))

(defun my/ghostel--clamp-glyph-scales (&optional window)
  "WINDOW 뷰포트의 ghostel 글리프 display 스케일을 줄 단위 실측으로 클램프.
글리프 런이 있는 각 줄의 전체 픽셀 높이가 기본 줄높이를 넘으면 그 줄의
모든 런을 0.85배씩 '합동' 축소(한 줄에 tall 글리프가 여럿이어도 서로
줄높이를 붙잡고 도는 일이 없음), 들어올 때까지 재측정한다(상한 8회)."
  ;; redraw 경로(:before ghostel--anchor-window)에서 실행 —
  ;; 어떤 에러도 출력 처리/anchoring을 끊으면 안 됨.
  (ignore-errors
    (when-let* ((win (or window (selected-window)))
                ((window-live-p win))
                (buf (window-buffer win)))
      (with-current-buffer buf
        (when (derived-mode-p 'ghostel-mode)
          (with-silent-modifications
            ;; vscroll 잔존분이 측정을 오염시키므로 먼저 0으로(위 주석 참조).
            (when (> (window-vscroll win t) 0)
              (set-window-vscroll win 0 t))
            (let* ((h (window-default-line-height win))
                   (limit (point-max))
                   ;; 렌더러가 props를 다시 쓰는 곳은 끝부분 뷰포트뿐.
                   ;; 스크롤백으로 밀린 줄은 뷰포트에 있을 때 이미 클램프됨.
                   (start (save-excursion (goto-char limit)
                                          (forward-line -250) (point))))
              (save-excursion
                (goto-char start)
                (while (< (point) limit)
                  (let* ((bol (point))
                         (eol (line-end-position))
                         (runs nil))    ; ((start end 원본키) ...)
                    ;; 1) 이 줄의 글리프 런 수집 + 캐시된 안전 스케일 선적용
                    (let ((p bol))
                      (while (< p eol)
                        (let* ((disp (get-text-property p 'display))
                               (end (min eol (or (next-single-property-change
                                                  p 'display nil eol)
                                                 eol)))
                               (scale (my/ghostel--glyph-run-scale disp)))
                          (when scale
                            (let* ((key (cons (buffer-substring-no-properties p end)
                                              scale))
                                   (hit (gethash key my/ghostel--glyph-safe-scale)))
                              (when (and (numberp hit) (< hit scale))
                                (my/ghostel--set-run-scale p end disp hit))
                              (push (list p end key) runs)))
                          (setq p end))))
                    ;; 2) 런이 있는 줄만: 줄 전체 실측 → 초과 시 합동 축소
                    (when runs
                      (let ((iter 0))
                        (while (and (< iter 8)
                                    (> (cdr (window-text-pixel-size
                                             win bol (min limit (1+ eol))))
                                       h))
                          (dolist (r runs)
                            (let ((disp (get-text-property (nth 0 r) 'display)))
                              (when-let* ((cur (my/ghostel--glyph-run-scale disp)))
                                (my/ghostel--set-run-scale
                                 (nth 0 r) (nth 1 r) disp (* cur 0.85)))))
                          (setq iter (1+ iter)))
                        ;; 3) 축소가 일어난 경우에만 (글리프 . 원본스케일) →
                        ;;    최소 안전 스케일을 기록. 단조 감소만 허용.
                        (when (> iter 0)
                          (dolist (r runs)
                            (let* ((disp (get-text-property (nth 0 r) 'display))
                                   (cur (my/ghostel--glyph-run-scale disp))
                                   (key (nth 2 r))
                                   (old (gethash key my/ghostel--glyph-safe-scale)))
                              (when (and (numberp cur) (< cur (cdr key))
                                         (or (not (numberp old)) (< cur old)))
                                (puthash key cur my/ghostel--glyph-safe-scale)))))))
                    (goto-char (min limit (1+ eol)))))))))))))

(with-eval-after-load 'ghostel
  (if (fboundp 'ghostel--anchor-window)
      (advice-add 'ghostel--anchor-window :before #'my/ghostel--clamp-glyph-scales)
    (lwarn 'my/ghostel :warning
           "ghostel--anchor-window 없음(패키지 업데이트?) — 글리프 클램프 미설치, 스피너 출렁임 재발 가능")))

;;; --- (4차 보강) ghostel: anchored 판정의 +1 슬랙 스냅백 코너 제거 ---
(with-eval-after-load 'ghostel
  (if (and (fboundp 'ghostel--window-anchored-p)
           (boundp 'ghostel--input-mode)
           (boundp 'ghostel--pixel-anchor-supported-p))
      (progn
        (defun my/ghostel--window-anchored-p-strict (_orig window &optional body-pixel-height)
          "`ghostel--window-anchored-p'의 엄격판 — 1줄 슬랙 없음."
          (with-current-buffer (window-buffer window)
            (when (and (derived-mode-p 'ghostel-mode)
                       (not (eq ghostel--input-mode 'emacs)))
              (let* ((dlh (max 1.0 (float (default-line-height))))
                     (lines (if body-pixel-height
                                (/ (float body-pixel-height) dlh)
                              (with-selected-window window (window-screen-lines))))
                     (n (if (and ghostel--pixel-anchor-supported-p
                                 (display-graphic-p (window-frame window)))
                            (ceiling lines)   ; 픽셀 anchor: ≤ ceil 줄 + vscroll
                          (floor lines))))    ; vscroll-0 폴백: 정확히 floor 줄
                (save-excursion
                  (goto-char (point-max))
                  (forward-line (- n))
                  (>= (window-start window) (point)))))))
        (advice-add 'ghostel--window-anchored-p :around
                    #'my/ghostel--window-anchored-p-strict))
    (lwarn 'my/ghostel :warning
           "ghostel--window-anchored-p/내부 변수 없음(패키지 업데이트?) — 엄격 anchored 판정 미설치(한 줄 위 스크롤 스냅백 가능)")))

;;; --- (보험) ghostel break-glass: 픽셀 bottom-anchor 끄기 (평소엔 주석 유지) ---
;;; 위 (4차) 클램프가 무력화됐을 때만(패키지 업데이트로 내부 심볼/prop 모양
;;; 변경 → lwarn 경고 또는 출렁임 재발) 주석 해제하는 비상 스위치.
;;; 효과: ghostel--anchor-window가 0.26식 라인 단위 anchoring으로 복귀
;;; (window-start 줄 경계 + vscroll 0 고정) → 줄높이 펄럭임이 vscroll로
;;; 번지지 않아 뷰포트 출렁임 정지.
;;; 대가(이 모드의 내재 한계): 바닥 정렬이 줄 단위가 되어 내용이 창 바닥에서
;;; 최대 (1- 기본줄높이) = 13px 떠 보일 수 있음. 스냅백은 위 (4차 보강) 엄격
;;; 판정이 함께 설치돼 있으면 이 모드에서도 발생하지 않는다(e2e 실측) —
;;; 반드시 함께 둘 것. 또한 upstream #384(아이콘/CJK 대량 출력 시 뷰포트
;;; 오배치)가 픽셀 anchor의 도입 동기였으므로 클램프 없이 장기 사용은 비추천.
;;; ghostel--pixel-anchor-supported-p는 defconst지만 with-eval-after-load가
;;; 패키지 재로드 때마다 다시 실행되므로 setq override가 유지된다.
;;; 주의: 실행 중 세션에서 eval로 활성화하면 창 본문높이가 줄높이 정배수가
;;; 아닌 창은 그 순간 바닥 추적이 잠시 멈춤 — 터미널에 아무 키나 입력하면
;;; 즉시 재고정. init.el 주석 해제 후 재시작 경로에는 해당 없음.
;; (with-eval-after-load 'ghostel
;;   (if (boundp 'ghostel--pixel-anchor-supported-p)
;;       (setq ghostel--pixel-anchor-supported-p nil)
;;     (lwarn 'my/ghostel :warning
;;            "ghostel--pixel-anchor-supported-p 없음 — break-glass 미적용")))


;;; --- (5차) ghostel: TUI가 남긴 마우스 트래킹 모드로 셸에서 휠이 새는 문제 ---
;;; 증상: tmux(`set -g mouse on')를 쓰고 나온 뒤 *tmux 밖* 일반 셸에서 휠을
;;; 굴리면 "65;50;44M65;50;44M…" 같은 문자가 프롬프트에 찍히고 화면은 전혀
;;; 스크롤되지 않는다.  찍히는 건 ghostel이 pty로 보낸 SGR 마우스 리포트
;;; ESC[<64;COL;ROW M 에서 readline이 ESC[< 를 먹고 남은 리터럴이다.
;;;
;;; 원인(batch 실측으로 확정): ghostel의 휠 포워딩 게이트 자체는 정상이다 —
;;; ghostel--forward-scroll-event → ghostel--mouse-event → (Zig)
;;; mouse_encode.shouldReport 는 DEC 이벤트 모드가 .none이면 0바이트를 내고
;;; nil을 반환, 상위 인터셉트가 ghostel--redispatch-scroll-event 로 로컬
;;; 스크롤에 폴백한다.  실측: 모드 전부 off → forward-ret=nil, sent=<NOTHING>;
;;; 1002+1006 on → forward-ret=t, sent="ESC[<64;21;11M".  즉 바이트가 나갔다는
;;; 것은 1000/1002/1003 중 하나가 *실제로* 켜져 있다는 뜻이고, 형식이
;;; SGR(1006)인 것은 그 모드를 켠 주체가 tmux임을 가리킨다.  tmux는 종료·디태치
;;; 때 DECRST를 보내지만 SSH가 끊기거나 비정상 종료하면 그 리셋이 ghostel에
;;; 도달하지 못한다 → "잔류 모드"로 남아 셸에서까지 휠이 pty로 새어 나간다.
;;; (일반 터미널도 같은 상황에선 같은 증상이며 보통 `reset'으로 푼다.)
;;;
;;; 판별자 선택 — alt-screen이 아니라 "pty 포그라운드 프로세스 그룹":
;;; 처음엔 alt-screen 여부로 갈랐으나 실측 리뷰에서 세 군데가 깨졌다.
;;;  * 비정상 종료는 ESC[?1002l 과 함께 ESC[?1049l 도 잃는다 → alt-screen까지
;;;    켜진 채 남아(프롬프트가 alt 화면 위에 그려짐) alt 판별은 통과해 버리고,
;;;    t→nil 전환이 없으니 청소도 영영 안 돈다(즉 목표 시나리오에서 무력).
;;;  * `fzf --height'·Bubble Tea·Ink 같은 인라인 TUI는 primary screen에서
;;;    마우스를 쓴다 → alt 게이트는 이들을 부당하게 막는다.
;;;  * tmux `set -ga terminal-overrides ",*:smcup@:rmcup@"' 나 GNU screen은
;;;    아예 primary screen에서 돈다 → 고치려던 워크플로가 오히려 깨진다.
;;; 반면 `process-running-child-p'(tcgetpgrp 1회)는 "지금 pty 포그라운드가
;;; 셸 자신인가"를 직접 답한다.  셸이 포그라운드면 화면 위에 마우스를 원할
;;; 프로그램이 아예 없으므로 그 마우스 모드는 정의상 잔류물이다.  OS가 알 수
;;; 없으면 t(=자식 있음)를 돌려주므로 그런 플랫폼에선 기존 동작이 유지된다.
;;;
;;; 해법 2단:
;;;  (a) 게이트 — 셸이 포그라운드일 때(=프롬프트)만 포워딩을 막고 ghostel
;;;      자체 스크롤백을 스크롤한다.  실행 중인 TUI(tmux/vim/less/htop,
;;;      인라인 TUI 포함)는 전부 포그라운드 자식이므로 영향이 없다.
;;;  (b) 청소 — 같은 조건에서 잔류 마우스 *이벤트* 모드에 DECRST를 먹여
;;;      실제로 꺼 준다(ghostel--write-vt/--write-input 은 pty가 아니라 터미널
;;;      파서에만 쓰므로 셸 입력줄이 오염되지 않는다).  redraw 꼬리에서 바로
;;;      쓰면 파서가 이스케이프 시퀀스 중간일 때 리터럴이 찍히므로(실측)
;;;      유휴 타이머로 미뤄 조건을 재확인한 뒤 쓴다.
;;; 이미 막힌 버퍼는 M-x my/ghostel-reset-mouse-modes 로 즉시 복구
;;; (C-u 를 주면 alt-screen에 갇힌 경우 rmcup까지 함께 보낸다).
;;;
;;; 인코딩 모드(1005/1006/1015/1016)는 일부러 건드리지 않는다: 살아남은 앱이
;;; 이벤트 모드만 다시 켜면 조용히 X10으로 강등돼 223열 넘는 좌표가 깨진다.
;;;
;;; 주의: 내부 심볼 의존 — ghostel--forward-scroll-event,
;;; ghostel--mouse-tracking-active-p(>=0.42.0에서 제거됨 — 없으면 조용히 생략),
;;; ghostel--write-vt(구 ghostel--write-input), ghostel--mode-enabled.
;;; 소실 시 lwarn으로 알림.

(declare-function ghostel--write-vt "ghostel-module")
(declare-function ghostel--write-input "ghostel-module")
(declare-function ghostel--mode-enabled "ghostel-module")
(defvar ghostel--term)
(defvar ghostel--process)

(defcustom my/ghostel-mouse-forward-require-command t
  "非nil이면 pty 포그라운드에 실제 프로그램이 있을 때만 마우스를 포워딩한다.
셸 프롬프트에서는 휠이 항상 ghostel 자체 스크롤백을 스크롤한다.
nil로 두면 (a) 게이트가 꺼지고 아래 (b) 자동 청소에만 의존한다."
  :type 'boolean
  :group 'ghostel)

(defconst my/ghostel--mouse-mode-reset "\e[?9l\e[?1000l\e[?1002l\e[?1003l"
  "DEC 마우스 *이벤트* 모드(9/1000/1002/1003)만 끄는 시퀀스.
인코딩 모드(1005/1006/1015/1016)는 일부러 남긴다 — 살아남은 앱이 이벤트
모드만 다시 켜면 X10으로 강등돼 223열 넘는 좌표가 깨지기 때문.
1001(highlight tracking)은 libghostty 미구현이라 넣지 않는다(경고 발생).")

(defconst my/ghostel--alt-screen-exit "\e[?1049l\e[?1047l\e[?47l"
  "alt-screen에서 primary로 되돌리는 시퀀스(1049/1047/47 전부).
정상 동작 중인 TUI에 먹이면 화면이 어긋나므로, 대화형 명령에서는
prefix-arg로만 보내고 자동 청소는 '셸이 포그라운드'일 때만 보낸다.")

(defun my/ghostel--vt-writer ()
  "터미널 파서에 raw 바이트를 먹이는 모듈 함수. 없으면 nil.
0.34 이후 업스트림에서 `ghostel--write-input' → `ghostel--write-vt' 로
이름이 바뀌었다.  `ghostel--write-pty' 는 셸 stdin으로 나가므로 절대 쓰지 않는다."
  (cond ((fboundp 'ghostel--write-vt) #'ghostel--write-vt)
        ((fboundp 'ghostel--write-input) #'ghostel--write-input)))

(defvar-local my/ghostel--shell-pgid nil
  "이 버퍼 pty의 셸 프로세스 그룹 ID 추정치 = 지금까지 관측한 최소 fg pgrp.
셸은 이 버퍼에서 가장 먼저 생기고 pid는 증가하므로, 셸이 띄우는 어떤
명령의 pgid보다도 작다.  따라서 최소값이 곧 셸이며, 더 작은 값을 보면
자동으로 내려가 스스로 교정된다(첫 관측이 명령 실행 중이었어도 프롬프트로
돌아오는 순간 바로잡힌다).")

(defun my/ghostel--fg-pgrp ()
  "pty 포그라운드 프로세스 그룹.
nil = Emacs가 띄운 프로세스 자신, 정수 = 그 pgid, t = OS가 알 수 없음."
  (ignore-errors
    (and (bound-and-true-p ghostel--process)
         (process-live-p ghostel--process)
         (process-running-child-p ghostel--process))))

(defun my/ghostel--shell-at-prompt-p ()
  "pty 포그라운드가 셸 자신이면 非nil(= 실행 중인 앱이 없다).
macOS에서 ghostel은 셸을 `/usr/bin/login' 래퍼로 띄우므로 Emacs가 아는
프로세스(login)와 실제 셸의 pid가 다르다 → `process-running-child-p' 는
프롬프트에서도 셸의 pgid를 돌려준다(실측).  그래서 nil 하나로 판정하지
않고 `my/ghostel--shell-pgid' 기준선과 비교한다.  래퍼가 없는 구성에서는
프롬프트에서 nil이 나오므로 그 경우도 함께 참으로 취급한다.
OS가 알 수 없으면(t) nil을 돌려 기존 동작(포워딩)을 유지한다."
  (let ((fg (my/ghostel--fg-pgrp)))
    (cond
     ((null fg) t)                      ; fg == Emacs가 띄운 프로세스 자신
     ((not (integerp fg)) nil)          ; t = OS가 모름 → 기존 동작 유지
     (t
      ;; 기준선 갱신(단조 감소) — 예측 자체가 기준선을 만든다.
      (when (or (null my/ghostel--shell-pgid) (< fg my/ghostel--shell-pgid))
        (setq my/ghostel--shell-pgid fg))
      (eql fg my/ghostel--shell-pgid)))))

(defun my/ghostel--mouse-modes-stale-p ()
  "마우스 이벤트 모드(9/1000/1002/1003)가 하나라도 켜져 있으면 非nil."
  (and (bound-and-true-p ghostel--term)
       (fboundp 'ghostel--mode-enabled)
       (or (ghostel--mode-enabled ghostel--term 1000)
           (ghostel--mode-enabled ghostel--term 1002)
           (ghostel--mode-enabled ghostel--term 1003)
           (ghostel--mode-enabled ghostel--term 9))))

(defun my/ghostel--mouse-forwardable-p ()
  "지금 이 버퍼에서 마우스 이벤트를 터미널로 넘겨도 되는지."
  (or (not my/ghostel-mouse-forward-require-command)
      (not (my/ghostel--shell-at-prompt-p))))

(defcustom my/ghostel-reset-alt-screen-when-idle t
  "非nil이면 잔류 alt-screen도 셸 프롬프트에서 자동으로 해제한다.
마우스 모드와 완전히 같은 이유·같은 게이트다: 비정상 종료한 TUI는
`ESC[?1002l' 과 함께 `ESC[?1049l' 도 잃어버리기 때문에 alt-screen이 켜진
채 남는다.  alt-screen 은 VT 설계상 스크롤백이 없으므로, 그 뒤에 실행하는
프로그램의 출력이 전부 '현재 한 페이지'에만 그려지고 스크롤백에 쌓이지
않는다 → ghostel copy-mode에서 방향키·휠이 한 화면 밖으로 못 나간다.
셸이 포그라운드면(=실행 중인 앱 없음) 그 alt-screen 은 정의상 잔류물이라
꺼도 안전하다.  끄는 순간 화면이 primary 로 돌아가 직전 프롬프트 그림이
사라지는 1회성 이질감이 있으나, 다음 프롬프트 출력에서 자연히 복구된다."
  :type 'boolean
  :group 'ghostel)

(defun my/ghostel--alt-screen-stale-p ()
  "alt-screen(1049/1047/47)이 켜져 있으면 非nil."
  (and (bound-and-true-p ghostel--term)
       (fboundp 'ghostel--mode-enabled)
       (or (ghostel--mode-enabled ghostel--term 1049)
           (ghostel--mode-enabled ghostel--term 1047)
           (ghostel--mode-enabled ghostel--term 47))))

(defun my/ghostel--residual-reset-string ()
  "이 버퍼에서 지금 꺼야 할 잔류 모드의 DECRST 시퀀스.  없으면 nil.
호출자가 '셸이 포그라운드'임을 이미 확인했다고 가정한다."
  (let ((s (concat (and (my/ghostel--mouse-modes-stale-p)
                        my/ghostel--mouse-mode-reset)
                   (and my/ghostel-reset-alt-screen-when-idle
                        (my/ghostel--alt-screen-stale-p)
                        my/ghostel--alt-screen-exit))))
    (and (> (length s) 0) s)))

;;;###autoload
(defun my/ghostel-reset-mouse-modes (&optional exit-alt-screen)
  "현재 ghostel 버퍼에 남은 DEC 마우스 트래킹 모드를 강제 해제.
prefix arg(\\[universal-argument])를 주면 alt-screen에서도 빠져나온다(rmcup) —
SSH가 끊겨 tmux의 복원 버스트를 통째로 못 받아 화면이 alt에 물린 경우에 사용.
pty에는 아무것도 보내지 않고 터미널 상태만 되돌린다(입력줄 안전)."
  (interactive "P")
  (let ((writer (my/ghostel--vt-writer)))
    (unless (and (derived-mode-p 'ghostel-mode)
                 (bound-and-true-p ghostel--term) writer)
      (user-error "ghostel 버퍼가 아니거나 터미널 핸들/VT writer가 없음"))
    (funcall writer ghostel--term
             (concat my/ghostel--mouse-mode-reset
                     (and exit-alt-screen my/ghostel--alt-screen-exit)))
    (message "ghostel: 마우스 트래킹 모드 해제%s"
             (if exit-alt-screen " + alt-screen 이탈" ""))))

;;; (a) 게이트: 셸 프롬프트에서는 마우스를 pty로 넘기지 않는다.
;;; 두 advice의 가드를 분리한다 — ghostel--mouse-tracking-active-p 는
;;; >=0.42.0에서 제거됐지만 ghostel--forward-scroll-event 는 남아 있으므로,
;;; 묶어서 검사하면 휠 게이트까지 통째로 미설치돼 버린다.
(with-eval-after-load 'ghostel
  (if (fboundp 'ghostel--forward-scroll-event)
      (progn
        (defun my/ghostel--forward-scroll-gate (orig event button)
          "프롬프트에서는 휠을 포워딩하지 않고 nil을 돌려 로컬 스크롤로 폴백."
          (when (my/ghostel--mouse-forwardable-p)
            (funcall orig event button)))
        (advice-add 'ghostel--forward-scroll-event :around
                    #'my/ghostel--forward-scroll-gate))
    (lwarn 'my/ghostel :warning
           "ghostel--forward-scroll-event 없음(패키지 업데이트?) — 휠 게이트 미설치, 셸에서 휠이 샐 수 있음")))

(with-eval-after-load 'ghostel
  (when (fboundp 'ghostel--mouse-tracking-active-p)
    (defun my/ghostel--mouse-tracking-gate (orig)
      "프롬프트에서는 클릭/드래그도 터미널로 넘기지 않는다."
      (and (my/ghostel--mouse-forwardable-p)
           (funcall orig)))
    (advice-add 'ghostel--mouse-tracking-active-p :around
                #'my/ghostel--mouse-tracking-gate)))

;;; (b) 청소: 프롬프트에 있고 잔류 모드가 있으면 유휴 시점에 DECRST.
(defvar-local my/ghostel--mouse-cleanup-timer nil
  "예약된 잔류 마우스 모드 청소 타이머(중복 예약 방지).")

(defun my/ghostel--mouse-cleanup-needed-p ()
  "이 버퍼에 잔류 모드(마우스/alt-screen)가 있고 지금 꺼도 안전하면 非nil."
  (and (derived-mode-p 'ghostel-mode)
       (bound-and-true-p ghostel--term)
       (my/ghostel--shell-at-prompt-p)
       (my/ghostel--residual-reset-string)))

(defun my/ghostel--flush-mouse-cleanup (buffer)
  "유휴 시점에 조건을 재확인한 뒤에만 DECRST를 파서에 먹인다.
redraw 꼬리에서 바로 쓰면 파서가 이스케이프 시퀀스 중간일 때 리터럴이
화면에 찍힌다(실측) — 그래서 여기서 쓴다.
끌 대상은 유휴 시점에 다시 계산한다(그 사이 모드가 바뀌었을 수 있다)."
  (ignore-errors
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq my/ghostel--mouse-cleanup-timer nil)
        (when-let* (((derived-mode-p 'ghostel-mode))
                    ((bound-and-true-p ghostel--term))
                    ((my/ghostel--shell-at-prompt-p))
                    (reset (my/ghostel--residual-reset-string))
                    (writer (my/ghostel--vt-writer)))
          (funcall writer ghostel--term reset))))))

(defun my/ghostel--schedule-mouse-cleanup (buffer &rest _)
  "redraw 후 잔류 마우스 모드가 보이면 유휴 청소를 1회 예약.
`ghostel--redraw-now' 는 0.39.0부터 선택 인자 FORCE를 받으므로 &rest 로
흡수한다 — :after advice는 apply로 불려서 arity가 어긋나면 본문의
`ignore-errors' *밖에서* wrong-number-of-arguments 가 난다."
  (ignore-errors
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (not my/ghostel--mouse-cleanup-timer)
                   (my/ghostel--mouse-cleanup-needed-p))
          (setq my/ghostel--mouse-cleanup-timer
                (run-with-idle-timer
                 0.2 nil #'my/ghostel--flush-mouse-cleanup buffer)))))))

(with-eval-after-load 'ghostel
  (let ((fn (cond ((fboundp 'ghostel--redraw-now) 'ghostel--redraw-now)
                  ((fboundp 'ghostel--delayed-redraw) 'ghostel--delayed-redraw))))
    (if fn
        (advice-add fn :after #'my/ghostel--schedule-mouse-cleanup)
      (lwarn 'my/ghostel :warning
             "ghostel--redraw-now 없음(패키지 업데이트?) — 잔류 마우스 모드 자동 청소 미설치"))))

(provide 'ghostel-fixes)
;;; ghostel-fixes.el ends here
