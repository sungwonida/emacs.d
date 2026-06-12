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
;;   ghostel--pixel-anchor-supported-p.  소실 시 lwarn으로 알림.
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

(provide 'ghostel-fixes)
;;; ghostel-fixes.el ends here
