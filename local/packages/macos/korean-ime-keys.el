;;; korean-ime-keys.el --- macOS 시스템 한글 IME에서 Emacs 단축키 호환 -*- lexical-binding: t; -*-

;;; Commentary:
;; Purpose
;; -------
;; macOS NS 포트에서 시스템 한글(2벌식, 2-Set Korean) 입력소스를 켠 채로도
;; Emacs 단축키가 동작하게 한다.  두 계층으로 구성:
;;
;;   1. key-translation-map 역번역 (수식이 붙은 키):
;;      시스템 IME가 한글일 때 수식(Control/Meta/Super)이 붙은 키의 base 글자는
;;      활성 입력소스가 호환 자모(U+3131..U+3163)로 번역해 버려 C-ㅁ / M-ㅌ /
;;      C-M-ㅗ 같은 '수식+자모' 이벤트가 되어 전부 undefined가 된다.
;;      (NS 포트는 Control·Meta 둘 다 IME '조합'은 우회하지만, 글자 자체는
;;       활성 입력소스 레이아웃에서 가져온다 — Control도 Meta처럼 깨진다.)
;;      → <수식조합>-<자모> 이벤트를 같은 자리의 QWERTY 글자로 역번역한다.
;;      수식 조합 목록(`my/korean-translated-modifier-combos')을 순회하므로
;;      조합 추가는 리스트 한 줄이면 된다.  수식 없는 맨자모는 매칭되지 않아
;;      한글 입력/조합은 그대로 유지된다.
;;
;;   2. sis (smart-input-source) + macism (프리픽스 뒤 '수식 없는 연속 키'):
;;      C-x o 의 o, M-s h r 의 h r 같은 키는 IME가 한글로 조합해 버리는데,
;;      이는 자모 역번역으론 불가(정상 한글 입력과 구분 불가).  유일한 해법은
;;      OS 입력소스 자동전환: `sis-global-respect-mode'가 프리픽스/minibuffer
;;      진입 시 ABC로 바꿨다가 끝나면 한글로 복원한다(수동 CMD+SPC 제어는 유지).
;;
;; Requirements
;; ------------
;; - macOS NS 포트 (GNU Emacs 30.x, emacs-app cask).  macOS 전용 —
;;   init.el에서 `:if (eq system-type 'darwin)' 으로 가드해 로드할 것.
;;   (Linux/WSL2는 ibus/fcitx가 수식키 이벤트를 통과시켜 이 문제가 없음.)
;; - macism CLI: brew install laishulu/homebrew/macism
;;   (입력소스 전환이 안 되면 System Settings > 개인정보 보호 및 보안 >
;;    손쉬운 사용에 Emacs.app 추가.)
;; - sis 패키지 (straight로 자동 설치).
;;
;; Limitations (실측으로 확정된 사실)
;; ----------------------------------
;; - sis는 '열거된 프리픽스'만 커버한다.  NS 포트엔 임의 프리픽스 대기를
;;   감지하는 훅이 없다.  새 프리픽스가 한글 모드에서 안 먹으면
;;   `sis-prefix-override-keys'에 그 키만 추가하면 된다.
;;   ※ key-translation-map이 C-ㅌ->C-x, M-ㄴ->M-s 로 먼저 바꾸므로(translation은
;;     emulation-map 조회보다 앞), 목록은 'latin 형태'(C-x, M-s)로 적어야 한다.
;; - Shift 콤보(C-S-k 등)는 한글 모드에서 구조적으로 불가: NS 포트가 caseless
;;   자모에서 shift 비트를 소실시켜(nsterm.m ns_get_shifted_character가 shift를
;;   글자에 흡수 + keyboard.c가 멀티바이트 이벤트의 shift 비트 폐기) 한글모드
;;   Ctrl+Shift+k == Ctrl+k (둘 다 C-ㅏ)로 도착한다.  elisp 복구 불가.
;;   shift 콤보 엔트리는 발화하지 않는 무해한 보험용이다.  한글모드에서 C-S-
;;   단축키가 필요하면 비-shift 콤보(s-k 등)를 병행 바인딩할 것.
;; - super(Option)는 shift와 달리 수식 비트가 보존되므로 정상 동작한다
;;   (한글모드 Option+k → s-ㅏ → s-k 번역, GUI 실측 검증됨).

;;; Code:

;;;; 1. key-translation-map: <수식>-<자모> -> <수식>-<QWERTY>

(defconst my/dubeolsik-jamo->latin
  '((?ㅂ . ?q) (?ㅈ . ?w) (?ㄷ . ?e) (?ㄱ . ?r) (?ㅅ . ?t)
    (?ㅛ . ?y) (?ㅕ . ?u) (?ㅑ . ?i) (?ㅐ . ?o) (?ㅔ . ?p)
    (?ㅁ . ?a) (?ㄴ . ?s) (?ㅇ . ?d) (?ㄹ . ?f) (?ㅎ . ?g)
    (?ㅗ . ?h) (?ㅓ . ?j) (?ㅏ . ?k) (?ㅣ . ?l)
    (?ㅋ . ?z) (?ㅌ . ?x) (?ㅊ . ?c) (?ㅍ . ?v)
    (?ㅠ . ?b) (?ㅜ . ?n) (?ㅡ . ?m)
    ;; Shift 자모 (쌍자음/ㅒㅖ)
    (?ㅃ . ?Q) (?ㅉ . ?W) (?ㄸ . ?E) (?ㄲ . ?R) (?ㅆ . ?T)
    (?ㅒ . ?O) (?ㅖ . ?P))
  "두벌식 호환 자모 -> 같은 자리의 QWERTY 글자.")

(defvar my/korean-translated-modifier-combos
  '((control) (meta) (control meta) (super)
    (control shift) (meta shift) (control meta shift))
  "역번역 대상 수식 조합.  여기에 조합을 추가하면 그만큼 확장된다.
\(super) = Option 키(custom.el에서 ns-alternate-modifier='super).
super는 shift와 달리 수식 비트가 보존되므로 한글 모드에서 정상 동작한다.")

(defun my/install-dubeolsik-key-translations ()
  "`my/korean-translated-modifier-combos' × `my/dubeolsik-jamo->latin' 의 모든
<수식>-<자모> 를 <수식>-<latin> 으로 `key-translation-map' 에 등록한다.
수식 없는 맨자모는 건드리지 않으므로 한글 self-insert 는 영향 없음.
shift 포함 콤보는 Emacs 정규형이 'S- + 소문자'(C-S-k)이므로 latin을 소문자로
정규화한다.  단, 이 빌드의 NS 포트는 caseless 자모에서 shift 비트를 소실시킴이
실측으로 확인됨(한글모드 Ctrl+Shift+k == Ctrl+k, 둘 다 C-ㅏ로 도착) — 따라서
shift 콤보 엔트리는 실제로는 발화하지 않는 무해한 보험용이다.  한글모드에서
C-S- 단축키가 필요하면 비-shift 콤보(s-k 등)를 병행 바인딩할 것."
  (dolist (combo my/korean-translated-modifier-combos)
    (dolist (pair my/dubeolsik-jamo->latin)
      (let ((latin (if (memq 'shift combo)
                       (downcase (cdr pair))
                     (cdr pair))))
        (define-key key-translation-map
                    (vector (append combo (list (car pair))))
                    (vector (append combo (list latin))))))))
(my/install-dubeolsik-key-translations)

;;;; 2. sis: 프리픽스 뒤 '수식 없는 연속 키' (C-x o, M-s h r ...)

(use-package sis
  :config
  (sis-ism-lazyman-config "com.apple.keylayout.ABC"
                          "com.apple.inputmethod.Korean.2SetKorean"
                          'macism)
  (setq sis-prefix-override-keys
        '("C-c" "C-x" "C-h" "C-r" "M-SPC"   ; sis 기본값
          "M-g" "M-s" "M-o"))               ; + 표준 Meta 프리픽스 (M-s h r 등)
  ;; sis 기본 predicate는 magit·read-only·특수(*..*) 버퍼에서 override를 꺼버려서
  ;; magit 버퍼 안에서 C-c v l 같은 프리픽스가 한글 모드일 때 깨진다(C-c ㅍ ...).
  ;; → magit/read-only 비활성화는 제거하고, minibuffer + 특수(*..*) 버퍼만 유지
  ;;   (vterm 등 *버퍼*에서 C-c마다 입력소스가 펄럭이는 것 방지).
  (setq sis-prefix-override-buffer-disable-predicates
        (list 'minibufferp
              (lambda ()
                (let ((n (downcase (string-trim (buffer-name)))))
                  (and (string-prefix-p "*" n)
                       (not (string-prefix-p "*new*" n))
                       (not (string-prefix-p "*scratch*" n)))))))
  (sis-global-respect-mode 1)
  (sis-global-cursor-color-mode 1)          ; (선택) 한/영 상태를 커서 색으로
  (sis-global-context-mode 1)               ; (선택) 버퍼 문맥 따라 자동 전환
  )

(provide 'korean-ime-keys)
;;; korean-ime-keys.el ends here
