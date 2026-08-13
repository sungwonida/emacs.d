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
;;      자모가 아닌 자리(grave -> ₩)는 별도 테이블
;;      `my/korean-layout-punct->latin' 이 같은 방식으로 처리한다.
;;
;;   2. sis (smart-input-source) + macism (프리픽스 뒤 '수식 없는 연속 키'):
;;      C-x o 의 o, M-s h r 의 h r 같은 키는 IME가 한글로 조합해 버리는데,
;;      이는 자모 역번역으론 불가(정상 한글 입력과 구분 불가).  유일한 해법은
;;      OS 입력소스 자동전환: `sis-global-respect-mode'가 프리픽스/minibuffer
;;      진입 시 ABC로 바꿨다가 끝나면 한글로 복원한다(수동 CMD+SPC 제어는 유지).
;;
;;   3. sis + ghostel 읽기전용(모달) 모드 ('수식 없는 한 글자' 명령):
;;      copy/emacs 모드의 q·n·p 처럼 프리픽스 없이 단독으로 명령인 키도 (2)와
;;      같은 이유로 한글 모드에서 죽는다.  프리픽스가 없어 sis의 respect-mode가
;;      걸리지 않으므로, 모드 진입/종료에 직접 훅을 걸어 입력소스를 전환한다.
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
;; - '수식 없는 맨 글자'는 자모 역번역으로 절대 고칠 수 없다.  한글 모드에서
;;   맨 `q'는 IME 조합(marked text)에 먹혀 `?ㅂ' 키 이벤트 자체가 발생하지
;;   않는다 — NS 포트는 조합 중 텍스트를 ns-put-working-text →
;;   `ns-insert-working-text' 로 처리하는데, 이건 make-overlay + after-string
;;   이라 read-only 버퍼에서도 에러 없이 조용히 preedit만 띄운다
;;   (term/ns-win.el).  `q'를 한 번 눌러도 이벤트가 없고 두 번 눌러야 첫 ㅂ이
;;   확정되는데, 그때 두 번째 ㅂ이 조합 중으로 남아 모드를 나간 뒤 셸로
;;   입력돼 버린다.  → (2)/(3)의 입력소스 전환만이 해법이다.
;; - (3)은 ghostel 버퍼를 나가지 않고 모드를 바꾸는 경로만 복원한다.  copy-mode
;;   상태로 다른 버퍼로 넘어가면 그 순간엔 ABC 인 채인데, sis의
;;   `sis--restore-from-buffer'가 새 버퍼의 기억값으로 곧바로 복원하므로
;;   실사용상 문제되지 않는다.

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

(defun my/install-key-translations (table combos)
  "COMBOS × TABLE 의 모든 <수식>-<한글레이아웃 글자> 를 <수식>-<latin> 으로
`key-translation-map' 에 등록한다.  TABLE 은 (한글자 . latin자) alist.
수식 없는 맨글자는 건드리지 않으므로 한글 self-insert 는 영향 없음.
shift 포함 콤보는 Emacs 정규형이 'S- + 소문자'(C-S-k)이므로 latin을 소문자로
정규화한다.  단, 이 빌드의 NS 포트는 caseless 자모에서 shift 비트를 소실시킴이
실측으로 확인됨(한글모드 Ctrl+Shift+k == Ctrl+k, 둘 다 C-ㅏ로 도착) — 따라서
shift 콤보 엔트리는 실제로는 발화하지 않는 무해한 보험용이다.  한글모드에서
C-S- 단축키가 필요하면 비-shift 콤보(s-k 등)를 병행 바인딩할 것."
  (dolist (combo combos)
    (dolist (pair table)
      (let ((latin (if (memq 'shift combo)
                       (downcase (cdr pair))
                     (cdr pair))))
        (define-key key-translation-map
                    (vector (append combo (list (car pair))))
                    (vector (append combo (list latin))))))))

(defun my/install-dubeolsik-key-translations ()
  "자모 역번역을 `key-translation-map' 에 등록한다."
  (my/install-key-translations my/dubeolsik-jamo->latin
                               my/korean-translated-modifier-combos))
(my/install-dubeolsik-key-translations)

;;;; 1b. key-translation-map: 자모가 아닌데 글자가 바뀌는 키 (grave -> ₩)

(defconst my/korean-layout-punct->latin
  '((?₩ . ?\`)      ; U+20A9 WON SIGN
    (?￦ . ?\`))     ; U+FFE6 FULLWIDTH WON SIGN (입력소스 변종 대비)
  "한글 레이아웃에서 자모가 아닌 글자로 바뀌는 키 -> 같은 자리의 QWERTY 글자.
macOS 2벌식은 grave(`) 자리에서 백틱 대신 ₩ 를 내보낸다(한글 모드에서 백틱이
안 쳐지는 것으로 유명한 그 동작).  그래서 자모 테이블엔 걸리지 않고
`s-`' 가 `s-₩' 로 도착해 undefined 가 된다.")

(defvar my/korean-translated-punct-modifier-combos
  '((super))
  "문장부호 역번역을 적용할 수식 조합.  기본은 super(Option)만.
`s-`' = `other-frame'(프레임 전환, term/ns-win.el)을 한글 모드에서도 쓰기 위함.
control/meta 까지 넓히지 않는 이유: 한글 모드의 C-` / M-`(=Cmd+`, `tmm-menubar')
동작까지 바꾸게 되므로 필요할 때만 추가한다.")

(defun my/install-korean-punct-key-translations ()
  "`my/korean-layout-punct->latin' 역번역을 `key-translation-map' 에 등록한다."
  (my/install-key-translations my/korean-layout-punct->latin
                               my/korean-translated-punct-modifier-combos))
(my/install-korean-punct-key-translations)

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

;;;; 3. ghostel 읽기전용(copy/emacs) 모드 동안 입력소스를 ABC로

;; ghostel copy/emacs 모드는 read-only 모달 모드라 `q'(종료)처럼 수식 없는 한
;; 글자가 그대로 명령이다.  한글 모드에선 그 키가 IME 조합에 먹혀 명령 루프에
;; 도착조차 않는다(위 Limitations 참고) — 키맵으로는 못 고친다.  그래서 (2)와
;; 같은 처방을 모드 단위로 적용한다: 진입 때 ABC로 바꾸고 나갈 때 되돌린다.
;; 덕분에 `q' 뿐 아니라 copy-mode의 모든 키가 영문 모드와 똑같이 동작한다.
;;
;; sis 내부 상태와의 정합성: 공개 명령 `sis-set-english'/`sis-set-other'만
;; 쓴다.  둘 다 `sis--for-buffer-locked'를 풀고 `sis--update-state'를 거쳐
;; 버퍼의 기억값(`sis--for-buffer')까지 같이 갱신하므로, respect-mode의
;; post-command 복원이 우리 전환을 되돌리지 않는다.
;;
;; 주의: 내부 심볼 의존 — ghostel--enter-readonly(진입 단일 관문),
;; ghostel-semi-char-mode / ghostel-char-mode / ghostel-line-mode(종료 경로.
;; `ghostel-readonly-exit'는 결국 이 셋 중 하나를 호출한다).  소실 시 lwarn.

(defcustom my/korean-ime-force-english-in-readonly-term t
  "非nil 이면 ghostel 읽기전용(copy/emacs) 모드 동안 입력소스를 ABC로 강제한다.
모드를 나갈 때, 우리가 바꿨고 그 사이 사용자가 직접 한글로 되돌리지 않았다면
진입 전 입력소스로 복원한다."
  :type 'boolean
  :group 'sis)

(defvar-local my/korean-ime--source-before-readonly nil
  "읽기전용 모드 진입 때 우리가 ABC로 바꿨다면 그 직전 입력소스(`other').
바꾸지 않았으면 nil.  버퍼 로컬이라 ghostel 버퍼마다 독립적으로 추적된다.")

(defun my/korean-ime--current-lang ()
  "현재 OS 입력소스를 `english' / `other' 로 반환한다(sis 없으면 nil).
`sis--current'는 유휴 타이머로만 갱신돼 오래됐을 수 있으므로 실제 상태를 읽는다."
  (when (fboundp 'sis--get)
    (sis--get))
  (bound-and-true-p sis--current))

(defun my/korean-ime--readonly-force-english (&rest _)
  "ghostel 읽기전용 모드 진입 시 입력소스를 ABC로 바꾼다.
copy↔emacs 토글처럼 이미 읽기전용 안에서의 재진입이면 아무것도 하지 않는다
\(최초 진입 때 기억해 둔 원래 입력소스를 덮어쓰면 복원이 깨지므로)."
  (when (and my/korean-ime-force-english-in-readonly-term
             (fboundp 'sis-set-english)
             (not my/korean-ime--source-before-readonly)
             (eq (my/korean-ime--current-lang) 'other))
    (setq my/korean-ime--source-before-readonly 'other)
    (sis-set-english)))

(defun my/korean-ime--readonly-restore (&rest _)
  "읽기전용 모드를 나갈 때 진입 전 입력소스로 되돌린다.
모드 중에 사용자가 직접 한글로 바꿔 뒀다면(현재가 `english'가 아니면) 손대지
않는다 — 수동 조작이 항상 이긴다."
  (when my/korean-ime--source-before-readonly
    (let ((saved my/korean-ime--source-before-readonly))
      (setq my/korean-ime--source-before-readonly nil)
      (when (and (eq saved 'other)
                 (fboundp 'sis-set-other)
                 (eq (my/korean-ime--current-lang) 'english))
        (sis-set-other)))))

(with-eval-after-load 'ghostel
  ;; 진입: copy 모드도 emacs 모드도 `ghostel--enter-readonly'를 지난다.
  (if (fboundp 'ghostel--enter-readonly)
      (advice-add 'ghostel--enter-readonly :after
                  #'my/korean-ime--readonly-force-english)
    (lwarn 'my/korean-ime :warning
           "ghostel--enter-readonly 없음(패키지 업데이트?) — 읽기전용 모드 입력소스 전환 미설치"))
  ;; 종료: 읽기전용을 벗어나는 모드는 이 셋뿐이다.  emacs 모드는 여전히
  ;; 읽기전용이라 대상이 아니다(ABC 유지가 맞다).
  (dolist (fn '(ghostel-semi-char-mode ghostel-char-mode ghostel-line-mode))
    (if (fboundp fn)
        (advice-add fn :after #'my/korean-ime--readonly-restore)
      (lwarn 'my/korean-ime :warning
             "%s 없음(패키지 업데이트?) — 이 경로에선 입력소스가 복원되지 않음" fn))))

(provide 'korean-ime-keys)
;;; korean-ime-keys.el ends here
