;;; elisp-dev.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho

;; =============================================================================
;; elisp-dev: Ambiente de desenvolvimento Emacs Lisp
;; Foco em: avaliação rápida, lint (checkdoc / package-lint), pares/indentação,
;; navegação (imenu/xref), REPL (ielm) e integração opcional com Flycheck/Company.
;; =============================================================================

;; --- Hooks e comportamento base ------------------------------------------------
(defun my/elisp-dev-setup ()
  "Configurações para `emacs-lisp-mode`."
  ;; Docs inline
  (eldoc-mode 1)
  ;; Indentação agressiva (mantém o buffer consistentemente indentado)
  (when (fboundp 'aggressive-indent-mode)
    (aggressive-indent-mode 1))
  ;; Delimitadores coloridos (visualiza níveis de parênteses)
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode 1))
  ;; Pareamento (fallback leve se não usar paredit/smartparens)
  (electric-pair-mode 1)
  ;; Menu de funções/defs
  (imenu-add-to-menubar "Index")
  ;; Checkdoc: style guide de docstrings/headers
  (checkdoc-minor-mode 1)
  ;; Flymake para elisp/checkdoc (nativo, Emacs 26+)
  (when (fboundp 'flymake-mode)
    (flymake-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-dev-setup)

;; --- Atalhos úteis em emacs-lisp-mode -----------------------------------------
(with-eval-after-load 'elisp-mode
  (let ((map emacs-lisp-mode-map))
    (define-key map (kbd "C-c C-b") #'eval-buffer)
    (define-key map (kbd "C-c C-r") #'eval-region)
    (define-key map (kbd "C-c C-f") #'eval-defun)
    (define-key map (kbd "C-c C-l") #'load-file)
    (define-key map (kbd "C-c C-c") #'byte-compile-file)
    (define-key map (kbd "C-c i")   #'ielm)
    ;; Ajuda contextual (troca para helpful se existir)
    (define-key map (kbd "C-c C-d")
      (if (fboundp 'helpful-at-point) #'helpful-at-point #'describe-symbol))
    ;; Macrostep (expandir macros interativamente)
    (when (fboundp 'macrostep-expand)
      (define-key map (kbd "C-c C-e") #'macrostep-expand))))

;; --- Integração opcional: Helpful / Macrostep ---------------------------------
(use-package helpful
  :commands
  (helpful-at-point helpful-function helpful-variable helpful-symbol))
(use-package macrostep :commands (macrostep-expand))

;; --- Lint extra: package-lint (opcional, roda sob demanda) ---------------------
;; Útil para checar headers, deps e convenções de pacotes ELPA/MELPA.
(use-package package-lint
  :commands (package-lint-current-buffer package-lint-batch-and-exit))

;; Atalhos rápidos para lint:
(with-eval-after-load 'package-lint
  (define-key emacs-lisp-mode-map (kbd "C-c L") #'package-lint-current-buffer))

;; --- Se você usa Flycheck, adiciona package-lint como checker ------------------
(with-eval-after-load 'flycheck
  ;; Checker nativo para elisp já existe; encadeie package-lint depois:
  (when (require 'package-lint-flycheck nil t)
    (flycheck-add-next-checker 'emacs-lisp 'package-lint)))

;; --- Company/Corfu para completion em Elisp -----------------------------------
;; Escolha apenas um bloco (Company OU Corfu). Ambos aproveitam `capf`.
;; COMPANY:
(when (featurep 'company)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local company-backends '(company-capf))
              (company-mode 1))))

;; CORFU:
(when (featurep 'corfu)
  (add-hook 'emacs-lisp-mode-hook #'corfu-mode))

;; --- Qualidade de compilação e warnings ---------------------------------------
;; Mostra mais avisos na compilação byte-compiled.
(setq byte-compile-warnings
      '(free-vars unresolved callargs redefine obsolete
        cl-functions interactive-only make-local mapcar constants
        suspicious docstrings))

;; Recompilar automaticamente arquivos .el deste projeto/pasta?
;; (descomente se quiser)
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (and (eq major-mode 'emacs-lisp-mode)
;;                        (buffer-file-name))
;;               (byte-compile-file (buffer-file-name)))))

;; --- Qualidade de vida: botão pra reportar bug referenciando linha/arquivo ----
(defun my/elisp-report-thing ()
  "Abre *Messages* com arquivo:linha para copiar em issues/commits."
  (interactive)
  (message "%s:%d"
           (or (buffer-file-name) (buffer-name))
           (line-number-at-pos)))

(define-key emacs-lisp-mode-map (kbd "C-c ! r") #'my/elisp-report-thing)

;; --- REPL confortável ----------------------------------------------------------
;; ielm com histórico maior e navegação
(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook
            (lambda ()
              (setq comint-input-ring-size 5000)
              (setq comint-move-point-for-output t))))
