;;; cpp-dev.el --- C para a 42 rio  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Author: Carlos Filho
;; Version: 1.0

;; Este arquivo configura os plugins necessário para desenvolver em c

;;; Commentary:
;;
;;


;;; Code:
;; =============================================================================
;; HEADER 42 - Carrega o módulo do header da 42
;; =============================================================================

(let* ((this-file (or load-file-name (buffer-file-name) default-directory))
       (dir (file-name-directory this-file)))
  (load (expand-file-name "header42.el" dir)))
(header-42-enable)

;; =============================================================================
;; CONFIGURAÇÃO DE INDENTAÇÃO E TABS
;; =============================================================================

;; Usa espaços ao invés de tabs, com largura de 4 espaços (padrão 42)

(defun my/smart-tab ()
  "Insere TAB literal no meio da linha, indenta no início."
  (interactive)
  (if (and (not (use-region-p))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      ;; Estamos no início da linha (só whitespace antes): indenta
      (c-indent-line-or-region)
    ;; Estamos no meio da linha: insere TAB literal
    (insert "\t")))

(defun my/c-mode-42-setup ()
  "Convenções 42 para C."
  (setq indent-tabs-mode t
        c-basic-offset 4
        tab-width 4
        c-syntactic-indentation t
        fill-column 80)
  (display-fill-column-indicator-mode 1)
  (electric-pair-mode 1)
  ;; Mostra espaços em branco visualmente
  (setq-local whitespace-style '(face tabs tab-mark))
  (whitespace-mode 1)
  ;; Rebind TAB para comportamento inteligente
  (local-set-key (kbd "TAB") #'my/smart-tab)
  ;; Checkers: gcc/clang + Norminette (encadeado)
  (when (fboundp 'flycheck-mode) (flycheck-mode 1)))

(add-hook 'c-mode-hook #'my/c-mode-42-setup)

(defun my/c++-mode-setup ()
  "Setup C++ moderno."
  (setq indent-tabs-mode nil
        c-basic-offset 4
        fill-column 100) ;; ajuste a seu gosto
  (display-fill-column-indicator-mode 1)
  (electric-pair-mode 1)
  (imenu-add-to-menubar "Index")
  ;; Clang-Format on save (só para C++)
  (when (and (executable-find "clang-format")
             (fboundp 'clang-format-buffer))
    (add-hook 'before-save-hook #'clang-format-buffer nil t))
  (when (fboundp 'flycheck-mode) (flycheck-mode 1)))

(add-hook 'c++-mode-hook #'my/c++-mode-setup)

;; Exceção: Makefiles DEVEM usar tabs reais (requisito do Make)
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;; Exceção adicional para outros modos de Makefile
(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

(add-hook 'makefile-bsdmake-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;; Define o tamanho padrão da coluna para 80
;; apenas em modos de programação
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fill-column 80)
            (setq display-fill-column-indicator-column 80)
            (display-fill-column-indicator-mode 1)))

;; LSP leve: ativa só se clangd estiver disponível
(when (and (require 'lsp-mode nil t)
           (executable-find "clangd"))
  (setq lsp-idle-delay 0.2
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-prefer-capf t)
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred))

;; =============================================================================
;; flycheck: Sistema de linting on-the-fly para Emacs
;; Adiciona suporte à Norminette como verificador customizado (42 style guide).
;; GitHub: https://github.com/flycheck/flycheck
;; =============================================================================

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1)
  :config
  ;; Configuração mais agressiva para detectar erros rapidamente
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled)
        flycheck-idle-change-delay 0.5
        flycheck-display-errors-delay 0.1)

  ;; Configuração do compilador
  (setq flycheck-c/c++-clang-executable "clang"
        flycheck-c/c++-gcc-executable "gcc")

  ;; Adiciona flags úteis para detectar mais erros
  (setq flycheck-clang-args '("-Wall" "-Wextra" "-pedantic" "-std=c99")
        flycheck-gcc-args '("-Wall" "-Wextra" "-pedantic" "-std=c99"))

  ;; ---------------------------------------------------------------------------
  ;; Custom Checker: Norminette (só adiciona se existir)
  ;; ---------------------------------------------------------------------------
  (when (executable-find "norminette")
    (flycheck-define-checker c-norminette
      "Checker personalizado para a Norminette (42)."
      :command ("norminette" source)
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ":" (message) line-end)
       (error line-start (file-name) ": Error!" line-end)
       (warning line-start (file-name) ":" line ":" (message) line-end))
      :modes c-mode)

    ;; Adiciona o checker da Norminette
    (add-to-list 'flycheck-checkers 'c-norminette)

    ;; Define ordem: primeiro clang/gcc, depois norminette
    (flycheck-add-next-checker 'c/c++-clang 'c-norminette t)
    (flycheck-add-next-checker 'c/c++-gcc 'c-norminette t))

  ;; ---------------------------------------------------------------------------
  ;; Atalho útil
  ;; ---------------------------------------------------------------------------
  (with-eval-after-load 'cc-mode
    (define-key c-mode-map (kbd "C-c !") #'flycheck-list-errors)
    (define-key c-mode-map (kbd "C-c n") #'flycheck-buffer)))

;; -----------------------------------------------------------------------------
;; Dica extra: rodar manualmente a Norminette no buffer atual
;; -----------------------------------------------------------------------------
(defun run-norminette ()
  "Executa Norminette no arquivo atual e mostra a saída."
  (interactive)
  (let ((buf (get-buffer-create "*Norminette Output*")))
    (with-current-buffer buf
      (erase-buffer)
      (call-process "norminette" nil buf t (buffer-file-name))
      (display-buffer buf))))
(global-set-key (kbd "C-c C-n") #'run-norminette)
