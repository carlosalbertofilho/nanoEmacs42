;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Este arquivo contém a configuração principal do Emacs usando Elpaca
;; como gerenciador de pacotes e Nano Emacs para a interface.

;;; Code:

;; =============================================================================
;; BOOTSTRAP DO ELPACA (Gerenciador de Pacotes)
;; =============================================================================

;; Desabilita package.el (já configurado no early-init.el, mas reforçamos aqui)
(setq package-enable-at-startup nil)

;; Define versão do instalador e diretórios do Elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

;; Cria diretórios se não existirem
(unless (file-exists-p elpaca-directory)
  (make-directory elpaca-directory t))

;; Lógica de bootstrap: verifica, clona, compila e carrega o Elpaca
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Instala o use-package via Elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Bloqueia até que o use-package esteja pronto
(elpaca-wait)

;; =============================================================================
;; DESATIVAÇÃO DE ELEMENTOS DA INTERFACE (Antes do Nano Emacs)
;; =============================================================================

;; Desativa elementos da interface (reforço do early-init.el)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; =============================================================================
;; TIPOGRAFIA E APARÊNCIA (Configurar ANTES do Nano Emacs)
;; =============================================================================

;; Fonte monoespaçada e tamanho
(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-size 11)

;; Espaçamento entre linhas (0 para código, 1 ou 2 para texto)
(setq-default line-spacing 0)

;; Sublinhado na posição de descida, não na linha de base
(setq x-underline-at-descent-line t)

;; Sem botões feios para checkboxes
(setq widget-image-enable nil)

;; Cursor em forma de linha e piscando
(set-default 'cursor-type '(bar . 1))
(blink-cursor-mode 1)

;; Sem sons
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Sem tooltips
(tooltip-mode 0)

;; Modo de parênteses (parte do tema)
(show-paren-mode t)

;; =============================================================================
;; NANO EMACS - Interface Minimalista
;; =============================================================================

;; Instala e configura o Nano Emacs
(use-package nano
  :ensure (:host github :repo "rougier/nano-emacs")
  :config
  ;; Layout padrão
  (require 'nano-layout)
  
  ;; Configuração do tema
  (require 'nano-base-colors)
  (require 'nano-faces)
  (require 'nano-theme-light)
  (require 'nano-theme-dark)
  
  ;; Linhas de cabeçalho e modo
  (require 'nano-modeline)
  
  ;; Modificação de keybindings do Nano
  (require 'nano-bindings)
  
  ;; Layout compacto (se solicitado via linha de comando)
  (when (member "-compact" command-line-args)
    (require 'nano-compact))
  
  ;; Mensagem de boas-vindas (SEM inhibit-message para permitir splash)
  (message "Bem-vindo ao GNU Emacs / Edição N Λ N O")
  (message (format "Tempo de inicialização: %s" (emacs-init-time)))
  
  ;; Splash screen (se não desabilitado via linha de comando)
  (unless (member "-no-splash" command-line-args)
    (require 'nano-splash))
  
  ;; Ajuda (se não desabilitada via linha de comando)
  (unless (member "-no-help" command-line-args)
    (require 'nano-help))
  
  ;; Ativa o Nano Emacs
  (require 'nano)
  
  ;; Garante que os elementos da interface permaneçam desativados
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; =============================================================================
;; HEADER 42 - Carrega o módulo do header da 42
;; =============================================================================

;; Carrega o arquivo header42.el
(load-file (expand-file-name "header42.el" user-emacs-directory))

;; Habilita o header 42 com atualização automática
(header-42-enable)

;; =============================================================================
;; UTILITÁRIOS E FUNÇÕES AUXILIARES
;; =============================================================================

;; Função para definir atalhos de acordo com a versão do Emacs
(defun bind-key (key command)
  "Define KEY para invocar COMMAND, usando a melhor API disponível para esta versão do Emacs."
  (if (fboundp 'keymap-global-set)
      (keymap-global-set key command)
    (global-set-key (kbd key) command)))

;; =============================================================================
;; ATIVAÇÃO DE MODOS ADICIONAIS
;; =============================================================================

;; Ativa destaque da linha atual
(global-hl-line-mode 1)

;; Mostra numero das linhas
(global-display-line-numbers-mode 1)

;; Mostra o número das colunas 
(column-number-mode 1)

;; highlight na linha corrente
(global-hl-line-mode 1)

;; Mostra match entre parenteses
(show-paren-mode 1)

;; Auto reverte arquivos alterados fora do Emacs 
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

;; =============================================================================
;; CONFIGURAÇÃO DE BACKUP E AUTO-SAVE
;; =============================================================================

;; Organiza arquivo de backup e auto-save
;; Cria os diretórios dentro de ~/.config/emacs/ (serão ignorados pelo git)
(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (autosave-dir (expand-file-name "auto-saves" user-emacs-directory)))
  ;; Cria os diretórios se não existirem
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p autosave-dir)
    (make-directory autosave-dir t))
  ;; Configura os caminhos
  (setq backup-directory-alist `(("." . ,backup-dir))
        make-backup-files t
        auto-save-default t
        auto-save-file-name-transforms `((".*" ,autosave-dir t))))

;; =============================================================================
;; CONFIGURAÇÃO DE INDENTAÇÃO E TABS
;; =============================================================================

;; Usa espaços ao invés de tabs, com largura de 4 espaços (padrão 42)
(setq-default indent-tabs-mode nil
              tab-width 4)

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

;; =============================================================================
;; BUFFER INICIAL
;; =============================================================================

;; Muda para o buffer *scratch* e depois exibe o splash do Nano Emacs
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Primeiro muda para o *scratch*
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (switch-to-buffer "*scratch*")
                                   ;; Depois exibe o splash do Nano Emacs
                                   (run-with-idle-timer 0.2 nil
                                                        (lambda ()
                                                          (when (fboundp 'nano-splash)
                                                            (nano-splash))))))))

(provide 'init)
;;; init.el ends here
