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
;; HEADER 42 - Funções para inserção e atualização automática
;; =============================================================================

;; Constantes do header 42
(defconst header-42-ascii-art
  '("       :::      ::::::::"
    "     :+:      :+:    :+:"
    "   +:+ +:+         +:+  "
    " +#+  +:+       +#+     "
    "+#+#+#+#+#+   +#+        "
    "    #+#    #+#          "
    "   ###   ########.fr    ")
  "ASCII art do logo 42.")

(defconst header-42-length 80
  "Comprimento total de cada linha do header.")

(defconst header-42-margin 5
  "Margem lateral do header.")

;; Funções auxiliares
(defun header-42-get-user ()
  "Retorna o nome do usuário (FT_LOGIN, USER ou fallback)."
  (or (getenv "FT_LOGIN")
      (getenv "USER")
      "csilva-d"))

(defun header-42-get-mail ()
  "Retorna o email do usuário."
  (let ((user (header-42-get-user)))
    (format "%s@student.42.fr" user)))

(defun header-42-get-filename ()
  "Retorna o nome do arquivo ou '< new >' se não existir."
  (or (file-name-nondirectory (buffer-file-name))
      "< new >"))

(defun header-42-get-date ()
  "Retorna a data/hora atual no formato do header 42."
  (format-time-string "%Y/%m/%d %H:%M:%S"))

(defun header-42-make-line (left right)
  "Cria uma linha do header com LEFT à esquerda e RIGHT à direita."
  (let* ((left-str (substring left 0 (min (length left)
                                           (- header-42-length
                                              (* 2 header-42-margin)
                                              (length right)))))
         (right-str right)
         (spaces-needed (- header-42-length
                          (* 2 header-42-margin)
                          (length left-str)
                          (length right-str)))
         (spaces (if (< spaces-needed 0) 0 spaces-needed))
         (start-margin (make-string (- header-42-margin 2) ?\s))
         (end-margin (make-string (- header-42-margin 2) ?\s)))
    (format "/*%s%s%s%s%s*/"
            start-margin
            left-str
            (make-string spaces ?\s)
            right-str
            end-margin)))

(defun header-42-get-ascii (line-number)
  "Retorna a linha LINE-NUMBER do ASCII art (1-indexed)."
  (if (and (>= line-number 1) (<= line-number 7))
      (nth (- line-number 1) header-42-ascii-art)
    ""))

(defun header-42-generate-line (n)
  "Gera a linha N do header (1-11)."
  (cond
   ;; Linha 1: borda superior
   ((= n 1)
    (format "/* %s */"
            (make-string (- header-42-length 6) ?*)))
   
   ;; Linha 2: linha vazia
   ((= n 2)
    (header-42-make-line "" ""))
   
   ;; Linha 3: ASCII art vazio
   ((= n 3)
    (header-42-make-line "" (header-42-get-ascii 1)))
   
   ;; Linha 4: nome do arquivo
   ((= n 4)
    (header-42-make-line (header-42-get-filename) (header-42-get-ascii 2)))
   
   ;; Linha 5: ASCII art vazio
   ((= n 5)
    (header-42-make-line "" (header-42-get-ascii 3)))
   
   ;; Linha 6: autor
   ((= n 6)
    (header-42-make-line (format "By: %s <%s>"
                                 (header-42-get-user)
                                 (header-42-get-mail))
                        (header-42-get-ascii 4)))
   
   ;; Linha 7: ASCII art vazio
   ((= n 7)
    (header-42-make-line "" (header-42-get-ascii 5)))
   
   ;; Linha 8: Created
   ((= n 8)
    (header-42-make-line (format "Created: %s by %s"
                                 (header-42-get-date)
                                 (header-42-get-user))
                        (header-42-get-ascii 6)))
   
   ;; Linha 9: Updated
   ((= n 9)
    (header-42-make-line (format "Updated: %s by %s"
                                 (header-42-get-date)
                                 (header-42-get-user))
                        (header-42-get-ascii 7)))
   
   ;; Linha 10: linha vazia
   ((= n 10)
    (header-42-make-line "" ""))
   
   ;; Linha 11: borda inferior
   ((= n 11)
    (format "/* %s */"
            (make-string (- header-42-length 6) ?*)))))

(defun header-42-insert ()
  "Insere o header 42 no início do arquivo."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Insere linha vazia após o header
    (insert "\n")
    ;; Insere as 11 linhas do header de baixo para cima
    (let ((line 11))
      (while (> line 0)
        (goto-char (point-min))
        (insert (header-42-generate-line line) "\n")
        (setq line (1- line))))))

(defun header-42-update ()
  "Atualiza as linhas 'Updated:' e filename do header 42 se existir."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Verifica se existe um header 42 (procura por "Updated:" na linha 9)
    (when (and (>= (count-lines (point-min) (point-max)) 11)
               (progn
                 (goto-char (point-min))
                 (forward-line 8)
                 (beginning-of-line)
                 (looking-at "^/\\*   Updated: ")))
      ;; Atualiza a linha 9 (Updated)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (header-42-generate-line 9))
      ;; Atualiza a linha 4 (filename) - caso o arquivo tenha sido renomeado
      (goto-char (point-min))
      (forward-line 3)
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (header-42-generate-line 4)))))

;; Hook para atualizar automaticamente ao salvar
(add-hook 'before-save-hook 'header-42-update)

;; Comando e atalho para inserir header
(defun stdheader ()
  "Insere ou atualiza o header 42."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (>= (count-lines (point-min) (point-max)) 11)
             (progn
               (goto-char (point-min))
               (forward-line 8)
               (beginning-of-line)
               (looking-at "^/\\*   Updated: ")))
        (header-42-update)
      (header-42-insert))))

;; Atalho para inserir o header da 42 (F1)
(bind-key "<f1>" 'stdheader)

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

;; Organiza arquivo de backup e auto-save
;; Cria os diretórios se não existirem (em ~/.emacs.d/ para não versionar)
(let ((backup-dir (expand-file-name "~/.emacs.d/backups"))
      (autosave-dir (expand-file-name "~/.emacs.d/auto-saves")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p autosave-dir)
    (make-directory autosave-dir t))
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
