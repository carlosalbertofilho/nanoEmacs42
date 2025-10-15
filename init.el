;;; early-init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;

;; --- Straight.el ------------------------------------------------------------
;; Instala o straight.el como gerenciador de pacotes
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; --- Nano Emacs-- -----------------------------------------------------------
;; Instala e configura o Nano Emacs
(straight-use-package '(nano :type git :host github :repo "rougier/nano-emacs"))

;; Default layout (optional)
(require 'nano-layout)

;; Theme
(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-theme-dark)

;; Dark Theme
(nano-theme-set-dark)
;;(call-interactively 'nano-refresh-theme)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))


;; --- Typography stack -------------------------------------------------------
(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-size 11)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 1)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; No Tooltips
(tooltip-mode 0)

;; Paren mode is part of the theme
(show-paren-mode t)

(require 'nano)

;; --- bind key --------------------------------------------------------------------
;; define atalhos de acordo com a versão do emacs
(defun bind-key (key command)
  "Define KEY to invoke COMMAND, using the best available API for this Emacs version."
  (if (fboundp 'keymap-global-set)
      (keymap-global-set key command)
    (global-set-key (kbd key) command)))

;; --- Ivy --------------------------------------------------------------------
;; Mecanismo de complete no Emacs
(straight-use-package '(ivy :type git :host github :repo "abo-abo/swiper"))
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq search-default-mode #'char-fold-to-regexp)
(bind-key "C-s" #'swiper-isearch)
(bind-key "C-c C-r" #'ivy-resume)
(bind-key "<f6>" #'ivy-resume)
;;(bind-key "M-x" #'counsel-M-x)
(bind-key "C-x C-f" #'counsel-find-file)
(bind-key "<f1> f" #'counsel-describe-function)
(bind-key "<f1> v" #'counsel-describe-variable)
(bind-key "<f1> o" #'counsel-describe-symbol)
(bind-key "<f1> l" #'counsel-find-library)
(bind-key "<f2> i" #'counsel-info-lookup-symbol)
(bind-key "<f2> u" #'counsel-unicode-char)
(bind-key "C-c g" #'counsel-git)
(bind-key "C-c j" #'counsel-git-grep)
(bind-key "C-c k" #'counsel-ag)
(bind-key "C-x l" #'counsel-locate)
(bind-key "C-S-o" #'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") #'counsel-minibuffer-history)

;; --- Activate / Deactivate modes --------------------------------------------
(tool-bar-mode -1) (menu-bar-mode -1) (blink-cursor-mode -1)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
