;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Configuração do Nano Emacs Theme
;;; Code:

;; =======================================================
;; TIPOGRAFIA E APARÊNCIA (Configurar ANTES do Nano Emacs)
;; =======================================================

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


;; 1) Tema externo primeiro
;;(use-package doom-themes
;;  :ensure (doom-themes :host github :repo "doomemacs/themes")
;;  :config
;;  (load-theme 'doom-one t))            ;; escolha o seu Doom favorito

(use-package dracula-theme
  :ensure (dracula-theme :host github :repo "dracula/emacs")
  :config
  (load-theme 'dracula t))

;; Nano Emacs: Interface minimalista e elegante para o Emacs
;; Tema moderno inspirado em design minimalista, com elementos visuais limpos
;; GitHub: https://github.com/rougier/nano-emacs


;; 2) Depois, NANO “por cima” herdando a paleta atual
(use-package nano
  :ensure (:host github :repo "rougier/nano-emacs")
  ;; :after doom-themes  ;; OU --
  :after dracula-theme
  :init
  ;; Fonte
  (setq nano-font-family-monospaced "Roboto Mono"
       nano-font-family-proportional "Roboto Slab"
       nano-font-size 14)
  :config
  (require 'nano-base-colors)
  (require 'nano-faces) (nano-faces)   ;; define faces baseadas na paleta ativa
  (require 'nano-theme)  (nano-theme)  ;; aplica mapeamentos do NANO
  )

;; Modeline NANO (pacote separado via GNU ELPA)
(use-package nano-modeline
  :ensure t
  :after nano
  :config
  (nano-modeline-mode 1))

;; se você usa --daemon, garanta aplicação em novos frames GUI:
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (f) (with-selected-frame f
                          (nano-modeline-mode 1)))))

 ;; Garante que os elementos da interface permaneçam desativados
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

(provide 'my-theme)
