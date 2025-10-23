;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Este arquivo contém a configuração principal do Emacs usando Elpaca
;; como gerenciador de pacotes e Nano Emacs para a interface.

;;; Code:

;; =========================================================
;; BOOTSTRAP DO ELPACA (Gerenciador de Pacotes)
;; =========================================================
(load (locate-user-emacs-file "src/elpaca-bootstrap.el") nil 'nomessage)

;; =========================================================
;; APARÊNCIA E TIPOGRAFIA
;; =========================================================
(load (locate-user-emacs-file "src/theme.el") nil 'nomessage)


;; =========================================================
;; SISTEMA DE ATALHOS E DESCOBERTA DE COMANDOS
;; =========================================================

;; Which-key: Mostra atalhos disponíveis em tempo real
;; Quando você pressiona um prefixo (como C-x), mostra os comandos disponíveis
;; GitHub: https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  ;; Tempo de espera antes de mostrar a ajuda (em segundos)
  (setq which-key-idle-delay 0.8)
  ;; Exibe a janela de ajuda na parte inferior
  (which-key-setup-side-window-bottom))

;; auto-complete: Extensão de autocompletar inteligente para Emacs
;; Fornece sugestões enquanto você digita, com interface visual e suporte a fontes de conclusão configuráveis.
;; GitHub: https://github.com/auto-complete/auto-complete 
(use-package auto-complete
  ;; pega direto do GitHub, funciona bem com Elpaca
  :ensure (auto-complete :host github :repo "auto-complete/auto-complete")
  :init
  ;; ajustes opcionais
  (setq ac-use-menu-map t
        ac-auto-show-menu 0.2
        ac-delay 0.0)
  :config
  ;; precisa carregar o módulo que define ac-config-default
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode 1))
