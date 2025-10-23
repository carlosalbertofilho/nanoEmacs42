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
;; FERRAMENSTAS DE EDIÇÃO
;; =========================================================
(load (locate-user-emacs-file "src/edit-tools.el") nil 'nomessage)

;; =========================================================
;; FERRAMENSTAS DE GERÊNCIA DE ARQUIVOS
;; =========================================================
(load (locate-user-emacs-file "src/file-manager.el") nil 'nomessage)

;; =========================================================
;; TERMINAL E ACESSO REMOTO (ssh)
;; =========================================================
(load (locate-user-emacs-file "src/shell.el") nil 'nomessage)

;; =========================================================
;; C PARA A 42Rio
;; =========================================================
(load (locate-user-emacs-file "src/cpp-dev.el") nil 'nomessage)

;; =========================================================
;; ELISP DE LEI
;; =========================================================
(load (locate-user-emacs-file "src/elisp-dev.el") nil 'nomessage)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9c6aa7eb1bde73ba1142041e628827492bd05678df4d9097cda21b1ebcb8f8b9"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
