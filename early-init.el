;;; early-init.el --- Early initialization for Nano Emacs with Elpaca -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; This file is loaded before init.el and package system initialization.
;; Use it for performance optimizations and early GUI setup.

;;; Code:

;; --- Package Management -----------------------------------------------------
;; Desabilitar package.el completamente (usamos Elpaca)
(setq package-enable-at-startup nil)

;; --- Performance Optimizations ----------------------------------------------
;; Aumentar threshold de garbage collection durante startup
;; Será restaurado no final do init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reduzir processamento de nomes de arquivo durante startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restaurar após startup (adicionar ao final do init.el ou usar hook)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

;; --- GUI Optimizations (Nano Emacs) -----------------------------------------
;; Desabilitar elementos GUI o mais cedo possível para evitar flicker
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Configurações adicionais de frame para Nano Emacs
(setq frame-inhibit-implied-resize t)  ; Não redimensionar frame automaticamente
(setq frame-resize-pixelwise t)        ; Redimensionamento pixel-perfect

;; --- Native Compilation (se disponível) -------------------------------------
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil  ; Silenciar warnings de compilação
        native-comp-deferred-compilation t            ; Compilação assíncrona
        native-comp-speed 2))                         ; Nível de otimização

;; --- Startup Optimizations --------------------------------------------------
;; Desabilitar verificações desnecessárias durante startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Não carregar default.el
(setq inhibit-default-init t)

;; Reduzir ruído visual durante inicialização
(setq inhibit-compacting-font-caches t)  ; Não compactar cache de fontes

;;; early-init.el ends here
