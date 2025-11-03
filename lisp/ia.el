;;; ia.el --- Configurações para IA -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Este arquivo contém as configurações para gptel.

;;; Code:

(use-package gptel
    :ensure (:host github :repo "karthink/gptel")
    :config
    (setq gptel-api-key (getenv "OPENAI_API_KEY")))

(provide 'ia)

