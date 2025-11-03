;;; IA.el --- Configurações para IA -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Este arquivo contém as configurações para gptel.

;;; Code:


(use-package gptel
    :ensure t
    :config
    (setq gptel-api-key (getenv "OPENAI_API_KEY"))

(use-package gptel-magit
    :ensure t
    :after (gptel magit)
    :config
    (gptel-magit-setup))

(provide 'IA)
