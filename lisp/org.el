;;; org.el --- Configurações para Org Mode e Markdown -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Este arquivo contém as configurações para o Org Mode e Markdown.

;;; Code:


;(use-package org
;  :ensure t
;  :after elpaca
;  :config
;  (setq org-ellipsis " ▼")
;  (setq org-log-done 'time)
;  (setq org-hide-leading-stars t)
;  (setq org-startup-indented t)
;  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'org)
;;; org.el ends here
