;;; edit-tools.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho


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

;; company: Framework moderno de autocompletar para Emacs
;; Fornece sugestões contextuais enquanto você digita, com interface leve,
;; backend modular e integração com LSP, yasnippet e diversas linguagens.
;; GitHub: https://github.com/company-mode/company-mode
(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-idle-delay 0.1
                company-minimum-prefix-length 1))

;; Vertico: Interface vertical minimalista para completação
;; Melhora a seleção de comandos, arquivos e buffers
;; GitHub: https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)  ;; Permite ciclar entre as opções (volta ao início)
  :init
  (vertico-mode))

;; Orderless: Estilo de completação flexível e poderoso
;; Permite buscar com múltiplas palavras em qualquer ordem
;; Ex: "buf em li" encontra "buffer-emacs-lisp" e "emacs-lisp-buffer"
;; GitHub: https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Marginalia: Anotações ricas para minibuffer
;; Adiciona informações úteis ao lado dos candidatos de completação
;; GitHub: https://github.com/minad/marginalia
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

;; Nerd Icons Completion: Adiciona ícones à completação
;; Exibe ícones bonitos ao lado dos candidatos de completação
;; GitHub: https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Corfu: Completação automática in-buffer (popup)
;; Sistema moderno de auto-completação enquanto você digita
;; GitHub: https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :init
  ;; Habilita Corfu globalmente (recomendado)
  ;; Funciona em todos os modos que fornecem completação (Capfs)
  (global-corfu-mode)

  ;; Extensões opcionais (descomente se desejar):
  ;; (corfu-history-mode)     ;; Histórico de completações
  (corfu-popupinfo-mode)   ;; Informações detalhadas em popup

  ;; Customizações opcionais (descomente e ajuste conforme necessário):
  :custom
  (corfu-cycle t)                ;; Cicla entre candidatos
  ;; (corfu-quit-at-boundary nil)   ;; Não sai ao chegar no limite
  ;; (corfu-quit-no-match nil)      ;; Não sai mesmo sem match
  ;; (corfu-preview-current nil)    ;; Desabilita preview do candidato atual
  ;; (corfu-preselect 'prompt)      ;; Pré-seleciona o prompt
  ;; (corfu-on-exact-match nil)     ;; Configuração para matches exatos

  ;; Habilitar apenas em modos específicos (alternativa ao global):
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  )

;; Consult: Comandos de busca e navegação aprimorados
;; Fornece versões melhoradas de comandos comuns com preview e filtros
;; GitHub: https://github.com/minad/consult
(use-package consult
  :ensure t
  
  ;; Habilita preview automático no buffer *Completions*
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configuração inicial (sempre executada, não lazy)
  :init
  
  ;; Melhora a formatação da preview de registradores
  ;; Substitui a janela padrão do Emacs pela versão do Consult (mais bonita)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Usa Consult para mostrar localizações do xref com preview
  ;; xref = sistema de referências cruzadas
  ;; (ir para definição, encontrar usos, etc)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configurações adicionais (carregadas após o pacote)
  :config
  ;; -----------------------------------------------------------------------------
  ;; COMANDOS DE MODO (C-c prefix) – globais
  ;; -----------------------------------------------------------------------------
  (define-key global-map (kbd "C-c M-x") #'consult-mode-command)
  (define-key global-map (kbd "C-c h")   #'consult-history)
  (define-key global-map (kbd "C-c k")   #'consult-kmacro)
  (define-key global-map (kbd "C-c m")   #'consult-man)
  (define-key global-map (kbd "C-c i")   #'consult-info)

  ;; -----------------------------------------------------------------------------
  ;; BUFFERS / JANELAS (C-x prefix)
  ;; -----------------------------------------------------------------------------
  ;; C-x M-:
  (define-key global-map (kbd "C-x M-:") #'consult-complex-command)

  ;; C-x b / 4 b / 5 b / t b / r b / p b — usar submaps quando existirem
  (define-key global-map (kbd "C-x b") #'consult-buffer)

  (when (boundp 'ctl-x-4-map)
    (define-key ctl-x-4-map (kbd "b") #'consult-buffer-other-window))

  (when (boundp 'ctl-x-5-map)
    (define-key ctl-x-5-map (kbd "b") #'consult-buffer-other-frame))

  ;; Emacs 27+: tab-prefix-map
  (if (boundp 'tab-prefix-map)
      (define-key tab-prefix-map (kbd "b") #'consult-buffer-other-tab)
    (define-key global-map (kbd "C-x t b") #'consult-buffer-other-tab))

  (when (boundp 'ctl-x-r-map)
    (define-key ctl-x-r-map (kbd "b") #'consult-bookmark))
  ;; Projeto (C-x p ...)
  (if (boundp 'project-prefix-map)
      (define-key project-prefix-map (kbd "b") #'consult-project-buffer)
    (define-key global-map (kbd "C-x p b") #'consult-project-buffer))

  ;; -----------------------------------------------------------------------------
  ;; REGISTRADORES (M-# e M-')
  ;; -----------------------------------------------------------------------------
  (define-key global-map (kbd "M-#")   #'consult-register-load)
  (define-key global-map (kbd "M-'")   #'consult-register-store)
  (define-key global-map (kbd "C-M-#") #'consult-register)

  ;; -----------------------------------------------------------------------------
  ;; HISTÓRICO (M-y)
  ;; -----------------------------------------------------------------------------
  (define-key global-map (kbd "M-y") #'consult-yank-pop)

  ;; -----------------------------------------------------------------------------
  ;; NAVEGAÇÃO (M-g prefix) – usar goto-map
  ;; -----------------------------------------------------------------------------
  (require 'goto-addr nil t) ;; opcional; garante maps carregados cedo
  (define-key goto-map (kbd "e")   #'consult-compile-error)
  (define-key goto-map (kbd "f")   #'consult-flymake)
  (define-key goto-map (kbd "g")   #'consult-goto-line)
  (define-key goto-map (kbd "M-g") #'consult-goto-line) ;; alternativo
  (define-key goto-map (kbd "o")   #'consult-outline)
  (define-key goto-map (kbd "m")   #'consult-mark)
  (define-key goto-map (kbd "k")   #'consult-global-mark)
  (define-key goto-map (kbd "i")   #'consult-imenu)
  (define-key goto-map (kbd "I")   #'consult-imenu-multi)

  ;; -----------------------------------------------------------------------------
  ;; BUSCA (M-s prefix) – usar search-map
  ;; -----------------------------------------------------------------------------
  (define-key search-map (kbd "d") #'consult-find)
  (define-key search-map (kbd "c") #'consult-locate)
  (define-key search-map (kbd "g") #'consult-grep)
  (define-key search-map (kbd "G") #'consult-git-grep)
  (define-key search-map (kbd "r") #'consult-ripgrep)
  (define-key search-map (kbd "l") #'consult-line)
  (define-key search-map (kbd "L") #'consult-line-multi)
  (define-key search-map (kbd "k") #'consult-keep-lines)
  (define-key search-map (kbd "u") #'consult-focus-lines)
  (define-key search-map (kbd "e") #'consult-isearch-history)

  ;; -----------------------------------------------------------------------------
  ;; INTEGRAÇÃO COM MINIBUFFER
  ;; -----------------------------------------------------------------------------
  (with-eval-after-load 'minibuffer
    (define-key minibuffer-local-map (kbd "M-s") #'consult-history)
    (define-key minibuffer-local-map (kbd "M-r") #'consult-history))

)

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
