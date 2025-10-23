;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
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
  ;;:config
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
