;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Este arquivo contém a configuração principal do Emacs usando Elpaca
;; como gerenciador de pacotes e Nano Emacs para a interface.

;;; Code:

;; =============================================================================
;; BOOTSTRAP DO ELPACA (Gerenciador de Pacotes)
;; =============================================================================

;; Desabilita package.el (já configurado no early-init.el, mas reforçamos aqui)
(setq package-enable-at-startup nil)

;; Define versão do instalador e diretórios do Elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

;; Cria diretórios se não existirem
(unless (file-exists-p elpaca-directory)
  (make-directory elpaca-directory t))

;; Lógica de bootstrap: verifica, clona, compila e carrega o Elpaca
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Instala o use-package via Elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Bloqueia até que o use-package esteja pronto
(elpaca-wait)

;; =============================================================================
;; DESATIVAÇÃO DE ELEMENTOS DA INTERFACE (Antes do Nano Emacs)
;; =============================================================================

;; Desativa elementos da interface (reforço do early-init.el)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; =============================================================================
;; TIPOGRAFIA E APARÊNCIA (Configurar ANTES do Nano Emacs)
;; =============================================================================

;; Fonte monoespaçada e tamanho
(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-size 11)

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

;; =============================================================================
;; NANO EMACS - Interface Minimalista
;; =============================================================================

;; Nano Emacs: Interface minimalista e elegante para o Emacs
;; Tema moderno inspirado em design minimalista, com elementos visuais limpos
;; GitHub: https://github.com/rougier/nano-emacs
(use-package nano
  :ensure (:host github :repo "rougier/nano-emacs")
  :config
  ;; Layout padrão do Nano
  (require 'nano-layout)
  
  ;; Sistema de cores e temas
  (require 'nano-base-colors)
  (require 'nano-faces)
  (require 'nano-theme-light)
  (require 'nano-theme-dark)
  
  ;; Modeline customizada (barra de status)
  (require 'nano-modeline)
  
  ;; Keybindings customizados do Nano
  (require 'nano-bindings)
  
  ;; Layout compacto (ativa se passar argumento -compact na linha de comando)
  (when (member "-compact" command-line-args)
    (require 'nano-compact))
  
  ;; Mensagem de boas-vindas
  (message "Bem-vindo ao GNU Emacs / Edição N Λ N O")
  (message (format "Tempo de inicialização: %s" (emacs-init-time)))
  
  ;; Tela de splash (desativa se passar -no-splash na linha de comando)
  (unless (member "-no-splash" command-line-args)
    (require 'nano-splash))
  
  ;; Sistema de ajuda do Nano (desativa se passar -no-help na linha de comando)
  (unless (member "-no-help" command-line-args)
    (require 'nano-help))
  
  ;; Ativa todos os componentes do Nano Emacs
  (require 'nano)
  
  ;; Garante que os elementos da interface permaneçam desativados
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; =============================================================================
;; SISTEMA DE ATALHOS E DESCOBERTA DE COMANDOS
;; =============================================================================

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

;; General: Framework avançado para definir keybindings
;; Permite criar esquemas de atalhos organizados e hierárquicos
;; GitHub: https://github.com/noctuid/general.el
(use-package general
  :ensure t)

;; =============================================================================
;; EDIÇÃO E COMPLETAÇÃO
;; =============================================================================

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
  ;; xref = sistema de referências cruzadas (ir para definição, encontrar usos, etc)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configurações adicionais (carregadas após o pacote)
  :config

  ;; =============================================================================
  ;; ATALHOS DO CONSULT - Organizados por categoria usando general.el
  ;; =============================================================================
  
  ;; ---------------------------------------------------------------------------
  ;; COMANDOS DE MODO (C-c prefix) - Contextuais ao modo atual
  ;; ---------------------------------------------------------------------------
  (general-define-key
   "C-c M-x" 'consult-mode-command    ;; Lista comandos específicos do modo atual
                                      ;; Ex: em python-mode mostra comandos de Python
   "C-c h"   'consult-history         ;; Histórico de comandos do minibuffer
                                      ;; Útil para repetir comandos complexos anteriores
   "C-c k"   'consult-kmacro          ;; Lista e executa macros de teclado gravadas
                                      ;; Macros = sequências de comandos gravadas
   "C-c m"   'consult-man             ;; Busca e visualiza páginas do manual (man pages)
                                      ;; Com preview interativo
   "C-c i"   'consult-info)           ;; Busca em documentação Info do Emacs
                                      ;; Info = sistema de documentação do GNU

  ;; ---------------------------------------------------------------------------
  ;; BUFFERS E JANELAS (C-x prefix) - Gerenciamento de buffers/frames
  ;; ---------------------------------------------------------------------------
  (general-define-key
   "C-x M-:"  'consult-complex-command      ;; Repete comando complexo anterior
                                            ;; Permite editar antes de executar
   "C-x b"    'consult-buffer               ;; Troca de buffer com preview
                                            ;; Substitui switch-to-buffer padrão
   "C-x 4 b"  'consult-buffer-other-window  ;; Abre buffer em outra janela (split)
   "C-x 5 b"  'consult-buffer-other-frame   ;; Abre buffer em novo frame (janela OS)
   "C-x t b"  'consult-buffer-other-tab     ;; Abre buffer em nova aba (Emacs 27+)
   "C-x r b"  'consult-bookmark             ;; Lista e pula para bookmarks salvos
                                            ;; Bookmarks = posições salvas em arquivos
   "C-x p b"  'consult-project-buffer)      ;; Lista buffers apenas do projeto atual
                                            ;; Projeto = repositório git ou diretório raiz

  ;; ---------------------------------------------------------------------------
  ;; REGISTRADORES (M-# e M-' prefix) - Armazenamento temporário
  ;; ---------------------------------------------------------------------------
  (general-define-key
   "M-#"   'consult-register-load    ;; Carrega conteúdo de um registrador
                                     ;; Registradores = clipboard múltiplo numerado
   "M-'"   'consult-register-store   ;; Salva seleção atual em registrador
                                     ;; Útil para copiar múltiplos trechos
   "C-M-#" 'consult-register)        ;; Lista todos registradores com preview
                                     ;; Mostra conteúdo de cada um

  ;; ---------------------------------------------------------------------------
  ;; HISTÓRICO (M-y) - Clipboard e kill-ring
  ;; ---------------------------------------------------------------------------
  (general-define-key
   "M-y" 'consult-yank-pop)          ;; Histórico completo do clipboard (kill-ring)
                                     ;; Permite escolher entre itens copiados anteriormente
                                     ;; Substitui yank-pop padrão com interface melhor

  ;; ---------------------------------------------------------------------------
  ;; NAVEGAÇÃO (M-g prefix) - Ir para linha/função/erro
  ;; ---------------------------------------------------------------------------
  (general-define-key
   :prefix "M-g"
   "e"   'consult-compile-error      ;; Pula para erros de compilação
                                     ;; Lista erros do *compilation* buffer
   "f"   'consult-flymake            ;; Pula para erros/avisos do Flymake
                                     ;; Flymake = verificador de sintaxe on-the-fly
   "g"   'consult-goto-line          ;; Ir para número de linha com preview
   "M-g" 'consult-goto-line          ;; Atalho alternativo (mantém M pressionado)
   "o"   'consult-outline            ;; Navegação por outline/seções do arquivo
                                     ;; Mostra estrutura: funções, classes, headers
   "m"   'consult-mark               ;; Lista de marcas locais do buffer atual
                                     ;; Marcas = posições salvas com C-SPC C-SPC
   "k"   'consult-global-mark        ;; Lista de marcas globais (todos buffers)
   "i"   'consult-imenu              ;; Índice de funções/classes do buffer atual
                                     ;; Imenu = sistema de navegação por símbolos
   "I"   'consult-imenu-multi)       ;; Imenu em múltiplos buffers do projeto
                                     ;; Busca funções em vários arquivos

  ;; ---------------------------------------------------------------------------
  ;; BUSCA EM ARQUIVOS (M-s prefix) - Find, grep, ripgrep
  ;; ---------------------------------------------------------------------------
  (general-define-key
   :prefix "M-s"
   "d" 'consult-find              ;; Busca arquivos por nome (comando find)
                                  ;; Recursivo a partir do diretório atual
   "c" 'consult-locate            ;; Busca arquivos no sistema todo (locate)
                                  ;; Usa banco de dados do updatedb (mais rápido)
   "g" 'consult-grep              ;; Busca texto em arquivos (grep)
                                  ;; Tradicional, funciona em qualquer sistema
   "G" 'consult-git-grep          ;; Busca apenas em arquivos rastreados pelo git
                                  ;; Mais rápido que grep normal em projetos git
   "r" 'consult-ripgrep           ;; Busca com ripgrep (MUITO mais rápido)
                                  ;; Requer ripgrep instalado: apt install ripgrep
   "l" 'consult-line              ;; Busca linhas no buffer atual
                                  ;; Alternativa ao C-s com melhor interface
   "L" 'consult-line-multi        ;; Busca linhas em múltiplos buffers
                                  ;; Útil para buscar em vários arquivos abertos
   "k" 'consult-keep-lines        ;; Mantém apenas linhas que correspondem ao padrão
                                  ;; Como grep mas filtra o buffer atual
   "u" 'consult-focus-lines       ;; Oculta linhas que não correspondem (foco)
                                  ;; Esconde temporariamente, não deleta
   "e" 'consult-isearch-history)  ;; Histórico de buscas do isearch
                                  ;; Lista buscas anteriores feitas com C-s

  ;; ---------------------------------------------------------------------------
  ;; INTEGRAÇÃO COM ISEARCH - Melhorias durante busca incremental
  ;; ---------------------------------------------------------------------------
  (general-define-key
   :keymaps 'isearch-mode-map      ;; Atalhos ativos durante C-s (isearch)
   "M-e"   'consult-isearch-history ;; Histórico de buscas (durante isearch)
   "M-s e" 'consult-isearch-history ;; Alternativa
   "M-s l" 'consult-line            ;; Alterna para consult-line durante busca
   "M-s L" 'consult-line-multi)     ;; Alterna para busca multi-buffer

  ;; ---------------------------------------------------------------------------
  ;; INTEGRAÇÃO COM MINIBUFFER - Histórico de comandos
  ;; ---------------------------------------------------------------------------
  (general-define-key
   :keymaps 'minibuffer-local-map  ;; Atalhos ativos no minibuffer
   "M-s" 'consult-history           ;; Navega histórico para frente
   "M-r" 'consult-history)          ;; Navega histórico para trás
                                    ;; Útil ao digitar comandos com M-x

  ;; ---------------------------------------------------------------------------
  ;; CONFIGURAÇÕES DE PREVIEW - Quando e como mostrar preview
  ;; ---------------------------------------------------------------------------
  (consult-customize
   ;; Preview de temas: mostra preview ao navegar (delay 0.2s)
   consult-theme :preview-key '(:debounce 0.2 any)
   
   ;; Preview de buscas: mostra contexto do resultado (delay 0.4s)
   ;; any = qualquer tecla dispara preview (setas, letras, etc)
   ;; :debounce = aguarda 0.4s de inatividade antes de mostrar
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; ---------------------------------------------------------------------------
  ;; NARROWING - Filtragem por tipo
  ;; ---------------------------------------------------------------------------
  (setq consult-narrow-key "<")    ;; Tecla < para ativar narrowing
                                   ;; Durante consult-buffer: < b = só buffers
                                   ;;                         < f = só files
                                   ;;                         < p = só project
                                   ;; Permite filtrar resultados por categoria
  ) ;; Fecha o use-package consult

;; =============================================================================
;; GERENCIAMENTO DE ARQUIVOS E QUALIDADE DE VIDA
;; =============================================================================

;; Dired-X: Extensões avançadas para o Dired (gerenciador de arquivos padrão)
;; Adiciona funcionalidades extras como omit-mode, guess-shell-command, etc.
;; Permite ocultar arquivos desnecessários e comandos inteligentes
(use-package dired-x
  :ensure t
  :config
  ;; Configura dired-omit-mode para ocultar arquivos ocultos (dotfiles)
  ;; Útil para limpar a visualização removendo .DS_Store, .git, etc.
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Diredfl: Colorização e destaque sintático para Dired
;; Adiciona cores diferentes para tipos de arquivo, permissões e diretórios
;; Melhora a legibilidade visual do gerenciador de arquivos
;; GitHub: https://github.com/purcell/diredfl
(use-package diredfl
  :ensure t
  :hook
  ((dired-mode . diredfl-mode)
   ;; Também ativa colorização no preview de diretórios do Dirvish
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  ;; Deixa nomes de diretórios em negrito para maior destaque
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Nerd Icons: Fonte de ícones moderna para Emacs
;; Fornece ícones baseados em fonte para arquivos, diretórios e modos
;; Usado como backend de ícones pelo Dirvish e outros pacotes
;; Requer fonte Nerd Font instalada no sistema
;; GitHub: https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :ensure t
  :config
  ;; Instala as fontes automaticamente se não estiverem presentes
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))

;; Dirvish: Gerenciador de arquivos moderno e poderoso
;; Substitui o Dired padrão com interface melhorada, ícones e preview
;; Funcionalidades: árvore de diretórios, preview de arquivos, ícones, git info
;; GitHub: https://github.com/alexluigit/dirvish
(use-package dirvish
  :ensure t
  
  ;; Substitui o Dired padrão automaticamente
  :init
  (dirvish-override-dired-mode)
  
  ;; Carrega o módulo de ícones explicitamente
  :config
  (require 'dirvish-side)
  (require 'dirvish-icons)
  
  ;; Configurações de aparência e comportamento
  ;; Atributos mostrados: ícones, tamanho, estado da árvore, mensagem git
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  
  ;; Customizações específicas dos ícones Nerd Icons
  (setq dirvish-icon-delimiter " ")         ;; Espaço após cada ícone
  (setq dirvish-nerd-icons-offset 0.0)     ;; Ajuste vertical dos ícones (0.0 = padrão)
  (setq dirvish-nerd-icons-height nil)     ;; Altura dos ícones (nil = padrão, try 0.8)
  (setq dirvish-nerd-icons-palette 'nerd-icons) ;; Esquema de cores (nerd-icons = padrão)
  
  ;; Header: caminho à esquerda, espaço livre à direita
  (setq dirvish-header-line-format '(:left (path) :right (free-space)))
  
  ;; Mode-line: ordenação e hora à esquerda, ações à direita
  (setq dirvish-mode-line-format '(:left (sort file-time) :right (omit yank index)))
  
  ;; Preview automático para: imagens, vídeos, gifs, arquivos compactados, PDFs
  (setq dirvish-preview-dispatchers '(image video gif archive pdf))
  
  ;; Configurações adicionais de preview
  (setq dirvish-use-header-line t)        ;; Mostra header com informações
  (setq dirvish-use-mode-line t)          ;; Mostra mode-line customizada
  (setq delete-by-moving-to-trash t)      ;; Move para lixeira ao invés de deletar
  
  ;; Desabilita número de linhas no Dirvish (não faz sentido em gerenciador de arquivos)
  :hook
  ((dirvish-mode . (lambda () 
                     (display-line-numbers-mode -1)
                     (setq-local display-line-numbers nil)))
   (dired-mode . (lambda () 
                   (display-line-numbers-mode -1)
                   (setq-local display-line-numbers nil))))
  
  ;; Atalhos de teclado
  :bind
  (("C-x d" . dirvish)               ;; Substitui dired padrão (C-x d)
   :map dirvish-mode-map
   ("TAB"   . dirvish-subtree-toggle) ;; Expande/colapsa subdiretórios
   ("f"     . dirvish-file-info-menu) ;; Menu com informações detalhadas do arquivo
   ("y"     . dirvish-yank-menu)      ;; Menu de cópia (nome, caminho, etc)
   ("N"     . dirvish-narrow)         ;; Filtra arquivos (busca incremental)
   ("^"     . dirvish-history-last)   ;; Volta para diretório anterior
   ("h"     . dirvish-history-jump)   ;; Histórico de diretórios visitados
   ("s"     . dirvish-quicksort)      ;; Menu rápido de ordenação
   ("v"     . dirvish-vc-menu)        ;; Menu de controle de versão (git)
   ("?"     . dirvish-dispatch)       ;; Menu principal com todos comandos
   ("q"     . dirvish-quit)))         ;; Fecha o dirvish


;; =============================================================================
;; Desenvolvimento 
;; =============================================================================

;; Transient: Framework para interfaces de comando com menus pop-up
;; Fornece menus interativos com dicas de teclas e argumentos
;; Usado pelo Magit e outros pacotes para criar interfaces ricas
;; GitHub: https://github.com/magit/transient
(use-package transient
  :ensure t)

;; Magit: Interface Git completa e poderosa para Emacs
;; A melhor interface Git disponível, melhor que qualquer GUI
;; Permite staging parcial, rebase interativo, histórico visual, etc.
;; GitHub: https://github.com/magit/magit
(use-package magit
  :ensure t
  :after transient  ;; Magit depende do Transient
  :bind (("C-x g" . magit-status)       ;; Abre o status do Git (tela principal)
         ("C-x C-g" . magit-status)))   ;; Atalho alternativo

;; Forge: Integração com GitHub, GitLab e outros forges no Magit
;; Permite gerenciar issues, pull requests e notificações diretamente do Emacs
;; Funcionalidades: criar/fechar PRs, revisar código, comentar issues, etc.
;; Requer configuração de token de acesso (ver README do Forge)
;; GitHub: https://github.com/magit/forge
(use-package forge
  :after magit  ;; Forge estende o Magit
  :ensure t)

;; =============================================================================
;; HEADER 42 - Carrega o módulo do header da 42
;; =============================================================================

;; Carrega o arquivo header42.el
(load-file (expand-file-name "header42.el" user-emacs-directory))

;; Habilita o header 42 com atualização automática
(header-42-enable)

;; =============================================================================
;; ATIVAÇÃO DE MODOS ADICIONAIS
;; =============================================================================

;; Ativa destaque da linha atual
(global-hl-line-mode 1)

;; Mostra numero das linhas
(global-display-line-numbers-mode 1)

;; Mostra o número das colunas 
(column-number-mode 1)

;; highlight na linha corrente
(global-hl-line-mode 1)

;; Mostra match entre parenteses
(show-paren-mode 1)

;; Auto reverte arquivos alterados fora do Emacs 
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

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

;; =============================================================================
;; CONFIGURAÇÃO DE INDENTAÇÃO E TABS
;; =============================================================================

;; Usa espaços ao invés de tabs, com largura de 4 espaços (padrão 42)
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Exceção: Makefiles DEVEM usar tabs reais (requisito do Make)
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;; Exceção adicional para outros modos de Makefile
(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

(add-hook 'makefile-bsdmake-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;; =============================================================================
;; BUFFER INICIAL
;; =============================================================================

;; Muda para o buffer *scratch* e depois exibe o splash do Nano Emacs
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Primeiro muda para o *scratch*
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (switch-to-buffer "*scratch*")
                                   ;; Depois exibe o splash do Nano Emacs
                                   (run-with-idle-timer 0.2 nil
                                                        (lambda ()
                                                          (when (fboundp 'nano-splash)
                                                            (nano-splash))))))))

(provide 'init)
;;; init.el ends here
