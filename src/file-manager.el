;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;

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

;; Dired-X: Extensões avançadas para o Dired (gerenciador de arquivos padrão)
;; Adiciona funcionalidades extras como omit-mode, guess-shell-command, etc.
;; Permite ocultar arquivos desnecessários e comandos inteligentes
;; NOTA: dired-x é built-in do Emacs, não precisa ser instalado via Elpaca
(use-package dired-x
  :ensure nil  ;; Não instalar via Elpaca (é built-in)
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
  
  ;; Configurações específicas do dirvish-side
  (setq dirvish-side-width 35)             ;; Largura da sidebar (35 colunas)
  (setq dirvish-side-auto-expand t)        ;; Expande automaticamente até arquivo atual
  (setq dirvish-side-follow-mode nil)      ;; Follow mode desabilitado por padrão
  
  ;; Header customizado para sidebar (mostra apenas o projeto)
  (setq dirvish-side-header-line-format '(:left (project)))
  
  ;; Atributos específicos da sidebar (menos informações para economizar espaço)
  (setq dirvish-side-attributes '(vc-state nerd-icons collapse file-size))
  
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
   ;; Atalhos globais para dirvish-side (sidebar como IDE)
   ("<f9>"    . dirvish-side)         ;; F9: Toggle sidebar (padrão, como VS Code/IntelliJ)
   ("C-c d s" . dirvish-side)         ;; C-c d s: Alternativa para toggle sidebar
   ("C-c d f" . dirvish-side-follow-mode) ;; C-c d f: Auto-seleciona arquivo atual na sidebar
   
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
   ("q"     . dirvish-quit)           ;; Fecha o dirvish
   
   ;; Atalhos específicos do dirvish-side quando ativo
   :map dirvish-mode-map
   ("+" . dirvish-side-increase-width) ;; Aumenta largura da sidebar
   ("-" . dirvish-side-decrease-width) ;; Diminui largura da sidebar
   ))         ;; Fecha o dirvish

;; TRAMP: Acesso transparente a arquivos remotos
;; Permite editar arquivos em servidores remotos via SSH, FTP, SCP, etc.
;; Integra perfeitamente com Dired/Dirvish para navegação remota
;; 
;; EXEMPLOS DE USO:
;; - Arquivo remoto via SSH: /ssh:user@servidor.com:/home/user/arquivo.txt
;; - Como root local: /sudo::/etc/hosts
;; - Como outro usuário: /su:username:/home/username/arquivo.txt
;; - Via SCP: /scp:user@servidor.com:/path/to/file
;; - Múltiplos hops: /ssh:user@gateway|ssh:user@servidor:/arquivo
;; 
;; ATALHOS ÚTEIS:
;; - C-x C-f: Abrir arquivo (use sintaxe TRAMP)
;; - C-x d: Dirvish em diretório remoto (/ssh:user@host:/path/)
;; - M-x tramp-cleanup-all-connections: Limpa todas conexões
;; 
;; GitHub: https://www.gnu.org/software/tramp/
(use-package tramp
  :ensure nil  ;; Não instalar via Elpaca (é built-in)
  :config
  ;; Habilita Dirvish completo sobre conexões TRAMP via SSH
  ;; Melhora performance de processos assíncronos remotos
  ;; Referência: https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  
  ;; Otimizações de performance para conexões remotas
  (setq tramp-verbose 0)                    ;; Reduz logs verbosos (0-10, 0=silencioso)
  (setq tramp-chunksize 2000)               ;; Tamanho dos chunks de transferência (bytes)
  (setq tramp-ssh-controlmaster-options nil) ;; Desabilita SSH ControlMaster (pode causar problemas)
  
  ;; Configurações adicionais para melhor experiência
  (setq tramp-default-method "ssh")         ;; Método padrão (ssh, scp, ftp, etc.)
  (setq tramp-auto-save-directory           ;; Diretório para auto-saves remotos
        (expand-file-name "tramp-auto-saves" user-emacs-directory))
  (setq tramp-persistency-file-name         ;; Cache de conexões persistentes
        (expand-file-name "tramp-connection-history" user-emacs-directory))
  
  ;; Cache de senhas por sessão (não salva no disco por segurança)
  (setq password-cache t)
  (setq password-cache-expiry 3600)         ;; Expira cache de senha após 1 hora
  
  ;; Melhora compatibilidade com diferentes shells remotos
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
  
  ;; Cria diretório de auto-saves se não existir
  (unless (file-exists-p tramp-auto-save-directory)
    (make-directory tramp-auto-save-directory t)))
