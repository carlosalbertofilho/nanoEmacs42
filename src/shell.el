;;; init.el --- Configuração principal do Nano Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;


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
  (setq tramp-verbose 0)        ;; Reduz logs verbosos (0-10, 0=silencioso)
  (setq tramp-chunksize 2000)   ;; Tamanho dos chunks de transferência (bytes)
  (setq tramp-ssh-controlmaster-options nil) ;; Desabilita SSH ControlMaster (pode causar problemas)
  
  ;; Configurações adicionais para melhor experiência
  (setq tramp-default-method "ssh")    ;; Método padrão (ssh, scp, ftp, etc.)
  (setq tramp-auto-save-directory      ;; Diretório para auto-saves remotos
        (expand-file-name "tramp-auto-saves" user-emacs-directory))
  (setq tramp-persistency-file-name    ;; Cache de conexões persistentes
        (expand-file-name "tramp-connection-history" user-emacs-directory))
  
  ;; Cache de senhas por sessão (não salva no disco por segurança)
  (setq password-cache t)
  (setq password-cache-expiry 3600)    ;; Expira cache de senha após 1 hora
  
  ;; Melhora compatibilidade com diferentes shells remotos
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
  
  ;; Cria diretório de auto-saves se não existir
  (unless (file-exists-p tramp-auto-save-directory)
    (make-directory tramp-auto-save-directory t)))


;; eshell: Terminal interno do Emacs com aliases personalizados
;; Integração leve com o ambiente 42 para compilar e testar programas em C.
;; Doc: https://www.gnu.org/software/emacs/manual/html_node/eshell/

(use-package eshell
  :ensure nil  ;; Não instalar via Elpaca (é built-in)
  :hook (eshell-first-time-mode . my/eshell-setup)
  :config
  ;; ------------------------------
  ;; Função de inicialização
  ;; ------------------------------
  (defun my/eshell-setup ()
    "Configuração personalizada para Eshell."
    ;; Diretório padrão (ajuste se quiser)
    (setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
    ;; Histórico maior
    (setq eshell-history-size 5000
          eshell-hist-ignoredups t
          eshell-scroll-to-bottom-on-input t)

    ;; ------------------------------
    ;; ALIASES para projetos 42 (C)
    ;; ------------------------------
    ;; Compilar com flags padrão da 42
    (eshell/alias "cc42"
                  "cc -Wall -Wextra -Werror $* -o a.out")
    ;; Rodar programa
    (eshell/alias "r42"
                  "./a.out")
    ;; Limpar binário
    (eshell/alias "clean42"
                  "rm -f a.out")
    ;; Compilar e rodar direto
    (eshell/alias "cr42"
                  "cc42 $* && ./a.out")
    ;; Compilar com valgrind
    (eshell/alias "val42"
                  "cc42 $* && valgrind --leak-check=full ./a.out")
    ;; Norminette local (caso tenha instalada via pip)
    (when (executable-find "norminette")
      (eshell/alias "norm42"
                    "norminette $*")))

  ;; ------------------------------
  ;; Funções úteis dentro do Eshell
  ;; ------------------------------
  ;; Abrir arquivo atual no Emacs
  (defun eshell/emacs (file)
    "Abrir FILE no buffer atual do Emacs."
    (find-file file))

  ;; Alternar para diretório do projeto atual
  (defun eshell/cdproj ()
    "Ir para o diretório do projeto detectado pelo Projectile."
    (when (fboundp 'projectile-project-root)
      (eshell/cd (projectile-project-root))))
  )

;; Atalho rápido para abrir o Eshell
(global-set-key (kbd "C-x t e") #'eshell)
;; Atalhos locais do eshell
(with-eval-after-load 'eshell
  (define-key eshell-mode-map (kbd "C-l") #'eshell/clear)
  (define-key eshell-mode-map (kbd "C-c C-k") #'eshell-kill-input))
