# 📦 Guia Completo do Elpaca no nanoEmacs

Este documento detalha como o Elpaca está configurado neste projeto e como usá-lo efetivamente.

## 📋 Índice

- [O que é Elpaca?](#o-que-é-elpaca)
- [Por que Elpaca?](#por-que-elpaca)
- [Como Funciona](#como-funciona)
- [Configuração no Projeto](#configuração-no-projeto)
- [Usando o Elpaca](#usando-o-elpaca)
- [Comandos Úteis](#comandos-úteis)
- [Troubleshooting](#troubleshooting)
- [Exemplos Práticos](#exemplos-práticos)

## O que é Elpaca?

[Elpaca](https://github.com/progfolio/elpaca) é um gerenciador de pacotes moderno para Emacs que oferece:

- ⚡ **Instalação Assíncrona**: Downloads e compilações em paralelo
- 🎯 **Integração use-package**: Sintaxe declarativa e limpa
- 🔧 **Flexibilidade**: Suporta múltiplas fontes (ELPA, MELPA, GitHub, etc.)
- 📦 **Controle Total**: Builds customizadas, branches específicos
- 🚀 **Moderno**: Desenvolvido com práticas atuais do Emacs

## Por que Elpaca?

### Comparação com outros gerenciadores:

| Recurso | package.el | straight.el | Elpaca |
|---------|-----------|-------------|--------|
| Assíncrono | ❌ | ❌ | ✅ |
| use-package | ⚠️ Parcial | ✅ | ✅ |
| Múltiplas fontes | ⚠️ Limitado | ✅ | ✅ |
| Performance | 🐌 Lento | 🏃 Médio | 🚀 Rápido |
| Manutenção | ✅ Oficial | ✅ Ativo | ✅ Ativo |
| Complexidade | 😊 Simples | 😐 Médio | 😊 Simples |

### Vantagens para este projeto:

1. **Velocidade**: ~30 pacotes instalados em 15-30s (primeira vez)
2. **Confiabilidade**: Instalação assíncrona robusta
3. **Manutenção**: Integração nativa com use-package
4. **Flexibilidade**: Fácil instalar de GitHub/GitLab

## Como Funciona

### Fluxo de Instalação

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Emacs inicia                                             │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│ 2. early-init.el executa                                    │
│    - Desabilita package.el                                  │
│    - Otimiza GC e file-handlers                             │
│    - Cria diretórios do Elpaca                              │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│ 3. init.el - Bootstrap do Elpaca                            │
│    - Verifica se Elpaca existe                              │
│    - Se não: clona repositório do GitHub                    │
│    - Compila Elpaca                                         │
│    - Carrega sistema Elpaca                                 │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│ 4. Configura use-package                                    │
│    (elpaca-require-package 'use-package)                    │
│    (elpaca-use-package-mode)                                │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│ 5. Processa declarações use-package                         │
│    - Lê todas as declarações (use-package ...)              │
│    - Adiciona à fila de instalação                          │
│    - Instala assincronamente em paralelo                    │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│ 6. (elpaca-wait)                                            │
│    - Aguarda todas as instalações terminarem                │
│    - Bloqueia até conclusão                                 │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│ 7. Configuração pós-instalação                              │
│    - (require 'pacote) e configurações                      │
│    - Emacs pronto para uso!                                 │
└─────────────────────────────────────────────────────────────┘
```

### Estrutura de Diretórios

```
~/.emacs.d/
└── elpaca/
    ├── repos/              # Repositórios git dos pacotes
    │   ├── elpaca/        # Código fonte do Elpaca
    │   ├── use-package/   # Código fonte do use-package
    │   ├── magit/         # Exemplo: código do magit
    │   └── ...
    └── builds/            # Versões compiladas
        ├── elpaca/        # Elpaca compilado (.elc)
        ├── use-package/
        ├── magit/
        └── ...
```

## Configuração no Projeto

### 1. early-init.el

```elisp
;; Desabilita package.el
(setq package-enable-at-startup nil)

;; Prepara diretórios do Elpaca
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Cria diretórios se não existirem
(unless (file-exists-p elpaca-directory)
  (make-directory elpaca-directory t))
```

### 2. init.el - Bootstrap (linhas 23-58)

```elisp
;; Define versão e diretórios
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Ordem de instalação do Elpaca
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref "develop"
                              :depth 1
                              :files (:defaults "elpaca-test.el" ...)
                              :build (:not elpaca--activate-package)))

;; Lógica de bootstrap (verifica, clona, compila, carrega)
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       ...)
  ;; ... código de instalação automática
  (require 'elpaca))

;; Adiciona grupos de receitas
(elpaca-add-recipe-group 'stable)
(elpaca-add-recipe-group 'unstable)
(elpaca-add-recipe-group 'develop)
```

### 3. Integração use-package (linhas 59-66)

```elisp
;; Instala use-package via Elpaca
(elpaca-require-package 'use-package)
(require 'use-package)

;; Configurações
(setq use-package-always-ensure t)       ; Sempre instalar pacotes
(setq use-package-expand-minimally t)    ; Expansão mínima (performance)

;; Ativa modo Elpaca para use-package
(elpaca-use-package-mode)

;; Processa fila inicial
(elpaca-process-queues)
```

## Usando o Elpaca

### Sintaxe Básica com use-package

#### 1. Pacote do MELPA/ELPA

```elisp
(use-package nome-do-pacote
  :ensure t
  :config
  (sua-configuração))
```

#### 2. Pacote do GitHub

```elisp
(use-package nome-do-pacote
  :ensure (:host github :repo "usuario/repositorio")
  :config
  (sua-configuração))
```

#### 3. Pacote do GitLab

```elisp
(use-package nome-do-pacote
  :ensure (:host gitlab :repo "usuario/repositorio")
  :config
  (sua-configuração))
```

#### 4. Branch ou Tag Específico

```elisp
(use-package nome-do-pacote
  :ensure (:host github 
           :repo "usuario/repo" 
           :branch "develop")
  :config
  (sua-configuração))

;; Ou com tag
(use-package nome-do-pacote
  :ensure (:host github 
           :repo "usuario/repo" 
           :tag "v1.2.3"))
```

#### 5. URL Direta

```elisp
(use-package nome-do-pacote
  :ensure (:repo "https://git.exemplo.com/usuario/repo.git")
  :config
  (sua-configuração))
```

#### 6. Arquivos Específicos

```elisp
(use-package nome-do-pacote
  :ensure (:host github 
           :repo "usuario/repo"
           :files ("*.el" "src/*.el" (:exclude "tests/*")))
  :config
  (sua-configuração))
```

### Aguardando Instalação

```elisp
;; Instalar múltiplos pacotes
(use-package pacote-1 :ensure t)
(use-package pacote-2 :ensure t)
(use-package pacote-3 :ensure t)

;; Aguardar conclusão antes de continuar
(elpaca-wait)

;; Agora pode usar os pacotes
(require 'pacote-1)
(pacote-1-enable)
```

## Comandos Úteis

### Gerenciamento de Pacotes

```elisp
;; Ver status de todos os pacotes
M-x elpaca-status

;; Ver logs detalhados
M-x elpaca-log

;; Buscar atualizações
M-x elpaca-fetch-all

;; Atualizar todos os pacotes
M-x elpaca-pull-all

;; Recompilar pacote específico
M-x elpaca-rebuild RET nome-do-pacote

;; Recompilar todos
M-x elpaca-rebuild-all

;; Remover pacote
M-x elpaca-delete RET nome-do-pacote

;; Testar pacote temporariamente (não persiste)
M-x elpaca-try RET nome-do-pacote
```

### Buffer de Status

Ao executar `M-x elpaca-status`, você verá:

```
Package Name          Status      Version    Source
─────────────────────────────────────────────────────
elpaca               ✓ Built     develop    GitHub
use-package          ✓ Built     2.4.5      GNU ELPA
magit                ✓ Built     3.3.0      MELPA
evil                 ⟳ Building  1.15.0     MELPA
denote               ✗ Failed    1.2.0      GitHub
```

**Legendas:**
- ✓ Built: Compilado com sucesso
- ⟳ Building: Em processo de compilação
- ↓ Cloning: Fazendo clone do repositório
- ✗ Failed: Falha (verifique logs)

## Troubleshooting

### Problema 1: Pacote não instala

**Sintomas:**
```
Error: Failed to clone package: <nome-do-pacote>
```

**Soluções:**
1. Verifique conexão com internet
2. Tente recompilar: `M-x elpaca-rebuild RET nome-do-pacote`
3. Verifique logs: `M-x elpaca-log`
4. Delete e reinstale:
   ```elisp
   M-x elpaca-delete RET nome-do-pacote
   ;; Reinicie o Emacs
   ```

### Problema 2: Erro de compilação

**Sintomas:**
```
Warning: Compiling file.el failed
```

**Soluções:**
1. Limpe o build: `M-x elpaca-rebuild RET nome-do-pacote`
2. Verifique dependências (podem precisar ser instaladas primeiro)
3. Limpe cache completo:
   ```bash
   rm -rf ~/.emacs.d/elpaca/builds/nome-do-pacote
   ```

### Problema 3: Emacs trava na inicialização

**Sintomas:**
- Emacs congela em "Loading packages..."

**Soluções:**
1. Inicie com `emacs -Q` (sem configuração)
2. Verifique se há `(elpaca-wait)` sem fim
3. Comente temporariamente pacotes problemáticos
4. Use `emacs --debug-init` para ver onde trava

### Problema 4: Versão antiga do pacote

**Sintomas:**
- Funções/features não disponíveis

**Soluções:**
```elisp
;; 1. Buscar atualizações
M-x elpaca-fetch-all

;; 2. Ver o que mudou
M-x elpaca-log

;; 3. Atualizar (merge)
M-x elpaca-pull-all

;; 4. Recompilar se necessário
M-x elpaca-rebuild RET nome-do-pacote
```

### Problema 5: Reset completo

**Quando tudo mais falhar:**

```bash
# 1. Feche o Emacs

# 2. Remova diretório do Elpaca
rm -rf ~/.emacs.d/elpaca/

# 3. Opcionalmente, limpe cache de native compilation
rm -rf ~/.emacs.d/eln-cache/

# 4. Reinicie o Emacs
# Tudo será reinstalado do zero
```

## Exemplos Práticos

### Exemplo 1: Adicionar Theme do GitHub

```elisp
(use-package doom-themes
  :ensure (:host github :repo "doomemacs/themes")
  :config
  (load-theme 'doom-one t))

(elpaca-wait)
```

### Exemplo 2: Pacote com Dependências

```elisp
;; Dependências primeiro
(use-package s :ensure t)         ; String manipulation
(use-package dash :ensure t)      ; List manipulation

;; Depois o pacote principal
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(elpaca-wait)
```

### Exemplo 3: Fork Pessoal

```elisp
;; Instalar do seu fork ao invés do repositório oficial
(use-package meu-pacote
  :ensure (:host github 
           :repo "meu-usuario/meu-pacote"
           :branch "my-custom-features")
  :config
  (meu-pacote-enable))
```

### Exemplo 4: Pacote Local (desenvolvimento)

```elisp
(use-package meu-pacote-local
  :ensure (:repo "~/projetos/meu-pacote-local/"
           :files ("*.el"))
  :config
  (require 'meu-pacote-local))
```

### Exemplo 5: Condicionalmente Instalar

```elisp
;; Só instalar no Linux
(use-package linux-specific-package
  :ensure t
  :if (eq system-type 'gnu/linux)
  :config
  (linux-specific-setup))

;; Só instalar no macOS
(use-package mac-specific-package
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (mac-specific-setup))
```

### Exemplo 6: Pacote com Build Customizado

```elisp
(use-package vterm
  :ensure (:host github 
           :repo "akermu/emacs-libvterm"
           :files ("*.el" "*.so" "*.h" 
                   (:exclude "*test*"))
           :build (:not autoload))
  :config
  (setq vterm-max-scrollback 10000))
```

## Recursos Adicionais

### Documentação Oficial
- [Elpaca GitHub](https://github.com/progfolio/elpaca)
- [Elpaca Manual](https://github.com/progfolio/elpaca/blob/master/doc/manual.md)
- [use-package GitHub](https://github.com/jwiegley/use-package)

### Comunidade
- [r/emacs](https://reddit.com/r/emacs)
- [Emacs StackExchange](https://emacs.stackexchange.com/)
- [Emacs Discord](https://discord.gg/emacs)

### Vídeos e Tutoriais
- [System Crafters - Elpaca Playlist](https://youtube.com/systemcrafters)
- [Protesilaos Stavrou - Package Management](https://protesilaos.com/)

---

**💡 Dica Final:** O Elpaca é configurado para "just work". Na maioria dos casos, você apenas adiciona `(use-package nome :ensure t)` e tudo funciona automaticamente!

*Última atualização: Outubro 2025*
