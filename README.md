# 🚀 nanoEmacs 42 - Configuração Minimalista do Emacs para a 42 Rio

Uma configuração moderna e minimalista do Emacs inspirada no projeto [N Λ N O](https://github.com/rougier/nano-emacs), otimizada para desenvolvimento e uso em containers Docker.


## 🎯 Início Rápido

```bash
# 1. Limpe configurações antigas (se existir)
rm -fr ~/.emacs.d ~/.config/emacs

# 2. Clone o repositório
git clone https://github.com/carlosalbertofilho/nanoEmacs42.git ~/.emacs.d

# 2. Inicie o Emacs
emacs

# 3. Aguarde a instalação automática dos pacotes (primeira vez)
# ⏱️ Isso pode levar 15-30 segundos

# 4. Pronto! Nas próximas vezes iniciará em ~1 segundo
```

**Ou use com Docker** (veja seção [🐳 Uso com Docker](#-uso-com-docker))

## 📚 Documentação Completa

Este projeto inclui documentação detalhada em português:

| Arquivo | Descrição |
|---------|-----------|
| **[README.md](./README.md)** | 📖 Visão geral e início rápido (você está aqui) |
| **[ELPACA.md](./ELPACA.md)** | 📦 Guia completo do Elpaca (gerenciador de pacotes) |

## 🐳 Uso com Docker

### Construindo a Imagem

```bash
# Clone o repositório
git clone https://github.com/carlosalbertofilho/nanoEmacs42.git
cd nanoEmacs42

# Com Podman
podman build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t nanoemacs .

# Com Docker
docker build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t nanoemacs .
```

### Executando o Container
Com podman
```bash
podman run -it --rm \
  --net=host \
  --userns=keep-id \
  --user "$(id -u)":"$(id -g)" \
  --security-opt label=disable \
  -e OPENAI_API_KEY="$OPENAI_API_KEY" \
  -e HOME="/home/$(id -un)" \
  -v "$HOME/Documents":/home/"$(id -un)"/Documents:rw \
  -v "$HOME/UERJ":/home/"$(id -un)"/UERJ:rw \
  -v "$HOME/Projects":/home/"$(id -un)"/Projects:rw \
  -v "$HOME/.gitconfig":/home/"$(id -un)"/.gitconfig:ro \
  -v "$HOME/.ssh":/home/"$(id -un)"/.ssh:ro \
  -v /dev/shm:/dev/shm \
  --workdir /home/"$(id -un)"/Projects \
  --name nanoemacs \
  nanoemacs bash

```

Com docker:
```bash
# Execute o container com compartilhamento de volumes
docker run -it --rm \
  --net=host \
  -e OPENAI_API_KEY=$OPENAI_API_KEY \
  -v "$HOME/Documents":/home/"$(whoami)"/Documents:z \
  -v "$HOME/UERJ":/home/"$(whoami)"/UERJ:z \
  -v "$HOME/Projects":/home/"$(whoami)"/Projects:z \
  -v "$HOME/.gitconfig":/home/"$(whoami)"/.gitconfig:ro,z \
  -v "$HOME/.ssh":/home/"$(whoami)"/.ssh:z \
  -v /dev/shm:/dev/shm \
  --name nanoemacs \
  nanoemacs bash
# Dentro do container, execute o Emacs
emacs
```
