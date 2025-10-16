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

## 🐳 Uso com Docker

### Construindo a Imagem

```bash
# Clone o repositório
git clone https://github.com/carlosalbertofilho/nanoEmacs42.git
cd nanoEmacs42

# Construa a imagem Docker
docker build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t nanoemacs .
```

### Executando o Container

```bash
# Aplique a permissão de acesso ao X11
xhost +SI:localuser:$(id -un)

# Execute o container com compartilhamento de volumes
docker run -it --rm \
  --net=host \
  -e DISPLAY=:0 \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  -v $HOME/Projects:/home/$(whoami)/Projects \
  -v $HOME/.Xauthority:/home/$(whoami)/.Xauthority:ro \
  -v $HOME/.ssh:/home/$(whoami)/.ssh \
  --ipc=host \
  --privileged \
  -v /dev/shm:/dev/shm \
  --name nanoemacs \
  nanoemacs bash

# Dentro do container, execute o Emacs
emacs
```
