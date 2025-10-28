# Usa a imagem padrão do Debian
FROM debian:latest

# Define argumentos de build para o nome e ID do usuário
ARG UNAME
ARG UID

# Atualiza a lista de pacotes e instala as dependências
RUN apt-get update && \
    apt-get install -y \
    git \
    ripgrep \
    fd-find \
    fzf \
    build-essential \
    curl \
    coreutils \
    pandoc \
    python3 \
    python3-pip \
    clang \
    gdb \
    cmake \
    clangd \
    valgrind \
    sudo \
    wget \
    unzip \
    ispell \
    fontconfig \
    libgccjit-14-dev \
    emacs \
    gnupg \
    gnupg2 \
    gpg-agent \
    pinentry-curses \
    pinentry-gtk2 \
    dirmngr \
    && rm -rf /var/lib/apt/lists/*

# Cria um grupo e um usuário com o mesmo UID e GID do host
RUN groupadd -g $UID $UNAME
RUN useradd -u $UID -g $UID -m -s /bin/bash $UNAME
RUN echo "$UNAME ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Define o novo usuário como o usuário padrão
USER $UNAME

# Define o diretório de trabalho no home do usuário
WORKDIR /home/$UNAME

# Configura o GPG para o usuário
RUN mkdir -p ~/.gnupg && \
    chmod 700 ~/.gnupg && \
    echo "use-agent" > ~/.gnupg/gpg.conf && \
    echo "pinentry-mode loopback" >> ~/.gnupg/gpg.conf && \
    echo "allow-loopback-pinentry" > ~/.gnupg/gpg-agent.conf && \
    chmod 600 ~/.gnupg/gpg.conf ~/.gnupg/gpg-agent.conf

# Baixa a chave GPG especificada (com retry em caso de falha)
RUN for i in 1 2 3; do \
        gpg --keyserver hkps://keyserver.ubuntu.com --recv-keys 71D78F6E8C7170B2 && break || \
        gpg --keyserver hkps://keys.openpgp.org --recv-keys 71D78F6E8C7170B2 && break || \
        sleep 5; \
    done || echo "Warning: Could not download GPG key 71D78F6E8C7170B2"

# Instala a norminette da 42
RUN python3 -m pip install -U norminette --break-system-packages

# Cria o diretório de configuração do Emacs
RUN mkdir -p ~/.config/emacs

# Copia os arquivos de configuração do Emacs do diretório local
COPY --chown=$UNAME:$UNAME . /home/$UNAME/.config/emacs/


# Instala as fontes Nerd Fonts (JetBrains Mono e Fira Code)
RUN mkdir -p ~/.local/share/fonts && \
    cd ~/.local/share/fonts && \
    wget https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip && \
    wget https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.zip && \
    unzip -o JetBrainsMono.zip && \
    unzip -o FiraCode.zip && \
    rm JetBrainsMono.zip FiraCode.zip && \
    fc-cache -fv

# Define variáveis de ambiente
ENV TERM=xterm-256color

# Inicializa o Emacs para baixar e instalar pacotes automaticamente com Elpaca
RUN emacs --batch --eval="(progn (load-file \"~/.config/emacs/init.el\") (kill-emacs))" || true