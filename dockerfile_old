FROM carlosalbertofilho/nanoemacs:20251018

USER user

# Set timezone
ENV TZ="America/Sao_Paulo"

# Definir variáveis de ambiente com valores padrão
ENV USER_NAME=csilva-d
ENV USER_UID=102068
ENV GROUP_NAME=2025_rio-de-janeiro
ENV GROUP_GID=4225

# Criar grupo e usuário com IDs das variáveis de ambiente
RUN sudo groupadd --gid ${GROUP_GID} ${GROUP_NAME} && \
    sudo useradd --uid ${USER_UID} --gid ${GROUP_GID} --create-home --shell /bin/bash ${USER_NAME} && \
    sudo passwd -d ${USER_NAME} && \
    echo "${USER_NAME} ALL=(ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers

# Update font cache
RUN sudo fc-cache -fv

# Update library cache
RUN sudo ldconfig

# Set environment variables
ENV TERM=xterm-256color
ENV PATH="/usr/local/bin:$HOME/.local/bin:$PATH"

# Criar diretório de configuração do emacs ANTES de mudar o usuário
# RUN mkdir -p "/home/${USER_NAME}/.config/emacs" && \
#     chown -R ${USER_UID}:${GROUP_GID} "/home/${USER_NAME}/.config" && \
#     chown -R ${USER_UID}:${GROUP_GID} "/home/${USER_NAME}"

# Switch to user only at the very end
#USER ${USER_NAME}
WORKDIR /home/${USER_NAME}

# # Copy emacs config files
# COPY --chown=${USER_UID}:${GROUP_GID} . /${HOME}/.config/emacs/


# # Inicializa o Emacs para baixar e instalar pacotes automaticamente com Elpaca
# RUN emacs --batch --eval="(progn (load-file \"~/.config/emacs/init.el\") (kill-emacs))" || true


CMD ["/bin/bash"]