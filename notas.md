# Melhorar o setup-user

Instale o startship

instale o modulo do pip e o pipx

garanta que o nominette está ok

└[~/Documents/nanoEmacs42]> id
uid=102068(csilva-d) gid=4225(2025_rio-de-janeiro) groups=4225(2025_rio-de-janeiro)


 docker build --build-arg USER_UID=102068 --build-arg GROUP_GID=4225 --build-arg USER_NAME=csilva-d --build-arg GROUP_NAME=2025_rio-de-janeiro -t nanoemacs 

```shell
#!/bin/bash
set -e

# Configuration
SETUP_USER="${SETUP_USER:-developer}"
SETUP_UID="${SETUP_UID:-1000}"
SETUP_GID="${SETUP_GID:-$SETUP_UID}"
SETUP_HOME="${SETUP_HOME:-/home/$SETUP_USER}"

# Validation
if [[ ! "$SETUP_UID" =~ ^[0-9]+$ ]] || [ "$SETUP_UID" -lt 1000 ]; then
    echo "Error: Invalid UID. Must be numeric and >= 1000"
    exit 1
fi

echo "=== User Setup ==="
echo "User: $SETUP_USER"
echo "UID:  $SETUP_UID"
echo "GID:  $SETUP_GID"
echo "Home: $SETUP_HOME"
echo "=================="

# Create group if it doesn't exist
if ! getent group "$SETUP_GID" >/dev/null 2>&1; then
    echo "Creating group $SETUP_USER ($SETUP_GID)"
    groupadd -g "$SETUP_GID" "$SETUP_USER"
fi

# Create user if it doesn't exist
if ! getent passwd "$SETUP_UID" >/dev/null 2>&1; then
    echo "Creating user $SETUP_USER ($SETUP_UID)"
    useradd -u "$SETUP_UID" -g "$SETUP_GID" -m -d "$SETUP_HOME" -s /bin/bash "$SETUP_USER"
    echo "User $SETUP_USER created successfully"
else
    echo "User with UID $SETUP_UID already exists"
fi

# Ensure proper ownership
if [ -d "$SETUP_HOME" ]; then
    chown -R "$SETUP_UID:$SETUP_GID" "$SETUP_HOME"
fi

echo "User setup completed!"
```

Env nos Dockerfile

```dockerfile
# No seu Dockerfile, adicione:
ENV SETUP_USER=developer
ENV SETUP_UID=1000

# Ou use ARG para permitir customização durante o build:
ARG SETUP_USER=developer
ARG SETUP_UID=1000
ENV SETUP_USER=$SETUP_USER
ENV SETUP_UID=$SETUP_UID
```

Uso no docker build

```shell
# Build com valores customizados
docker build --build-arg SETUP_USER=myuser --build-arg SETUP_UID=1001 .

# Ou run com environment variables
docker run -e SETUP_USER=myuser -e SETUP_UID=1001 your-image
```