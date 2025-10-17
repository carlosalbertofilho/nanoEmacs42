#!/bin/bash
# Script de utilitários para desenvolvimento C - 42 School
# Use: chmod +x c-utils.sh && ./c-utils.sh

# Cores para output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Função para criar projeto C básico
create_project() {
    local project_name=$1
    if [ -z "$project_name" ]; then
        echo -e "${RED}Uso: create_project <nome_do_projeto>${NC}"
        return 1
    fi
    
    echo -e "${BLUE}Criando projeto: $project_name${NC}"
    mkdir -p "$project_name"/{src,include,obj}
    
    # Cria main.c básico
    cat > "$project_name/src/main.c" << 'EOF'
#include <stdio.h>
#include <stdlib.h>

int	main(int argc, char **argv)
{
	printf("Hello, 42!\n");
	return (0);
}
EOF

    # Cria header básico
    cat > "$project_name/include/main.h" << 'EOF'
#ifndef MAIN_H
# define MAIN_H

# include <stdio.h>
# include <stdlib.h>

#endif
EOF

    # Copia Makefile template
    if [ -f ~/.config/emacs/Makefile.42 ]; then
        cp ~/.config/emacs/Makefile.42 "$project_name/Makefile"
        # Ajusta nome no Makefile
        sed -i.bak "s/NAME = programa/NAME = $project_name/" "$project_name/Makefile"
        sed -i.bak "s/SRCS = main.c/SRCS = src\/main.c/" "$project_name/Makefile"
        rm "$project_name/Makefile.bak"
    fi
    
    echo -e "${GREEN}Projeto $project_name criado com sucesso!${NC}"
    echo -e "${YELLOW}Estrutura:${NC}"
    tree "$project_name" 2>/dev/null || ls -la "$project_name"
}

# Função para compilação rápida
quick_compile() {
    local file=$1
    if [ -z "$file" ]; then
        echo -e "${RED}Uso: quick_compile <arquivo.c>${NC}"
        return 1
    fi
    
    local output="${file%.*}"
    echo -e "${YELLOW}Compilando: $file -> $output${NC}"
    cc -Wall -Wextra -Werror -std=c99 "$file" -o "$output"
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Compilação bem-sucedida!${NC}"
        echo -e "${BLUE}Execute com: ./$output${NC}"
    else
        echo -e "${RED}Erro na compilação!${NC}"
    fi
}

# Função para compilação com debug
debug_compile() {
    local file=$1
    if [ -z "$file" ]; then
        echo -e "${RED}Uso: debug_compile <arquivo.c>${NC}"
        return 1
    fi
    
    local output="${file%.*}_debug"
    echo -e "${YELLOW}Compilando com debug: $file -> $output${NC}"
    cc -Wall -Wextra -Werror -std=c99 -g -fsanitize=address "$file" -o "$output"
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Compilação debug bem-sucedida!${NC}"
        echo -e "${BLUE}Execute com: ./$output${NC}"
    else
        echo -e "${RED}Erro na compilação!${NC}"
    fi
}

# Função para verificar norminette
check_norm() {
    echo -e "${YELLOW}Verificando norminette...${NC}"
    if command -v norminette &> /dev/null; then
        norminette *.c *.h **/*.c **/*.h 2>/dev/null
        if [ $? -eq 0 ]; then
            echo -e "${GREEN}Norminette OK!${NC}"
        else
            echo -e "${RED}Erros de norminette encontrados!${NC}"
        fi
    else
        echo -e "${RED}Norminette não instalada!${NC}"
        echo -e "${YELLOW}Instale com: pip3 install norminette${NC}"
    fi
}

# Função para limpar arquivos temporários
clean_temp() {
    echo -e "${YELLOW}Limpando arquivos temporários...${NC}"
    find . -name "*.o" -delete 2>/dev/null
    find . -name "*.out" -delete 2>/dev/null
    find . -name "a.out" -delete 2>/dev/null
    find . -name "core" -delete 2>/dev/null
    find . -name "*~" -delete 2>/dev/null
    find . -name ".*~" -delete 2>/dev/null
    find . -name ".DS_Store" -delete 2>/dev/null
    echo -e "${GREEN}Limpeza concluída!${NC}"
}

# Função de ajuda
show_help() {
    echo -e "${BLUE}=== Utilitários para Desenvolvimento C - 42 School ===${NC}"
    echo ""
    echo -e "${YELLOW}Comandos disponíveis:${NC}"
    echo "  create_project <nome>  - Cria estrutura de projeto C"
    echo "  quick_compile <file.c> - Compilação rápida com flags 42"
    echo "  debug_compile <file.c> - Compilação com debug e sanitizer"
    echo "  check_norm            - Verifica norminette em todos arquivos"
    echo "  clean_temp            - Remove arquivos temporários"
    echo "  show_help            - Mostra esta ajuda"
    echo ""
    echo -e "${YELLOW}Exemplos:${NC}"
    echo "  ./c-utils.sh create_project libft"
    echo "  ./c-utils.sh quick_compile main.c"
    echo "  ./c-utils.sh debug_compile main.c"
    echo "  ./c-utils.sh check_norm"
    echo "  ./c-utils.sh clean_temp"
}

# Main
case "$1" in
    "create_project")
        create_project "$2"
        ;;
    "quick_compile")
        quick_compile "$2"
        ;;
    "debug_compile")
        debug_compile "$2"
        ;;
    "check_norm")
        check_norm
        ;;
    "clean_temp")
        clean_temp
        ;;
    "help"|"-h"|"--help"|"")
        show_help
        ;;
    *)
        echo -e "${RED}Comando desconhecido: $1${NC}"
        show_help
        exit 1
        ;;
esac