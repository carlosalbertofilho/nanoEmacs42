;;; header42.el --- Header padrão da 42 School -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carlos Filho
;;
;; Author: Carlos Filho
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: 42, header, convenience
;;
;; Este arquivo implementa o header padrão da 42 School com atualização automática.

;;; Commentary:
;;
;; Fornece funções para inserir e atualizar automaticamente o header padrão
;; da 42 School em arquivos de código.
;;
;; Uso:
;;   - Pressione F1 para inserir/atualizar o header
;;   - O header é atualizado automaticamente ao salvar o arquivo
;;
;; Configuração:
;;   - Define FT_LOGIN ou USER no ambiente para o nome do usuário

;;; Code:

;; =============================================================================
;; CONSTANTES
;; =============================================================================

(defconst header-42-ascii-art
  '("       :::      ::::::::"
    "     :+:      :+:    :+:"
    "   +:+ +:+         +:+  "
    " +#+  +:+       +#+     "
    "+#+#+#+#+#+   +#+        "
    "    #+#    #+#          "
    "   ###   ########.fr    ")
  "ASCII art do logo 42.")

(defconst header-42-length 80
  "Comprimento total de cada linha do header.")

(defconst header-42-margin 5
  "Margem lateral do header.")

;; =============================================================================
;; FUNÇÕES AUXILIARES
;; =============================================================================

(defun header-42-get-user ()
  "Retorna o nome do usuário (FT_LOGIN, USER ou fallback)."
  (or (getenv "FT_LOGIN")
      (getenv "USER")
      "marvin"))

(defun header-42-get-mail ()
  "Retorna o email do usuário."
  (let ((user (header-42-get-user)))
    (format "%s@student.42.fr" user)))

(defun header-42-get-filename ()
  "Retorna o nome do arquivo ou '< new >' se não existir."
  (or (file-name-nondirectory (buffer-file-name))
      "< new >"))

(defun header-42-get-date ()
  "Retorna a data/hora atual no formato do header 42."
  (format-time-string "%Y/%m/%d %H:%M:%S"))

(defun header-42-make-line (left right)
  "Cria uma linha do header com LEFT à esquerda e RIGHT à direita."
  (let* ((left-str (substring left 0 (min (length left)
                                           (- header-42-length
                                              (* 2 header-42-margin)
                                              (length right)))))
         (right-str right)
         (spaces-needed (- header-42-length
                          (* 2 header-42-margin)
                          (length left-str)
                          (length right-str)))
         (spaces (if (< spaces-needed 0) 0 spaces-needed))
         (start-margin (make-string (- header-42-margin 2) ?\s))
         (end-margin (make-string (- header-42-margin 2) ?\s)))
    (format "/*%s%s%s%s%s*/"
            start-margin
            left-str
            (make-string spaces ?\s)
            right-str
            end-margin)))

(defun header-42-get-ascii (line-number)
  "Retorna a linha LINE-NUMBER do ASCII art (1-indexed)."
  (if (and (>= line-number 1) (<= line-number 7))
      (nth (- line-number 1) header-42-ascii-art)
    ""))

(defun header-42-generate-line (n)
  "Gera a linha N do header (1-11)."
  (cond
   ;; Linha 1: borda superior
   ((= n 1)
    (format "/* %s */"
            (make-string (- header-42-length 6) ?*)))
   
   ;; Linha 2: linha vazia
   ((= n 2)
    (header-42-make-line "" ""))
   
   ;; Linha 3: ASCII art vazio
   ((= n 3)
    (header-42-make-line "" (header-42-get-ascii 1)))
   
   ;; Linha 4: nome do arquivo
   ((= n 4)
    (header-42-make-line (header-42-get-filename) (header-42-get-ascii 2)))
   
   ;; Linha 5: ASCII art vazio
   ((= n 5)
    (header-42-make-line "" (header-42-get-ascii 3)))
   
   ;; Linha 6: autor
   ((= n 6)
    (header-42-make-line (format "By: %s <%s>"
                                 (header-42-get-user)
                                 (header-42-get-mail))
                        (header-42-get-ascii 4)))
   
   ;; Linha 7: ASCII art vazio
   ((= n 7)
    (header-42-make-line "" (header-42-get-ascii 5)))
   
   ;; Linha 8: Created
   ((= n 8)
    (header-42-make-line (format "Created: %s by %s"
                                 (header-42-get-date)
                                 (header-42-get-user))
                        (header-42-get-ascii 6)))
   
   ;; Linha 9: Updated
   ((= n 9)
    (header-42-make-line (format "Updated: %s by %s"
                                 (header-42-get-date)
                                 (header-42-get-user))
                        (header-42-get-ascii 7)))
   
   ;; Linha 10: linha vazia
   ((= n 10)
    (header-42-make-line "" ""))
   
   ;; Linha 11: borda inferior
   ((= n 11)
    (format "/* %s */"
            (make-string (- header-42-length 6) ?*)))))

;; =============================================================================
;; FUNÇÕES PRINCIPAIS
;; =============================================================================

(defun header-42-insert ()
  "Insere o header 42 no início do arquivo."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Insere linha vazia após o header
    (insert "\n")
    ;; Insere as 11 linhas do header de baixo para cima
    (let ((line 11))
      (while (> line 0)
        (goto-char (point-min))
        (insert (header-42-generate-line line) "\n")
        (setq line (1- line))))))

(defun header-42-update ()
  "Atualiza as linhas 'Updated:' e filename do header 42 se existir."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Verifica se existe um header 42 (procura por "Updated:" na linha 9)
    (when (and (>= (count-lines (point-min) (point-max)) 11)
               (progn
                 (goto-char (point-min))
                 (forward-line 8)
                 (beginning-of-line)
                 (looking-at "^/\\*   Updated: ")))
      ;; Atualiza a linha 9 (Updated)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (header-42-generate-line 9))
      ;; Atualiza a linha 4 (filename) - caso o arquivo tenha sido renomeado
      (goto-char (point-min))
      (forward-line 3)
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (header-42-generate-line 4)))))

;;;###autoload
(defun stdheader ()
  "Insere ou atualiza o header 42."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (>= (count-lines (point-min) (point-max)) 11)
             (progn
               (goto-char (point-min))
               (forward-line 8)
               (beginning-of-line)
               (looking-at "^/\\*   Updated: ")))
        (header-42-update)
      (header-42-insert))))

;; =============================================================================
;; CONFIGURAÇÃO DE HOOKS E ATALHOS
;; =============================================================================

;;;###autoload
(defun header-42-enable ()
  "Habilita o header 42 com atualização automática."
  (interactive)
  ;; Hook para atualizar automaticamente ao salvar
  (add-hook 'before-save-hook 'header-42-update)
  ;; Atalho F5 para inserir/atualizar header usando general.el
  (when (fboundp 'general-define-key)
    (general-define-key
     "<f5>" 'stdheader))
  ;; Fallback para versões sem general.el
  (unless (fboundp 'general-define-key)
    (global-set-key (kbd "<f5>") 'stdheader))
  (message "Header 42 habilitado! Use F5 para inserir/atualizar."))

;;;###autoload
(defun header-42-disable ()
  "Desabilita o header 42."
  (interactive)
  (remove-hook 'before-save-hook 'header-42-update)
  ;; Remove atalho usando general.el ou fallback
  (when (fboundp 'general-define-key)
    (general-define-key
     "<f5>" nil))
  (unless (fboundp 'general-define-key)
    (global-set-key (kbd "<f5>") nil))
  (message "Header 42 desabilitado."))

(provide 'header42)
;;; header42.el ends here
