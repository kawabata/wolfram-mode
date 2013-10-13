;;; math++.el --- Mathematica editing and inferior mode.  -*- lexical-binding: t -*-

;; Filename: math++.el
;; Description: Mathematica editing and inferior Mode
;; Author: Daichi Mochihashi <daichi at cslab.kecl.ntt.co.jp>
;; Modified by: Taichi Kawabata <kawabata.taichi_at_gmail.com>
;; Created: 2009-07-08
;; Modified: 2013-10-13
;; Keywords: languages, processes, tools
;; Namespace: math-
;; URL: https://github.com/kawabata/emacs-math-mode/

;; math.el, A Mathematica interface for GNU Emacs
;; based on math.el, mma.el, mathematica.el.
;; $Id: math++.el,v 1.3 2009/07/08 04:16:17 daichi Exp $
;;
;; Copyright (C) 2009 Daichi Mochihashi <daichi at cslab.kecl.ntt.co.jp>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mathematica is (C) Copyright 1988-2013 Wolfram Research, Inc.
;;
;; Protected by copyright law and international treaties.
;;
;; Unauthorized reproduction or distribution subject to severe civil
;; and criminal penalties.
;;
;; Mathematica is a registered trademark of Wolfram Research.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; - math-imenu-generic-expression
;; - sending useful commands to comint buffer.

;;; Commentary:

;; This code is modified version of `math++.el'
;; (http://chasen.org/~daiti-m/dist/math++.el).

;; You should add the followings to `~/.emacs.d/init.el'.

;;  (autoload 'math-mode "math++" nil t)
;;  (autoload 'run-math "math++" nil t)
;;  (setq math-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
;;  (add-to-list 'auto-mode-alist '("\\.m$" . math-mode))

;;; Change Log:

;; 2013-10-01  Kawabata Taichi <kawabata.taichi_at_gmail.com>
;;         * Modified to work with Emacs 24.3.
;;         * Remove duplicate functions, undefined function calls and compiler warnings.
;; 2013-10-07
;;         * Change `math-process-string' to `math-program'
;;         * Change `scheme-args-to-list' to `split-string-and-unquote'
;; 2013-10-12
;;         * Change `setq/make-local-variable' to `setq-local'
;;         * Change `(* ... *)' comment to be nestable.
;;         * `math-outline-regexp' : New variable.
;;         * `math-electric' : New function.
;;         * Change syntax of "`" to be word-constituent.
;;         * Change indentation routines to use SMIE.
;; 2013-10-13
;;         * `math-program-arguments' : New variable
;;         * `run-math' : Cleanup

;;; Code:

(require 'comint)
(require 'smie)

;;;; Customs and Variables

(defgroup math++ nil
  "Editing Mathematica code"
  :group 'languages)

(defcustom math-mode-hook nil
  "Normal hook run when entering `math-mode'.
See `run-hooks'."
  :type 'hook
  :group 'math++)

(defcustom math-program "math"
  "Command to invoke at `run-math'."
  :type 'string
  :group 'math++)

(defcustom math-program-arguments '()
  "Command to invoke at `run-math'."
  :type '(repeat string)
  :group 'math++)

(defcustom math-indent 8
  "Basic Indentation for newline."
  :type 'integer
  :group 'math++)

;;;; math-mode

(defvar math-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'newline-and-indent)
    (define-key map "]" 'math-electric-braket)
    (define-key map ")" 'math-electric-paren)
    (define-key map "}" 'math-electric-brace)
    map)
  "Keymap for Math++ mode.")

(defvar math-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; white space
    (modify-syntax-entry ?  " " syntax-table)
    (modify-syntax-entry ?\t " " syntax-table)
    (modify-syntax-entry ?\f " " syntax-table)
    (modify-syntax-entry ?\n " " syntax-table)
    (modify-syntax-entry ?\^m " " syntax-table)

    ;; comments and parens
    (modify-syntax-entry ?( "()1n" syntax-table)
    (modify-syntax-entry ?) ")(4n" syntax-table)
    (modify-syntax-entry ?* "_ 23n" syntax-table)

    ;; pure parens
    (modify-syntax-entry ?[ "(]" syntax-table)
    (modify-syntax-entry ?] ")[" syntax-table)
    (modify-syntax-entry ?{ "(}" syntax-table)
    (modify-syntax-entry ?} "){" syntax-table)

    ;; punctuation
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?: "." syntax-table)
    (modify-syntax-entry ?% "." syntax-table)
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?| "." syntax-table)
    (modify-syntax-entry ?_ "." syntax-table)
    (modify-syntax-entry ?/ "." syntax-table)
    (modify-syntax-entry ?! "." syntax-table)
    (modify-syntax-entry ?@ "." syntax-table)
    (modify-syntax-entry ?# "." syntax-table)
    (modify-syntax-entry ?\' "." syntax-table)

    ;; quotes
    (modify-syntax-entry ?\\ "\\" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)

    ;; for Math numbers, the following would be better as
    ;; parts of symbols
    (modify-syntax-entry ?- "_" syntax-table)
    (modify-syntax-entry ?. "_" syntax-table)
    (modify-syntax-entry ?\` "_" syntax-table)
    (modify-syntax-entry ?^ "_" syntax-table)

    (modify-syntax-entry ?$ "_" syntax-table)
    (modify-syntax-entry ?+ "_" syntax-table)

    syntax-table)
  "Syntax table used in `math-mode'.")

(define-abbrev-table 'math-mode-abbrev-table ())

(defvar math-font-lock-keywords
  '(
    ("^In\[[0-9]+\]:=" . font-lock-keyword-face)
    ("^Out\[[0-9]+\]=" . font-lock-keyword-face)
    ("^Out\[[0-9]+\]//[A-Za-z][A-Za-z0-9]*=" . font-lock-keyword-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t]*[\[][ \t]*[\[]" 1 "default")
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t]*[\[]" 1 font-lock-function-name-face)
    ("//[ \t\f\n]*\\([A-Za-z][A-Za-z0-9`]*\\)" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t\f\n]*/@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t\f\n]*//@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t\f\n]*@@" 1 font-lock-function-name-face)
    ("_[) \t]*\\?\\([A-Za-z][A-Za-z0-9`]*\\)" 1 font-lock-function-name-face)
    ("\\(&&\\)" 1 "default")
    ("&" . font-lock-function-name-face)
    ("\\\\[[A-Za-z][A-Za-z0-9]*\]" . font-lock-constant-face )
    ("$[A-Za-z0-9]+" . font-lock-variable-name-face )
    ("\\([A-Za-z0-9]+\\)[ \t]*\\->" 1 font-lock-type-face )
    ("<<[ \t\f\n]*[A-Za-z][A-Za-z0-9]*`[ \t\f\n]*[A-Za-z][A-Za-z0-9]*[ \t\f\n]*`"
     . font-lock-type-face )
    ("[A-Za-z][A-Za-z0-9]*::[A-Za-z][A-Za-z0-9]*" . font-lock-warning-face)
    ("\\[Calculating\\.\\.\\.\\]" . font-lock-warning-face)
    ("\\[Mathematica.*\\]" . font-lock-warning-face)
    ("^Interrupt>" . font-lock-warning-face)
    ("-Graphics-" . font-lock-type-face)
    ("-DensityGraphics-" . font-lock-type-face)
    ("-ContourGraphics-" . font-lock-type-face)
    ("-SurfaceGraphics-" . font-lock-type-face)
    ("-Graphics3D-" . font-lock-type-face)
    ("-GraphicsArray-" . font-lock-type-face)
    ("-Sound-" . font-lock-type-face)
    ("-CompiledCode-" . font-lock-type-face)))

(defvar math-outline-regexp "\\((\\*\\|.+?:=\\)")

(defvar math-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    `((head) (epsilon) (string)
      (top (top "\n" expr))
      (expr (head "[" exprs "]")
            (expr "[[" exprs "]]")
            ("{" exprs "}")
            ("(" expr ")")
            ;; message
            (expr "::" string)
            ;; statement separation
            (expr ";" expr)
            (expr "&")
            ;; delayed set
            (expr ":=" expr)
            (head "/:" expr ":=" expr)
            ;; set
            (expr "=" expr)
            (head "/:" expr "=" expr)
            (expr "+=" expr)
            (expr "-=" expr)
            (expr "*=" expr)
            (expr "/=" expr)
            ;; operation
            (expr "~" head "~" expr)
            (expr "@@" expr)
            (expr "==" expr)
            (expr "||" expr)
            (expr "&&" expr)
            (expr "+" expr)
            (expr "-" expr)
            (expr "*" expr)
            (expr "/" expr)
            (expr "^" expr)
            ;; application
            (expr ":" expr)
            (expr "/;" expr)
            (expr "//" expr))
      (exprs (epsilon)
             (expr)
             (exprs "," expr)))
    '((assoc ";")
      (assoc "::")
      (assoc "&")
      (assoc "/:")
      (assoc ":=" "=" "+=" "-=" "*=" "/=")
      (assoc "/;" ":" "//")
      (assoc "~")
      (assoc "@@" "==")
      (assoc "||" "&&")
      (assoc "+" "-")
      (assoc "*" "/")
      (assoc "^")
      (assoc "[[")))))

(defun math-smie-rules (kind token)
  "Mathematica SMIE indentation function for KIND and TOKEN."
  (pcase (cons kind token)
    (`(:before . "[")
     (save-excursion
       (smie-default-backward-token)
       `(column . ,(current-column))))
    (`(:after . ":=") `(column . ,math-indent))
    (`(:after . ,(or "]" "}" ")")) '(column . 0))
    (`(:after . ,(or "[" "{" "("))
     (save-excursion
       (beginning-of-line)
       (skip-chars-forward " \t")
       `(column . ,(+ math-indent (current-column)))))
    (`(,_ . ";") (smie-rule-separator kind))
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:elem . ,_) 0)
    (t nil)))

(defalias 'math-smie-forward-token 'smie-default-forward-token)
(defalias 'math-smie-backward-token 'smie-default-backward-token)

;;;###autoload
(define-derived-mode math-mode prog-mode "Mathematica"
  "Major mode for editing Mathematica text files in Emacs.

\\{math-mode-map}
Entry to this mode calls the value of `math-mode-hook'
if that value is non-nil."
  :syntax-table math-mode-syntax-table
  :abbrev-table math-mode-abbrev-table
  (smie-setup math-smie-grammar #'math-smie-rules
              :forward-token 'math-smie-forward-token
              :backward-token 'math-smie-backward-token)
  (math-mode-variables))

(defun math-mode-variables ()
  "Local variables for both Major and Inferior mode."
  (set-syntax-table math-mode-syntax-table)
  ;; set local variables
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local comment-start-skip "(\\*")
  (setq-local font-lock-defaults '(math-font-lock-keywords nil t))
  (setq-local outline-regexp math-outline-regexp))

(defun math-electric (char arg)
  "Indent on closing a CHAR ARG times."
  (if (not arg) (setq arg 1) nil)
  (dotimes (_i arg) (insert char))
  (funcall indent-line-function)
  (blink-matching-open))

(defun math-electric-paren (arg)
  "Indent on closing a paren ARG times."
  (interactive "p")
  (math-electric ")" arg))

(defun math-electric-braket (arg)
  "Indent on closing a braket ARG times."
  (interactive "p")
  (math-electric "]" arg))

(defun math-electric-brace (arg)
  "Indent on closing a brace ARG times."
  (interactive "p")
  (math-electric "}" arg))

;;;; inferior Mathematica mode.

(defun math-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-math-mode)
				      (current-buffer)
				    "*math*"))))
    (or proc
	(error "No current process.  Do M-x `run-math'"))))

(defun math-send-region (start end)
  "Send the current region to the inferior Mathematica process."
  (interactive "r")
  (comint-send-region (math-proc) start end)
  (comint-send-string (math-proc) "\C-j"))

(define-derived-mode inferior-math-mode comint-mode "Inferior Mathematica"
  "Major mode for interacting with an inferior Mathematica process"
  :abbrev-table math-mode-abbrev-table
  (setq comint-prompt-regexp "^(In|Out)\[[0-9]*\]:?= *")
  (math-mode-variables)
  (setq mode-line-process '(":%s"))
  (setq comint-process-echoes t))

;;;###autoload
(defun run-math (cmd)
  "Run an inferior Mathematica process CMD, input and output via buffer *math*."
  (interactive (list (if current-prefix-arg
                         (read-string "Run Mathematica: " math-program)
                       math-program)))
  (setq math-program cmd) ; memo
  (let ((buffer (get-buffer "*math*"))
        (cmdlist (append (split-string-and-unquote cmd)
                             math-program-arguments)))
    (apply 'make-comint-in-buffer "math" buffer (car cmdlist)
           nil (cdr cmdlist))
    (set-buffer "*math*")
    (inferior-math-mode)
    (pop-to-buffer-same-window "*math*")))

(defun math-here-is-space ()
  (let ((ca (char-after))
	(cb (char-before)))
    (and ca cb
	 (string-match "[ \t\n]" (char-to-string ca))
	 (string-match "[ \t\n]" (char-to-string cb)))))

(defun math-moveto-last-content ()
  (while (math-here-is-space)
    (backward-char 1)))

(defun math-moveto-first-content ()
  (while (math-here-is-space)
    (forward-char 1)))

(defun math-beginning-of-cell ()
  (math-moveto-last-content)
  (if (re-search-backward "^$" nil t) (forward-char 1)
    (goto-char (point-min))))

(defun math-end-of-cell ()
  (math-moveto-first-content)
  (if (re-search-forward "^$" nil t) (backward-char 1)
    (goto-char (point-max))))

(defun math-send-last-mathexp ()
  "Send the last math expression to the inferior Mathematica process."
  (interactive)
  (save-excursion
    (let ((math-start (progn (math-beginning-of-cell) (point)))
	  (math-end (progn (math-end-of-cell) (point))))
      (comint-send-region (math-proc) math-start math-end)
      (comint-send-string (math-proc) "\C-j"))))

(define-key math-mode-map "\C-c\C-r" 'math-send-region)
(define-key math-mode-map "\C-c\C-e" 'math-send-last-mathexp)
(define-key math-mode-map "\C-c\C-s" 'math-send-last-mathexp)

(provide 'math++)

;; Local Variables:
;; coding: utf-8-unix
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; math++.el ends here
