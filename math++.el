;;; math++.el --- Mathematica editing and inferior mode. mode  -*- lexical-binding: t -*-

;; Filename: math++.el
;; Description: Mathematica editing and inferior Mode
;; Author: Daichi Mochihashi <daichi at cslab.kecl.ntt.co.jp>
;; Modified by: Taichi Kawabata <kawabata.taichi_at_gmail.com>
;; Created: 2009-07-08
;; Modified: 2013-10-06
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

;; - better font-lock for Constants and Package namespaces.
;; - math-imenu-generic-expression
;; - math-outline-regexp
;; - sending useful commands to comint buffer.

;;; Commentary:

;; This code is modified version of `math++.el'
;; (http://chasen.org/~daiti-m/dist/math++.el), which is based on
;; math.el, mma.el, mathematica.el.

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
;;         * change `math-process-string' to `math-program'
;;         * change `scheme-args-to-list' to `split-string-and-unquote'

;;; Code:

(require 'comint)

;;;; Custom Variables

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

;;;; Variables
(defvar math-mode-line-process "")
(defvar math-process-buffer "*math*")

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
    (modify-syntax-entry ?( "()1b" syntax-table)
    (modify-syntax-entry ?) ")(4b" syntax-table)
    (modify-syntax-entry ?* "_ 23b" syntax-table)

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
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t]*[\[][ \t]*[\[]" 1 "default")
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t]*[\[]" 1 font-lock-function-name-face)
    ("//[ \t\f\n]*\\([A-Za-z][A-Za-z0-9]*\\)" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t\f\n]*/@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t\f\n]*//@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t\f\n]*@@" 1 font-lock-function-name-face)
    ("_[) \t]*\\?\\([A-Za-z][A-Za-z0-9]*\\)" 1 font-lock-function-name-face)
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

;;;###autoload
(define-derived-mode math-mode prog-mode "Mathematica"
  "Major mode for editing Mathematica text files in Emacs.

\\{math-mode-map}
Entry to this mode calls the value of `math-mode-hook'
if that value is non-nil."
  :syntax-table math-mode-syntax-table
  :abbrev-table math-mode-abbrev-table
  (math-mode-variables))

(defun math-mode-variables ()
  (set-syntax-table math-mode-syntax-table)
  ;; make local variables
  (make-local-variable 'indent-line-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'font-lock-defaults)
  ;; set local variables
  (setq indent-line-function 'math-indent-line)
  (setq comment-start "(*")
  (setq comment-end "*)")
  (setq comment-start-skip "(\\*")
  (setq font-lock-defaults '(math-font-lock-keywords nil t)))

(defun math-indent-determine-in-comment ()
  "Returns the beginning of the comment, or nil."
  (save-excursion
    (let ((here (point)) (no-open nil) (first-open) (no-close nil) (first-close))

      (if (search-backward "(*" nil t)
	  (setq first-open (point))
	(setq no-open t))

      (goto-char here)
      (if (search-backward "*)" nil t)
	  (setq first-close (point))
	(setq no-close t))

      (cond ((and no-open no-close) nil)
	    ((and (not no-open) no-close) first-open)
	    ((and no-open (not no-close)) nil)
	    ((and (not no-open) (not no-close))
	     (if (> first-open first-close) first-open nil))))))

(defun math-indent-determine-unbalanced ()
  "Returns the beginning of the open paren or nil. Assumes not in
comment."
  (save-excursion
    (let ((toplevel nil) (home nil))
      (condition-case nil
	  (while (not home)
	    (up-list -1)
	    (if (and (<= (+ (point) 2) (point-max))
		     (string=
		      (format "%s" (buffer-substring (point) (+ (point) 2)))
		      "(*"))
		(setq home nil)
	      (setq home t)))
	(error (setq toplevel (point))))
      (if toplevel nil (point)))))

(defun math-indent-stepthrough-comments ()
  "Moves the point backward through comments and non-eoln whitespace."
  (let ((home nil))
    (while (not home)
      (skip-chars-backward " \t")
      (setq home t) ; tenative assumtion
      (if (and (>= (- (point) 2) (point-min))
	       (string=
		(format "%s" (buffer-substring (- (point) 2) (point)))
		"*)"))
	  (if (search-backward "(*" nil t)
	      (setq home nil)
	    nil)))))

(defun math-indent-line-emptyp ()
  "Returns t if the line the point is on is empty, nil if not."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "[\f\n]")))

(defun math-indent-determine-prevline ()
  "Returns the meaningful end of the previous line (is it a
semicolon?), under the assumtion that you're not in a comment or
unbalanced parens."
  (save-excursion
    (let ((home nil) (meaningful-end))
      (while (not home)
	(beginning-of-line)
	(if (= (point) (point-min))
	    (progn ; There's nowhere to go. You're quite done.
	      (setq meaningful-end nil)
	      (setq home t))
	  (progn

	    (backward-char 1)
	    (math-indent-stepthrough-comments)

	    (if (math-indent-line-emptyp)
		(progn ; we're done, there is no previous line
		  (setq meaningful-end nil)
		  (setq home t))
	      (progn
		(setq meaningful-end (point))
		(beginning-of-line)
		(if (= meaningful-end (point))
		    (setq home nil) ; there was nothing on this line but
                                        ; comments
		  (setq home t)))))))
      meaningful-end)))

(defun math-indent-determine-indent ()
  "Returns the indentation of the line the point is on."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun math-indent-calculate (start)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (skip-chars-forward " \t")

    (let ((start-char) (start-close-paren ? )
	  (in-comment) (in-unbalanced) (prevline))
      (if (not (= (point) (point-max)))
	  (progn
	    (setq start-char (char-after))
	    (cond ((= start-char ?\)) (setq start-close-paren ?\())
		  ((= start-char ?\]) (setq start-close-paren ?\[))
		  ((= start-char ?}) (setq start-close-paren ?{))))
	nil) ; end if you're not at the very end of the buffer
      (setq in-comment (math-indent-determine-in-comment))
      (if in-comment ; in-comment = the position of the opening of the comment
	  (let ((tmp (+ in-comment 2)) (tmp-column))
	    (goto-char tmp)
	    (setq tmp-column (current-column))

	    (skip-chars-forward " \t")
	    (if (looking-at "[\f\n]") ; first comment line has nothing
                                        ; but "(*"
		(1+ tmp-column) ; return one space after the "(*"
	      (current-column)))

	(progn ; from now on, you're not in a comment
	  (setq in-unbalanced (math-indent-determine-unbalanced))
	  (if in-unbalanced ; in-unbalanced = the opening paren
	      (progn
		(goto-char in-unbalanced)
		(if (= (char-after) start-close-paren)
		    ;; (current-column) ;; 2009-07-05 patch by daichi
		    (save-excursion
		      (progn (skip-chars-backward "0-9A-Za-z")
			     (current-column)))
		  (progn ;let ((tmp in-unbalanced))
		    (forward-char 1)
		    (skip-chars-forward " \t")
		    (if (looking-at "[\f\n]")
			(+ (math-indent-determine-indent) 4)
		      (current-column)))))
	    (progn ; from now on, you're not in a comment or
                                        ; unbalanced paren (you're at toplevel)
	      (setq prevline (math-indent-determine-prevline))
	      (if prevline
		  (progn ; prevline = end of the last line
		    (goto-char prevline)
		    (if (= (char-before) ?\;)
			0 ; a fully top-level command
		      4 ; a continuation of a toplevel command
		      ) ; end if last line ended in a semicolon
		    ) ; end progn there was a last line
		0 ; if there's no previous line (in this execution
                                        ; block) don't indent
		))))))))

(defun math-indent-line ()
  "Indent current line as Mathematica code."
  (interactive)
  (let ((indent (math-indent-calculate (point))) shift-amt beg ; end -- unused variable
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	nil
      (progn
	(delete-region beg (point))
	(indent-to indent))) ; end if there is nothing to shift

    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))
      nil)))

(defun math-electric-paren (arg)
  "Indents on closing a paren."
  (interactive "p")
  (let ((start (point)))
    (if (not arg) (setq arg 1) nil)
    (let ((i 0)) (while (< i arg) (insert ")") (setq i (1+ i))))
    (save-excursion
      (goto-char start)
      (skip-chars-backward " \t")
      (if (= (current-column) 0)
	  (math-indent-line)
	nil)))
  (blink-matching-open))

(defun math-electric-braket (arg)
  "Indents on closing a braket."
  (interactive "p")
  (let ((start (point)))
    (if (not arg) (setq arg 1) nil)
    (let ((i 0)) (while (< i arg) (insert "]") (setq i (1+ i))))
    (save-excursion
      (goto-char start)
      (skip-chars-backward " \t")
      (if (= (current-column) 0)
	  (math-indent-line)
	nil)))
  (blink-matching-open))

(defun math-electric-brace (arg)
  "Indents on closing a brace."
  (interactive "p")
  (let ((start (point)))
    (if (not arg) (setq arg 1) nil)
    (let ((i 0)) (while (< i arg) (insert "}") (setq i (1+ i))))
    (save-excursion
      (goto-char start)
      (skip-chars-backward " \t")
      (if (= (current-column) 0)
	  (math-indent-line)
	nil)))
  (blink-matching-open))

;;;; inferior Mathematica mode.

(defun math-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-math-mode)
				      (current-buffer)
				    math-process-buffer))))
    (or proc
	(error "No current process.  See variable `math-process-buffer'"))))

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
  (setq comint-process-echoes t)
  ;;(setq comint-input-filter (function math-send-filter))
  ;;(setq comint-get-old-input (function math-get-old-input))
  )

;;;###autoload
(defun run-math (cmd)
  "Run an inferior Mathematica process, input and output via buffer *math*."
  (interactive (list (if current-prefix-arg
                         (read-string "Run Mathematica: " math-program)
                       math-program)))
  (if (not (comint-check-proc "*math*"))
      (let ((cmdlist (split-string-and-unquote cmd)))
        (set-buffer (apply 'make-comint "math" (car cmdlist)
                           nil (cdr cmdlist)))
        (inferior-math-mode)))
  (setq math-program cmd)
  (setq math-process-buffer "*math*")
  (pop-to-buffer "*math*"))

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
