;;;
;;; math++.el, A Mathematica interface for GNU Emacs
;;; based on math.el, mma.el, mathematica.el.
;;; $Id: math++.el,v 1.3 2009/07/08 04:16:17 daichi Exp $
;;;
;;; Copyright (C) 2009 Daichi Mochihashi <daichi at cslab.kecl.ntt.co.jp>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mathematica is (C) Copyright 1988-1999 Wolfram Research, Inc.
;;;
;;; Protected by copyright law and international treaties.
;;;
;;; Unauthorized reproduction or distribution subject to severe civil
;;; and criminal penalties.
;;;
;;; Mathematica is a registered trademark of Wolfram Research.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar math-mode-map nil)
(defvar math-mode-syntax-table nil)
(defvar math-mode-line-process "")
(defvar math-process-string "math")
(defvar math-process-buffer "*math*")

(if math-mode-map ()
  (setq math-mode-map (make-sparse-keymap))
  (define-key math-mode-map "\C-m" 'newline-and-indent)
  (define-key math-mode-map "]" 'math-electric-braket)
  (define-key math-mode-map ")" 'math-electric-paren)
  (define-key math-mode-map "}" 'math-electric-brace)
  )

(if math-mode-syntax-table ()
  (let ((i 0))
    (setq math-mode-syntax-table (make-syntax-table))

    ;; white space
    (modify-syntax-entry ?  " " math-mode-syntax-table)  
    (modify-syntax-entry ?\t " " math-mode-syntax-table)
    (modify-syntax-entry ?\f " " math-mode-syntax-table)
    (modify-syntax-entry ?\n " " math-mode-syntax-table)
    (modify-syntax-entry ?\^m " " math-mode-syntax-table)
      
    ;; comments and parens
    (modify-syntax-entry ?( "()1b" math-mode-syntax-table)  
			 (modify-syntax-entry ?) ")(4b" math-mode-syntax-table)
    (modify-syntax-entry ?* "_ 23b" math-mode-syntax-table)

    ;; pure parens
    (modify-syntax-entry ?[ "(]" math-mode-syntax-table)
			 (modify-syntax-entry ?] ")[" math-mode-syntax-table)
    (modify-syntax-entry ?{ "(}" math-mode-syntax-table)
    (modify-syntax-entry ?} "){" math-mode-syntax-table)

    ;; punctuation
    (modify-syntax-entry ?= "." math-mode-syntax-table)
    (modify-syntax-entry ?: "." math-mode-syntax-table)
    (modify-syntax-entry ?% "." math-mode-syntax-table)
    (modify-syntax-entry ?< "." math-mode-syntax-table)
    (modify-syntax-entry ?> "." math-mode-syntax-table)
    (modify-syntax-entry ?& "." math-mode-syntax-table)
    (modify-syntax-entry ?| "." math-mode-syntax-table)
    (modify-syntax-entry ?_ "." math-mode-syntax-table)
    (modify-syntax-entry ?/ "." math-mode-syntax-table)
    (modify-syntax-entry ?! "." math-mode-syntax-table)
    (modify-syntax-entry ?@ "." math-mode-syntax-table)
    (modify-syntax-entry ?# "." math-mode-syntax-table)
    (modify-syntax-entry ?\' "." math-mode-syntax-table)

    ;; quotes
    (modify-syntax-entry ?\\ "\\" math-mode-syntax-table)
    (modify-syntax-entry ?\" "\"" math-mode-syntax-table)

    ;; for Math numbers, the following would be better as
    ;; parts of symbols
    (modify-syntax-entry ?- "_" math-mode-syntax-table)
    (modify-syntax-entry ?. "_" math-mode-syntax-table)
    (modify-syntax-entry ?\` "_" math-mode-syntax-table)
    (modify-syntax-entry ?^ "_" math-mode-syntax-table)

    (modify-syntax-entry ?$ "_" math-mode-syntax-table)
    (modify-syntax-entry ?+ "_" math-mode-syntax-table)

    ;; create an abbrev table for math mode
    (define-abbrev-table 'math-mode-abbrev-table ())

    )					; end of let
  )

(defvar mathematica-font-lock-keywords
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
    ("-CompiledCode-" . font-lock-type-face)
    )
  )


;; main

(defun math-mode ()
  "Major mode for editing Mathematica text files in Emacs."
  (interactive)
  (kill-all-local-variables)
  (math-mode-initialize)
  (math-mode-variables)
  (run-hooks 'math-mode-hook))

(defun math-mode-initialize ()
  (use-local-map math-mode-map)
  (setq major-mode 'math-mode)
  (setq mode-name "Mathematica"))

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
  (setq font-lock-defaults '(mathematica-font-lock-keywords nil t))
)

(defgroup math nil
  "Editing Mathematica code"
  :group 'lisp)

(defcustom math-mode-hook nil
  "Normal hook run when entering `math-mode'.
See `run-hooks'."
  :type 'hook
  :group 'math)

;;;;
;;;;  from mathematica.el.
;;;;

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
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

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
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

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
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )


(defun math-indent-samelinep (first second)
  "Determines if the two points belong to the same line."
  (let ((limit (- second first)) (same-line))
    (save-excursion
      (if (re-search-forward "[\f\n]" limit t)
	  (setq same-line nil)
	(setq same-line t)
	) ; end of if
      ) ; end of excursion
    ) ; end of let
  )

(defun math-indent-determine-in-comment ()
  "Returns the beginning of the comment, or nil."
  (save-excursion
    (let ((here (point)) (no-open nil) (first-open) (no-close nil) (first-close))

      (if (search-backward "(*" nil t)
	  (setq first-open (point))
	(setq no-open t)
	) ; end if

      (goto-char here)
      (if (search-backward "*)" nil t)
	  (setq first-close (point))
	(setq no-close t)
	) ; end if
      
      (cond ((and no-open no-close) nil)
	    ((and (not no-open) no-close) first-open)
	    ((and no-open (not no-close)) nil)
	    ((and (not no-open) (not no-close))
	     (if (> first-open first-close) first-open nil)
	     )
	    ) ; end cond
      ) ; end let
    ) ; end excursion
  )

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
		      "(*")
		     ) ; end of and
		(setq home nil)
	      (setq home t)
	      ) ; end if this open paren is the start of a comment
	    ) ; end while looking for an unbalanced open paren
	(error (setq toplevel (point)))
	) ; end condition-case
      (if toplevel nil (point))
      ) ; end let
    ) ; end excursion
  )

(defun math-indent-stepthrough-comments ()
  "Moves the point backward through comments and non-eoln whitespace."
  (let ((home nil))
    (while (not home)
      (skip-chars-backward " \t")
      (setq home t) ; tenative assumtion
      (if (and (>= (- (point) 2) (point-min))
	       (string=
		(format "%s" (buffer-substring (- (point) 2) (point)))
		"*)")
	       ) ; end of and
	  (if (search-backward "(*" nil t)
	      (setq home nil)
	    nil
	    ) ; end if comment has a beginning
	) ; end if we stopped at the end of a comment
      ) ; end loop between comments and whitespace
    ) ; end of let
  )

(defun math-indent-line-emptyp ()
  "Returns t if the line the point is on is empty, nil if not."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "[\f\n]")
    ) ; end excursion
  )

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
	      (setq home t)
	      ) ; end of progn
	  (progn

	    (backward-char 1)
	    (math-indent-stepthrough-comments)
	
	    (if (math-indent-line-emptyp)
		(progn ; we're done, there is no previous line
		  (setq meaningful-end nil)
		  (setq home t)
		  ) ; end progn
	      (progn
		(setq meaningful-end (point))
		(beginning-of-line)
		(if (= meaningful-end (point))
		    (setq home nil) ; there was nothing on this line but
				    ; comments
		  (setq home t) ; this is a good line
		  )
		) ; end progn
	      ) ; end if-else line empty

	    ) ; end line empty progn
	  ) ; end line empty if-else

	) ; end while
      
      meaningful-end
      ) ; end let
    ) ; end excursion
  )

(defun math-indent-determine-indent ()
  "Returns the indentation of the line the point is on."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)
    ) ; end excursion
  )

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
		  ((= start-char ?}) (setq start-close-paren ?{))
		  ) ; end of cond
	    ) ; end of progn
	nil
	) ; end if you're not at the very end of the buffer

      (setq in-comment (math-indent-determine-in-comment))
      (if in-comment ; in-comment = the position of the opening of the comment
	  (let ((tmp (+ in-comment 2)) (tmp-column))
	    (goto-char tmp)
	    (setq tmp-column (current-column))

	    (skip-chars-forward " \t")
	    (if (looking-at "[\f\n]") ; first comment line has nothing
				      ; but "(*"
		(1+ tmp-column) ; return one space after the "(*"
	      (current-column)
	      ) ; end if
	    ) ; end let in-comment

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
		  (let ((tmp in-unbalanced)) 
		    (forward-char 1)
		    (skip-chars-forward " \t")
		    (if (looking-at "[\f\n]")
			(+ (math-indent-determine-indent) 4)
		      (current-column)
		      ) ; end if unbalanced-paren ends the line
		    ) ; end let unbalanced-paren isn't immediately matched
		  ) ; end if immediate match
		) ; end progn unbalanced-paren
	    
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
		) ; end prevline if-else
	      ) ; end at toplevel progn

	    ) ; end unbalanced if-else
	  ) ; end non-comment progn
	) ; end in-comment if-else
      ) ; end outermost let
    ) ; end excursion
  )

(defun math-indent-line ()
  "Indent current line as Mathematica code."
  (interactive)
  (let ((indent (math-indent-calculate (point))) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	nil
      (progn
	(delete-region beg (point))
	(indent-to indent)
	) ; end of progn
      ) ; end if there is nothing to shift
  
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))
      nil
      ) ; end if we need to move the cursor
    ) ; end of let
  )

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
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

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
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

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
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

;;
;; inferior Mathematica mode.
;; like cmuscheme.el.  
;;

(require 'comint)

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

(defun scheme-args-to-list (string) ; copied from cmuscheme.el
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (scheme-args-to-list (substring string (+ 1 where)
                                                 (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (scheme-args-to-list (substring string pos
                                                 (length string)))))))))
  
(define-derived-mode inferior-math-mode comint-mode "Inferior Mathematica"
  "Major mode for interacting with an inferior Mathematica process"
  (setq comint-prompt-regexp "^(In|Out)\[[0-9]*\]:?= *")
  (math-mode-variables)
  (setq mode-line-process '(":%s"))
  (setq comint-process-echoes t)
  ; (setq comint-input-filter (function math-send-filter))
  (setq comint-get-old-input (function math-get-old-input)))

(defun run-math (cmd)
    "Run an inferior Mathematica process, input and output via buffer *math*."
    (interactive (list (if current-prefix-arg
			   (read-string "Run Mathematica: " math-process-string)
                         math-process-string)))
  (if (not (comint-check-proc "*math*"))
      (let ((cmdlist (scheme-args-to-list cmd)))
        (set-buffer (apply 'make-comint "math" (car cmdlist)
                           nil (cdr cmdlist)))
        (inferior-math-mode)))
  (setq math-process-string cmd)
  (setq math-buffer "*math*")
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
    (beginning-of-buffer)))

(defun math-end-of-cell ()
  (math-moveto-first-content)
  (if (re-search-forward "^$" nil t) (backward-char 1)
      (end-of-buffer)))

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

(provide 'math)
