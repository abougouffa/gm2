;; Copyright (C) 1985, 1986, 1987, 2001, 2002, 2003, 2004,
;;               2005, 2006, 2007, 2008, 2009
;; Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;
;;   To do:
;;
;;   (1)   fix m2-tag to search: current implementation module,
;;                               current definition module,
;;                               library implementation module and
;;                               then definition module
;;

;; Modula-2 mode editing commands for Emacs
;;
;; Author Gaius Mulley (gaius@glam.ac.uk)
;;    contains automatic indentation
;;    automatic END matching
;;    electric THEN, END, ELSE
;;    expression formatting
;;    comment formatting
;;    procedure declaration finding
;;    const, type, var declaration finding.
;;    word expansion
;;
;; with statement expansion and module visiting lisp routines from:
;;
;;    Mick Jordan
;;    amended by Peter Robinson
;;    ported to GNU Michael Schmidt <michael@pbinfo.UUCP>
;;    modified by Tom Perrine <Perrin@LOGICON.ARPA> (TEP)
;;
;;


(defconst m2-indent-level 3
  "*Indentation of M2 statements with respect to containing block.")

(defconst m2-auto-indent-on-end t
  "*Automatic indentation when END is typed.")

(defconst m2-auto-indent-on-then t
  "*Automatic indentation when THEN is typed.")

(defconst m2-auto-indent-on-else t
  "*Automatic indentation when ELSE is typed.")

;; default path which is overwritten by the environment variable
;; M2PATH. (If it exists).

(defconst m2-default-path ".")

(defvar m2-path (or (getenv "M2PATH") m2-default-path))

(defvar m2-options (getenv "M2OPTIONS"))

(defvar m2-debug-command (getenv "M2DEBUG")
  "Command to compile Modula-2 programs")

(defvar m2-compile-command (concat "gm2 -c " m2-options)
  "Command to compile Modula-2 programs")

(defvar m2-link-command (concat "gm2 -I. " m2-options)
  "Command to link Modula-2 programs")

(defvar m2-link-name nil
  "Name of the executable.")

(defconst m2-if-then-same-line nil
  "set this to t if you like the THEN on the same line as IF,
   or set it to nil if you place THEN on the next line.")

(defconst m2-assign-future t
  "set to to nil if you have not assigned all future code to the
   fsf or set to t if you have. If you have assigned all future
   code to the FSF then this is useful as it will automatically
   generate the FSF copyright and warranty disclaimer in any
   new module.")

(defvar g-mode-abbrev-table nil
  "Abbrev table in use in g-mode buffers.")
(define-abbrev-table 'g-mode-abbrev-table ())

(defvar g-mode-map ()
  "Keymap used in M2 mode.")

(defun setup-g-mode-keys ()
  "sets up the keymap for g-mode."
  (setq g-mode-map (make-sparse-keymap))
  (define-key g-mode-map "\t" 'm2-tab)
  (define-key g-mode-map "D" 'm2-test-end)
  (define-key g-mode-map "N" 'm2-test-then)
  (define-key g-mode-map "E" 'm2-test-else)
;;  (define-key g-mode-map "%" 'm2-local-test)
;;  (define-key g-mode-map "!" 'm2-local-recompile)
  (define-key g-mode-map "\177" 'backward-delete-char-untabify)
  (define-key g-mode-map "\e."   'm2-tag)
  (define-key g-mode-map "\e\t"  'm2-complete)
  (define-key g-mode-map "\C-cb" 'm2-begin)
  (define-key g-mode-map "\C-cc" 'm2-case)
  (define-key g-mode-map "\C-cd" 'm2-definition)
  (define-key g-mode-map "\C-ce" 'm2-else)
  (define-key g-mode-map "\C-cf" 'm2-for)
  (define-key g-mode-map "\C-ch" 'm2-header)
  (define-key g-mode-map "\C-ci" 'm2-if)
  (define-key g-mode-map "\C-cm" 'm2-module)
  (define-key g-mode-map "\C-cl" 'm2-loop)
  (define-key g-mode-map "\C-co" 'm2-or)
  (define-key g-mode-map "\C-cp" 'm2-procedure)
  (define-key g-mode-map "\C-c\C-w" 'm2-with)
  (define-key g-mode-map "\C-c\C-e" 'm2-elsif)
  (define-key g-mode-map "\C-cr" 'm2-record)
  (define-key g-mode-map "\C-cs" 'm2-stdio)
  (define-key g-mode-map "\C-ct" 'm2-type)
  (define-key g-mode-map "\C-cu" 'm2-until)
  (define-key g-mode-map "\C-cv" 'm2-var)
  (define-key g-mode-map "\C-cw" 'm2-while)
  (define-key g-mode-map "\C-cx" 'm2-export)
  (define-key g-mode-map "\C-cy" 'm2-import)
  (define-key g-mode-map "\C-c\C-h" 'm2-help)
  (define-key g-mode-map "\C-c\C-z" 'suspend-emacs)
  (define-key g-mode-map "\C-c\C-v" 'm2-visit)
  (define-key g-mode-map "\C-c\C-t" 'm2-toggle)
  (define-key g-mode-map "\C-c\C-l" 'm2-link)
  (define-key g-mode-map "\C-c\C-d" 'm2-debug)
  (define-key g-mode-map "\C-c\C-a" 'm2-assembler)
  (define-key g-mode-map "\C-c\C-c" 'm2-compile))

(defun m2-tag ()
  "m2-tag finds the declaration of the modula-2 symbol the cursor is on."
  (interactive)
  (setq case-fold-search nil)
  (save-excursion
    (while (looking-at "[A-Za-z0-9]")
      (forward-char -1))
    (forward-char 1)
    (let (m2-start)
      (setq m2-start (point))
      (while (looking-at "[A-Za-z0-9]")
	(forward-char 1))
      (let (m2-end)
	(setq m2-end (point))
	(let (m2-object)
	  (setq m2-object (buffer-substring m2-start m2-end))
	  (if (not (m2-find-declaration m2-object))
	      (message "cannot find declaration of: %s" m2-object)))))))

(defun m2-complete ()
  "m2-complete - expands the string the cursor is currently on."
  (interactive)
  (setq case-fold-search nil)
  (forward-char -1)
  (while (looking-at "[A-Za-z0-9]")
    (forward-char -1))
  (forward-char 1)
  (let (m2-start)
    (setq m2-start (point))
    (while (looking-at "[A-Za-z0-9]")
      (forward-char 1))
    (let (m2-end)
      (setq m2-end (point))
      (m2-find-previous-string (buffer-substring m2-start m2-end)))))

(defun m2-find-previous-string (m2-object)
  "searches for a previous m2-object and inserts the remaining characters."
  (interactive)
  (save-excursion
    (message "searching for %s" m2-object)
    (let (m2-insert)
      (setq m2-insert (point))
      (forward-char (- 0 (length m2-object)))
      (if (search-backward m2-object (point-min) t nil)
	  (progn
	    (forward-char (length m2-object))
	    (let (m2-start)
	      (setq m2-start (point))
	      (while (looking-at "[A-Za-z0-9]")
		(forward-char 1))
	      (let (m2-end)
		(setq m2-end (point))
	      (goto-char m2-insert)
	      (insert (buffer-substring m2-start m2-end)))))
      (progn
	(message "cannot complete: %s" m2-object)))))
  (while (looking-at "[A-Za-z0-9]")
    (forward-char 1)))

(defun m2-find-declaration (m2-object)
  "searches for object locally and then searches externally for object."
  (interactive)
  (let (source-window)
    (let (source-buffer)
      (setq source-window (selected-window))
      (setq source-buffer (current-buffer))
      (delete-other-windows)
      (split-window)
      (if (m2-find-declaration-procedure m2-object)
	  (progn
	    (m2-move-to-procedure-start)
	    (other-window 1)
	    t)
	(if (m2-find-type-or-var m2-object)
	    (progn
	      (m2-move-to-type-or-var-start)
	      (other-window 1)
	      t)
	  (if (m2-find-export-declaration m2-object ".mod")
	      (if (m2-find-declaration m2-object)
                 (progn
		  (switch-to-buffer source-buffer)
		  t)
		(progn
		  (delete-other-windows)
		  (select-window source-window)
		  (switch-to-buffer source-buffer)
		  (split-window)
		  (if (m2-find-export-declaration m2-object ".def")
		      (progn
			(m2-find-declaration m2-object)
			t)
		    (progn
		      (message "not found in the definition module")
		      (sit-for 1)
		      nil))))
	    (progn
	      (message "hmm should not get here m2-find-declaration")
	      (sit-for 1)
	      nil)))))))

(defun m2-find-type-or-var (m2-object)
  "searches for an object defined as CONST, TYPE or VAR."
  (interactive)
  (goto-char (point-min))
  (let (m2-value)
    (setq m2-value nil)
    (while (and (not m2-value)
		(re-search-forward "TYPE\\|CONST\\|VAR" nil t))
      (progn
	(m2-forward-to-token)
	(while (and (not (looking-at "BEGIN\\|END"))
		    (not (looking-at m2-object)))
	  (m2-forward-to-token))
	(if (looking-at m2-object)
	    (progn
	      (m2-forward-to-token)
	      (if (looking-at "[=:,]")
		  (setq m2-value t))))))
    m2-value))

(defun m2-move-to-type-or-var-start ()
  "moves to the start of the CONST, TYPE or VAR declaration."
  (beginning-of-line)
  (let (m2-point)
    (setq m2-point (point))
    (skip-chars-backward " \t\n\f" (point-min))
    (re-search-backward "TYPE\\|CONST\\|VAR" nil t)
    (set-window-start (selected-window) (point))
    (goto-char m2-point)))

(defun m2-move-to-procedure-start ()
  "moves to the start of the procedure implementation (before the
   comments start - if they exist)."
  (beginning-of-line)
  (let (m2-point)
    (setq m2-point (point))
    (skip-chars-backward " \t\n\f" (point-min))
    (if (and (> (point) 2)
	     (save-excursion (forward-char -2) (looking-at "\\*)")))
	(progn
	  (m2-run-back-over-comments (point-min))
	  (forward-char 1)))
    (set-window-start (selected-window) (point))))

(defun m2-find-declaration-procedure (m2-object)
  "attempts to find the declaration of m2-object as a procedure."
  (interactive)
  (goto-char (point-min))
  (let (m2-value)
    (setq m2-value nil)
    (setq m2-object (concat m2-object " "))
    (while (and (not m2-value)
		(re-search-forward "PROCEDURE" nil t))
      (progn
	(m2-forward-to-token)
	(if (and (looking-at m2-object) (not (m2-is-forward-declaration)))
	    (setq m2-value t))))
    m2-value))

(defun m2-is-forward-declaration ()
  "returns true if this procedure heading is just a FORWARD declaration
   of a implementation further down the file."
  (interactive)
  (save-excursion
    (m2-forward-to-token)
    (if (looking-at "(")
	(progn
	  (while (and (< (point) (point-max))
		      (not (looking-at ")")))
	    (m2-forward-to-token))))
    (while (and (< (point) (point-max))
		(not (looking-at ";")))
      (m2-forward-to-token))
    (m2-forward-to-token)
    (looking-at "FORWARD")))

(defun m2-find-export-declaration (m2-object m2-extension)
  "scans the export list of the current module for m2-object.
   If m2-object is found then the appropriate module with
   m2-extension is opened and the cursor is placed at the
   start of the declaration."
  (interactive)
  (goto-char (point-min))
  (let (m2-success)
    (setq m2-success nil)
    (let (m2-module-name)
      (let (m2-continue)
	(setq m2-module-name nil)
	(setq m2-continue t)
	(while (and (< (point) (point-max))
		    (not m2-success)
		    m2-continue
		    (not (m2-indent-commencer)))
	  (progn
	    (if (re-search-forward "[ ;\n\t]\\(BEGIN\\|FINALLY\\|EXCEPT\\|CONST\\|TYPE\\|VAR\\|FROM\\|PROCEDURE\\)" nil t)
		(progn
		  (forward-char -1)
		  (while (looking-at "[A-Za-z0-9]")
		    (forward-char -1))
		  (forward-char 1)))
	    (if (looking-at "FROM")
		(progn
		  (m2-forward-until-white (point-max))
		  (m2-forward-to-token)
		  (let (m2-start)
		    (setq m2-start (point))
		    (while (looking-at "[A-Za-z0-9]")
		      (forward-char 1))
		    (setq m2-module-name (buffer-substring m2-start (point)))
		    (setq m2-success (m2-found-import-ident m2-object))
		    (if m2-success
			(progn
			  (message "Found %s in module <%s>" m2-object m2-module-name)
			  (m2-find-module (concat m2-module-name m2-extension))))))
	      (progn
		(setq m2-continue (not (m2-indent-commencer)))
		(m2-forward-until-white (point-max))
		(m2-forward-to-token)))))))
    m2-success))

(defun m2-found-import-ident (m2-object)
  "scans the import current list for m2-object.
   If m2-object is found then true is returned."
  (interactive)
  (let (m2-success)
    (setq m2-success nil)
    (while (and (not (looking-at ";\\|BEGIN\\|CONST\\|TYPE\\|VAR\\|FROM\\|PROCEDURE"))
		(not m2-success))
      (setq m2-success (looking-at m2-object))
      (m2-forward-to-token))
    m2-success))

(defun m2-help ()
  "displays the help buffer."
  (interactive)
  (let (m2-buffer)
    (setq m2-buffer (current-buffer))
    (if (get-buffer "*Modula-2-Help*")
	(kill-buffer "*Modula-2-Help*"))
    (switch-to-buffer "*Modula-2-Help*")
    
    (insert
"This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing
Control-C followed by the first character of the construct.

  Control-c b begin         Control-c c case
  Control-c d definition    Control-c e else
  Control-c f for           Control-c h header
  Control-c i if            Control-c m module
  Control-c l loop          Control-c o or
  Control-c p procedure     Control-c Control-w with
  Control-c r record        Control-c s stdio
  Control-c t type          Control-c u until
  Control-c v var           Control-c w while
  Control-c x export        Control-c y import
  Control-c Control-d debug             Control-c Control-t toggle def/mod
  Control-c Control-c compile           Control-x ` next-error
  Control-c Control-l link              Esc .   find declaration
  Control-c Control-h help              Esc \\t  complete name
  Control-c Control-a generate assembler for a source line/function

   m2-compile-command holds the command to compile a Modula-2 program.
   m2-link-command holds the command to link a Modula-2 program.

   Your modula-2 default library search path is: " m2-path)
    (insert "\n   Your modula-2 options are: " m2-options)
    (toggle-read-only)
    (goto-char (point-min))
    (delete-other-windows)
    (split-window)
    (switch-to-buffer m2-buffer)))

(defun m2-newline ()
  "insert a newline and tab to the correct indent."
  (interactive)
  (end-of-line)
  (insert "\n")
  (m2-tab))

(defun m2-begin ()
  "Insert a BEGIN keyword and indent for the next line."
  (interactive)
  (insert "BEGIN")
  (m2-newline)
  (m2-tab))

(defun m2-case ()
  "Build skeleton CASE statment, prompting for the <expression>."
  (interactive)
  (insert "CASE " (read-string "expression: ") " OF")
  (m2-tab)
  (m2-newline)
  (insert "\nEND")
  (m2-tab))

(defun m2-fsf-copyright ()
  "emit the a copyright notice providing m2-assign-future is set."
  (if m2-assign-future
      (insert "(* Copyright (C) 2009 Free Software Foundation, Inc. *)\n")))

(defun m2-fsf-gpl-notice ()
  "emit the fsf gpl notice at the relevant position."
  (insert "(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)\n\n"))

(defun m2-fsf-lgpl-notice ()
  "emit the fsf notice at the relevant position."
  (insert "(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)\n\n"))

(defun m2-fsf-notice ()
  "if the m2-assign-future is set then prompt the user for 'g'pl or 'l'gpl."
  (interactive)
  (if m2-assign-future
      (let ((type (read-string "which kind of license (g)GPL or (l)LGPL? ")))
	(if (string-equal type "l")
	    (m2-fsf-lgpl-notice)
	  (m2-fsf-gpl-notice)))))
  
(defun m2-definition ()
  "Build skeleton DEFINITION MODULE, prompting for the <module name>."
  (interactive)
  (m2-fsf-copyright)
  (m2-fsf-notice)
  (insert "DEFINITION MODULE ")
  (insert (substring (buffer-name) 0 -4) " ;\n")
  (insert "\n\n\nEND " (substring (buffer-name) 0 -4) ".\n")
  (previous-line 3)
  (m2-header))

(defun m2-else ()
  "Insert ELSE keyword and indent for next line."
  (interactive)
  (insert "ELSE")
  (m2-tab)
  (m2-newline))

(defun m2-for ()
  "Build skeleton FOR loop statment, prompting for the loop parameters."
  (interactive)
  (insert "FOR " (read-string "init: ") " TO " (read-string "end: "))
  (let ((by (read-string "by: ")))
    (if (not (string-equal by ""))
	(insert " BY " by)))
  (insert " DO")
  (m2-tab)
  (m2-newline)
  (insert "\nEND")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "(*\n    Title      : ")
  (if (or (string-equal (substring (buffer-name) -4) ".def")
	  (string-equal (substring (buffer-name) -4) ".mod"))
      (insert (substring (buffer-name) 0 (- (length (buffer-name)) 4)))
    (insert (buffer-name)))
  (insert "\n    Author     : ")
  (insert (user-full-name))
;;;  (insert (concat "\n\t\t<" (user-login-name) "@" (system-name) ">\n"))
  (insert "\n    System     : GNU Modula-2")
  (insert "\n    Date       : ")
  (insert (current-time-string))
  (insert "\n    Revision   : $Version$ ")
  (insert "\n    Description: ")
  (insert "\n*)\n\n")
  (previous-line 3)
  (end-of-line))

(defun m2-if ()
  "Insert skeleton IF statment, prompting for <boolean-expression>."
  (interactive)
  (m2-tab)
  (if m2-if-then-same-line
      (insert "IF " (read-string "<boolean-expression>: ") " THEN")
    (progn
      (insert "IF " (read-string "<boolean-expression>: ") )
      (insert "\nTHEN")))
  (m2-tab)
  (m2-newline)
  (insert "\nEND")
  (m2-tab)
  (previous-line 1))

(defun m2-loop ()
  "Build skeleton LOOP (with END)."
  (interactive)
  (m2-tab)
  (insert "LOOP")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-module ()
  "Build skeleton MODULE, prompting for definition, implementation or module."
  (interactive)
  (m2-fsf-copyright)
  (m2-fsf-notice)
  (let ((type (read-string "(i)mplementation or (m)odule: ")))
    (if (string-equal type "i")
	(insert "IMPLEMENTATION "))
    (insert "MODULE " (substring (buffer-name) 0 -4) " ;\n\n")
    (insert "\n\n")
    (if (string-equal type "m")
	(insert "BEGIN\n\n"))
    (insert "END " (substring (buffer-name) 0 -4) ".\n"))
  (previous-line 3))

(defun m2-or ()
  (interactive)
  (m2-newline)
  (insert "OR")
  (m2-newline)
  (m2-tab))

(defun m2-procedure ()
  (interactive)
  (insert "(*\n")
  (m2-indent-comment)
  (let ((name (read-string "Procedure name: " ))
	args)
    (insert name " - \n")
    (insert "*)")
    (m2-indent-comment)
    (end-of-line)
    (insert "\n\n")
    (insert "PROCEDURE ")
    (let (results)
      (setq args (read-string "Arguments: "))
      (setq results (read-string "Result Type: "))
      (insert name)
      (if (or (not (string-equal args ""))
	      (not (string-equal results "")))
	  (insert " (" args ")"))
      (if (not (string-equal results ""))
	  (insert " : " results))
      (insert " ;")
      (m2-newline)
      (insert "BEGIN")
      (m2-newline)
      (m2-newline)
      (insert "END ")
      (insert name)
      (insert " ;\n\n")
      (previous-line 2)
      (m2-tab)
      (previous-line 1)
      (m2-tab)
      (previous-line 5)
      (end-of-line))))

(defun m2-with ()
  (interactive)
  (m2-tab)
  (insert "WITH ")
  (insert (read-string "Designator: "))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-elsif ()
  (interactive)
  (m2-tab)
  (insert "ELSIF ")
  (insert (read-string "<boolean expression>: "))
  (end-of-line)
  (m2-tab)
  (insert "\nTHEN")
  (m2-tab)
  (m2-newline))

(defun m2-record ()
  (interactive)
  (m2-tab)
  (insert "RECORD")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-stdio ()
  (interactive)
  (insert "
FROM StrIO IMPORT WriteString, ReadString, WriteLn ;
FROM StdIO IMPORT Write, Read ;
"))

(defun m2-type ()
  (interactive)
  (insert "TYPE")
  (m2-newline)
  (m2-tab))

(defun m2-until ()
  (interactive)
  (insert "REPEAT")
  (m2-tab)
  (m2-newline)
  (m2-newline)
  (m2-tab)
  (insert "UNTIL ")
  (insert (read-string "<expression>: "))
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-var ()
  (interactive)
  (m2-newline)
  (insert "VAR")
  (m2-newline)
  (m2-tab))

(defun m2-while ()
  (interactive)
  (m2-tab)
  (insert "WHILE ")
  (insert (read-string "<expression>: "))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-export ()
  (interactive)
  (insert "EXPORT QUALIFIED "))

(defun m2-import ()
  (interactive)
  (insert "FROM ")
  (insert (read-string "Module: "))
  (insert " IMPORT "))

(defun m2-debug ()
  (interactive)
  (gdb m2-debug-command))

(defun m2-compile ()
  (interactive)
  (compile compile-command))

(defun m2-assembler ()
  (interactive)
  (let (source-window)
    (let (source-buffer)
      (let (source-line)
        (let (m2-file)
          (setq m2-file (concat (substring (buffer-name) 0 -4) ".s"))
          (setq source-line (substring (what-line) 5 (length (what-line))))
          (delete-other-windows)
	  (split-window)
	  (if (not (or (get-buffer m2-file)
		       (find-file  m2-file)))
	      (progn
		(let (old-compile-command)
		  (setq old-compile-command compile-command)
		  (compile (generate-assembler-command m2-file))
		  (setq compile-command old-compile-command)))
	    (m2-search-for-line m2-file source-line)))))))

(defun m2-search-for-line (m2-file source-line)
  "search for the source code equivalent, source-line, in assembly file, m2-file"
  (interactive)
  (save-excursion
    (if (switch-to-buffer m2-file)
	(if (search-forward (concat ".stabn 68,0," source-line))
	    (progn
	      (beginning-of-line)
	      (while (and (< (point) (point-max))
			  (looking-at "^\\."))
		(next-line 1))))))
  (message "failed to compile module %s" (buffer-name)))

(defun generate-assembler-command (assembly-file)
  "generate the compilation string which will generate, assembly-file"
  (interactive)
  (if (string-equal "gm2" (substring compile-command 0 3))
      (concat "gm2 -S -g " (substring compile-command 4 (length compile-command)))
    ""))

(defun m2-link ()
  (interactive)
  (let (modulename)
    (setq modulename (buffer-name))
    (cond ((string-equal (substring (buffer-name) -4) ".def")
	   (compile (concat m2-link-command " "
			    (substring (buffer-name) 0 -4))))
	  ((string-equal (substring (buffer-name) -4) ".mod")
	   (compile (concat m2-link-command " "
			    (substring (buffer-name) 0 -4)))))))

(defun execute-monitor-command (command)
  (let* ((shell shell-file-name)
	 (csh (equal (file-name-nondirectory shell) "csh")))
    (call-process shell nil t t "-cf" (concat "exec " command))))

(defun m2-visit ()
  (interactive)
  (let (modulename)
    (save-excursion
      (setq modulename
	    (read-string "Module name: "))
      (m2-find-module modulename))))

(defun m2-find-module (name)
  "attempts to find module, name, by searching the directories
   determined by m2-path."
  (let (m2-len)
    (setq m2-len 0)
    (let (m2-start)
      (setq m2-start 0)
      (let (m2-found)
	(setq m2-found nil)
	(let (m2-file)
	  (setq m2-file nil)
	  (aref "abcdefg" 1)
	  (while (and (<= m2-len (length m2-path))
		      (not m2-found))
	    (if (or (= m2-len (length m2-path))
		    (= (aref m2-path m2-len) ?\ ))
		(progn
		  (if (>= m2-len (length m2-path))
		      (setq m2-file (concat
				     (concat (substring m2-path m2-start nil) "/")
  	       			     name))
		    (setq m2-file (concat
				   (concat (substring m2-path m2-start m2-len) "/")
				   name)))
		  (setq m2-len (+ m2-len 1))
		  (setq m2-start m2-len)
		  (save-excursion
		    (setq m2-found (file-exists-p m2-file))))
	      (setq m2-len (+ m2-len 1))))
	  (if m2-found
	      (find-file m2-file))
	  m2-found)))))

(defun m2-toggle ()
  "Toggle between .mod and .def files for the module."
  (interactive)
  (cond ((string-equal (substring (buffer-name) -4) ".def")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4) ".mod")))
	((string-equal (substring (buffer-name) -4) ".mod")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4)  ".def")))
	((string-equal (substring (buffer-name) -3) ".mi")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".md")))
	((string-equal (substring (buffer-name) -3) ".md")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".mi")))))

(defun m2-tab ()
  "tab moves to the correct indentation for the current line."
  (interactive)
  (let (m2-point)
    (if (and (> (point) 1)
	     (save-excursion (forward-char -1) (looking-at "(\\*")))
	(progn
	  (setq m2-point 1)
	  (forward-char -1))
      (setq m2-point 0))
    (if (m2-in-parameter)
	(progn
	  (message "Formatting parameter block")
	  (m2-format-expression))
      (if (m2-in-expression)
	  (progn
	    (message "Formatting expression")
	    (m2-format-expression))
	(m2-indent))
      (forward-char m2-point))))

(defun m2-in-parameter ()
  "returns true if the cursor is currently inside a parameter block."
  (interactive)
  (let (m2-point)
    (setq m2-point (point))
    (save-excursion
      (m2-backward-to-token)
      (if (looking-at "PROCEDURE[ \n]")
	  nil
	(while (and (> (point) 1) (not (m2-is-parameter-commencer))
		    (not (m2-is-parameter-terminator)))
	  (m2-backward-to-token))
	(if (looking-at "PROCEDURE[ \n]")
	    (progn
	      (goto-char m2-point)
	      (while (not (m2-is-parameter-terminator))
		(m2-forward-to-token))
	      (looking-at ")"))
	  nil)))))

(defun m2-is-parameter-commencer ()
  "returns true if we are looking at a parameter block commencer"
  (looking-at "PROCEDURE[ \n]"))

(defun m2-is-parameter-terminator ()
  "returns true if we are looking at a parameter block terminator"
  (looking-at ")\\|BEGIN\\|IF\\|THEN\\|ELSE\\|END\\|WHILE\\|REPEAT\\|UNTIL\\|LOOP\\|CONST|\\MODULE"))

(defun m2-indent-comment ()
  "moves cursor to the current indentation of the comment."
  (interactive)
  (let (m2-point-end)
    (let (m2-indent)
      (let (m2-start-line)
	(save-excursion
	  (save-excursion
	    (beginning-of-line)
	    (setq m2-start-line (point)))
	  (m2-backward-to-comment-start)
	  (setq m2-point-end (point))
	  (beginning-of-line)
	  (setq m2-indent (- m2-point-end (point))))
	(m2-remove-leading-spaces)
	(if (and (not (looking-at "\\*)"))
		 (>= m2-start-line m2-point-end))
	    (setq m2-indent (+ m2-indent m2-indent-level)))
	(m2-create-indent m2-indent)))))

(defun m2-backward-to-comment-start ()
  "moves back to the start of a comment."
  (interactive)
  (let (m2-point)
    (let (carry-on)
      (if (and (> (point) 1)
	       (looking-at "\\*)"))
	  (forward-char -1))
      (setq m2-point (point))
      (setq carry-on t)
      (while carry-on
	(setq m2-point (point))
	(if (and (>= (point) 3)
		 (save-excursion
		   (forward-char -2)
		   (looking-at "\\*)")))
	    (m2-run-back-over-comments 1))
	(if (> (point) 1)
	    (forward-char -1))
	(setq carry-on (and (> (point) 1)
			    (not (looking-at "(\\*"))))))))
    
(defun m2-indent ()
  "calculates the indentation for the current line."
  (interactive)
  (let (m2-point)
    (save-excursion
      (m2-remove-leading-spaces))
    (setq m2-point (point))
    (beginning-of-line)
    (setq m2-point (- m2-point (point)))
    (m2-create-indent (m2-calculate-indent))
    (while (> m2-point 0)
      (forward-char 1)
      (setq m2-point (- m2-point 1)))))

(defun m2-remove-leading-spaces ()
  "removes any leading spaces in a source line."
  (interactive)
  (beginning-of-line)
  (if (looking-at "[\t| ]")
      (progn (delete-char 1)
	     (m2-remove-leading-spaces))))

(defun m2-create-indent (num)
  "generates num indent."
  (interactive)
  (while (> num 0)
    (progn
      (insert " ")
      (setq num (- num 1)))))

(defun m2-calculate-indent ()
  "calculates the indentation required for the current source line."
  (interactive)
  (let ((m2-level)
	(m2-found-record))
    (if (m2-indent-terminator)
	(setq m2-level (- m2-indent-level))
      (setq m2-level 0))
    (save-excursion
      (m2-backward-to-token)
      (while (and (> (point) 1)
		  (or (and (m2-in-parameter)
			   (looking-at "VAR"))
		      (not (or (m2-indent-terminator)
			       (m2-indent-commencer)
			       (looking-at "CASE")))))
	(m2-backward-to-token))
      (if (looking-at "CASE")
	  (setq m2-level  0)
	(if (m2-indent-commencer)
	    (setq m2-level  (+ m2-level m2-indent-level))))
      (setq m2-found-record nil)
      (if (looking-at "END")
	  (progn
	    (save-excursion
	      (m2-match-end)
	      (if (looking-at "RECORD")
		  (progn
		    (setq m2-level (+ m2-level (m2-calculate-indent)))
		    (setq m2-found-record t))))))
      (if (not m2-found-record)
	  (progn
	    (setq m2-level (+ m2-level (point)))
	    (beginning-of-line)
	    (setq m2-level (- m2-level (point))))))
    m2-level))

(defun m2-in-expression ()
  "returns true if we are currently inside a modula-2 expression."
  (interactive)
  (let (m2-level)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[\t ]")
	  (progn
	    (m2-forward-to-token)
	    (if (looking-at ")")
		(progn
		  (forward-char -1)
		  (message "found )")))))
      (while (and (> (point) 1)
		  (not (or (m2-expression-commencer)
			   (m2-expression-terminator))))
	(if (looking-at ")")
	    (m2-skip-brackets ")" "(")
	  (m2-backward-to-token)))
      (setq m2-level (and (not (looking-at "(\\*"))
			  (m2-expression-commencer))))
    m2-level))

(defun m2-skip-brackets (m2-open m2-close)
  "skips the brackets () it stops at the next matching bracket."
  (interactive)
  (m2-backward-to-token)
  (while (and (> (point) 1)
	      (not (or (m2-expression-terminator)
		       (looking-at m2-close))))
    (if (looking-at m2-open)
	(m2-skip-brackets m2-open m2-close)
      (m2-backward-to-token))))

(defun m2-expression-backstop ()
  "returns true if we are looking at a Modula-2 keyword behind which
   an expression cannot exist."
  (interactive)
  (or (looking-at "UNTIL")
      (looking-at "WHILE")
      (looking-at "IF")
      (looking-at ":=")
      (looking-at "TO")
      (looking-at "(\\*")
      (looking-at "FOR")))

(defun m2-expression-commencer ()
  "returns true if we are currently looking at an expression commencer."
  (interactive)
  (looking-at "[-+*=(<>/#[]"))
;;; be VERY careful if you add anything to the above regexp
;;; test it against:
;;;    a := = b + - c * d : ; ) ( , < > . [ ] # / !

(defun m2-expression-terminator ()
  "returns true if we are currently looking at an expression terminator."
  (interactive)
  (or (looking-at "THEN")
      (looking-at "ELSE")
      (looking-at "ELSIF")
      (looking-at "END")
      (looking-at "VAR")
      (looking-at "TYPE")
      (looking-at "CONST")
      (looking-at "BEGIN")
      (looking-at "WITH")
      (looking-at "RECORD")
      (looking-at "PROCEDURE")
      (looking-at "OF")
      (looking-at "DO")
      (looking-at "[;:]")))

(defun m2-format-expression ()
  "formats the current expression."
  (interactive)
  (beginning-of-line)
  (let (m2-spaces)
    (let (m2-cursor)
      (let (m2-end)
	(let (m2-start)
	  (let (m2-start-exp)
	    (let (m2-next-tok)
	      (setq m2-cursor (point))
	      (save-excursion
		(m2-find-beginning-of-expression)
		(setq m2-start-exp (point))
		(save-excursion
		  (beginning-of-line)
		  (setq m2-start (point))
		  (end-of-line)
		  (setq m2-end (point)))
		(m2-forward-to-token)
		(setq m2-next-tok (point)))
	      (if (> m2-next-tok m2-end)
		  (progn
		    (setq m2-spaces (+ (- m2-start-exp m2-start) 1))
		    (goto-char m2-cursor)
		    (beginning-of-line)
		    (m2-remove-leading-spaces)
		    (if (looking-at "[\])]")
			(progn
			  (setq m2-spaces (- m2-start-exp m2-start))))
		    (if (or (m2-indent-commencer)
			    (m2-indent-terminator))
			(m2-indent)
		      (m2-create-indent m2-spaces)))
		(progn
		  (m2-remove-leading-spaces)
		  (if (looking-at "[\])]")
		      (progn
			(m2-create-indent (- m2-start-exp m2-start)))
		    (m2-create-indent (- m2-next-tok m2-start))))))))))))

(defun m2-find-beginning-of-expression ()
  "positions the cursor on the start of the current subexpression."
  (interactive)
  (let (m2-level)
    (setq m2-level 1)
    (while (and
	    (not (and (= m2-level 0)
		      (m2-expression-commencer)))
	    (not (m2-expression-backstop)))
      (m2-backward-to-token)
      (if (looking-at ")")
	  (setq m2-level (+ m2-level 1))
	(if (looking-at "(")
	    (setq m2-level (- m2-level 1)))))))

(defun m2-delete-indent-spaces ()
  "removes 3 spaces from the beginning of the line if they exist."
  (interactive)
  (m2-delete-space)
  (m2-delete-space)
  (m2-delete-space))

(defun m2-delete-space ()
  "removes a space from the beginning of the line if one exists."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\t")
	(progn
	  (delete-char 1)
	  (insert "       "))
      (if (looking-at " ")
	  (delete-char 1)))))
	  
(defun m2-old-calculate-indent ()
  "calculates the indentation required for the current source line."
  (interactive)
  (let (m2-level)
    (save-excursion
      (setq m2-level 0)
      (while (> (point) 1)
	(if (m2-indent-terminator)
	    (setq m2-level (- m2-level 1)))
	(m2-backward-to-token)
	(if (m2-indent-commencer)
	    (setq m2-level (+ m2-level 1)))))
    (while (> m2-level 0)
      (progn
	(insert "   ")
	(setq m2-level (- m2-level 1))))))

(defun m2-indent-commencer ()
  "returns true if a token representing the beginning of an indent sequence was found."
  (or (and
       m2-if-then-same-line
       (looking-at "IF"))
      (and
       (not m2-if-then-same-line)
       (looking-at "THEN"))
      (looking-at "BEGIN")
      (looking-at "FINALLY")
      (looking-at "EXCEPT")
      (looking-at "RECORD")
      (looking-at "FOR")
      (looking-at "CONST")
      (looking-at "TYPE")
      (looking-at "VAR")
      (looking-at "WITH")
      (looking-at "LOOP")
      (looking-at "REPEAT")
      (looking-at "ELSE")
      (looking-at "WHILE")))

(defun m2-indent-terminator ()
  "returns true if a token representing the end of an indent sequence was found."
  (or (looking-at "END")
      (looking-at "BEGIN")
      (looking-at "FINALLY")
      (looking-at "EXCEPT")
      (looking-at "CONST")
      (looking-at "TYPE")
      (looking-at "VAR")
      (looking-at "ELSE")
      (looking-at "ELSIF")
      (looking-at "UNTIL")))

(defun m2-local-recompile ()
 "recompile the g-mode and load the file"
 (interactive)
 (byte-compile-file (concat (getenv "HOME") "/m2/comp/el/g-mode.el"))
 (read-abbrev-file  (concat (getenv "HOME") "/m2/comp/el/g-mode.elc"))
 (message "Compilation complete"))

(defvar g-mode-syntax-table nil
  "Syntax table in use in g-mode buffers.")

(if g-mode-syntax-table
    ()
  (setq g-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" g-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" g-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" g-mode-syntax-table)
  (modify-syntax-entry ?+ "." g-mode-syntax-table)
  (modify-syntax-entry ?- "." g-mode-syntax-table)
  (modify-syntax-entry ?= "." g-mode-syntax-table)
  (modify-syntax-entry ?% "." g-mode-syntax-table)
  (modify-syntax-entry ?< "." g-mode-syntax-table)
  (modify-syntax-entry ?> "." g-mode-syntax-table)
  (modify-syntax-entry ?& "." g-mode-syntax-table)
  (modify-syntax-entry ?| "." g-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" g-mode-syntax-table))

(defun m2-recursive (lim)
  "test my understanding of local bindings!"
  (interactive)
  (let (m2-level)
    (setq m2-level lim)
    (sit-for 1)
    (message "lim value is now %d" m2-level)
    (if (> m2-level 1)
	(progn
	  (setq m2-level (- m2-level 1))
	  (m2-recursive m2-level)
	  (sit-for 1)
	  (message "returned and value is %d" m2-level)))))

(defun m2-test-end ()
  "check to see whether END has been typed"
  (interactive)
  (insert "D")
  (setq case-fold-search nil)
  (if (and (not (m2-is-in-string))
	   (> (point) 4))
      (save-excursion
	(forward-char -4)
	(if (looking-at "[; \t\n]END[; \t\n]")
	    (progn
	      (m2-found-end))))))

(defun m2-found-end ()
  "found END now attempt to line up code"
  (interactive)
  (if m2-auto-indent-on-end
      (m2-tab))
  (save-excursion
    (if (m2-match-end)
	(m2-display-match (point))
      (message "no matching statement found"))))

(defun m2-test-then ()
  "check to see whether THEN has been typed"
  (interactive)
  (insert "N")
  (setq case-fold-search nil)
  (if (and (not (m2-is-in-string))
	   (> (point) 5))
      (save-excursion
	(forward-char -5)
	(if (looking-at "[; \t\n]THEN[; \t\n]")
	    (m2-found-then)))))

(defun m2-found-then ()
  "found THEN now attempt to line up code"
  (interactive)
  (if m2-auto-indent-on-then
      (m2-tab)))

(defun m2-test-else ()
  "check to see whether ELSE has been typed"
  (interactive)
  (insert "E")
  (setq case-fold-search nil)
  (if (and (not (m2-is-in-string))
	   (> (point) 5))
      (save-excursion
	(forward-char -5)
	(if (looking-at "[; \t\n]ELSE[; \t\n]")
	    (m2-found-else)))))

(defun m2-found-else ()
  "found ELSE now attempt to line up code"
  (interactive)
  (if m2-auto-indent-on-else
      (m2-tab)))

(defun m2-local-test ()
 "simple test hook."
 (interactive)
 (let ((m2-success (m2-testing)))
   (if (numberp m2-success)
       (message "number found")
     (if (stringp m2-success)
	 (message "Error found missing: %s" m2-success)
       (if (bufferp m2-success)
	   (message "buffer found")
	 (if (symbolp m2-success)
	     (if m2-success
		 (message "No error found")
	       (message "FALSE returned %s" m2-success))))))))

(defun m2-testing ()
  "simple function which returns a number of different symbols"
  nil
)

(defun m2-match-end ()
  "finds the start of the statement matching the END returns true if found."
  (interactive)
  (let (m2-level)
    (let (beginning)
      (setq m2-level 1)
      (while (and (> m2-level 0) (> (point) 1))
	(re-search-backward "[ ;\n\t]\\(END\\|IF\\|LOOP\\|WITH\\|WHILE\\|RECORD\\|CASE\\|FOR\\|MODULE\\|PROCEDURE\\)" nil t)
	(forward-char 1)
	(if (not (m2-is-in-string))
	    (if (looking-at "END")
		(setq m2-level (+ m2-level 1))
	      (setq m2-level (- m2-level 1)
		    beginning (point)))))
      (= m2-level 0))))

(defun m2-end-commencer ()
  "returns true if a token representing the beginning of an END was found."
  (and (> (point) 1)
       (or (save-excursion (forward-char -1) (looking-at "[ ;\n\t]IF"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]LOOP"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]WITH"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]WHILE"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]RECORD"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]CASE"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]FOR"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]MODULE"))
	   (save-excursion (forward-char -1) (looking-at "[ ;\n\t]PROCEDURE")))))

(defun m2-is-end()
  "returns true if END is found."
  (and (> (point) 1)
       (save-excursion (forward-char -1) (looking-at "[ ;\n\t]END"))))

(defun m2-display-match (m2-beginning)
  "displays a match"
  (interactive)
  (if (<= (window-start) m2-beginning)
      (progn
	(goto-char m2-beginning)
	(sit-for 1))
    (progn
      (save-excursion
	(let (m2-start)
	  (let (m2-end)
	    (beginning-of-line)
	    (setq m2-start (point))
	    (end-of-line)
	    (setq m2-end (point))
	(message "matches: %s" (buffer-substring m2-start m2-end))))))))

(defun m2-backward-to-token ()
  "moves back to the beginning of the last token"
  (interactive)
  (if (> (point) 1)
      (progn
	(forward-char -1)
	(while (or (and (> (point) 1)
			(save-excursion (forward-char -1) (looking-at "\\*)")))
		   (looking-at "[ \t\n]"))
	  (if (looking-at "[ \t\n]")
	      (forward-char -1)
	    (progn
	      (m2-backward-to-comment-start)
	      (if (> (point) 1)
		  (forward-char -1)))))
	(if (save-excursion (forward-char -1) (or (looking-at ":=")
						  (looking-at "<=")
						  (looking-at ">=")
						  (looking-at "\\.\\.")
						  (looking-at "<>")))
	    (forward-char -1)
	  (if (looking-at "'")
	      (m2-move-backward-over-quotes)
	    (if (looking-at "[0-9a-zA-Z]")
		(progn
		  (while (and (> (point) 1)
			      (looking-at "[0-9a-zA-Z]"))
		    (forward-char -1))
		  (if (> (point) 1)
		      (forward-char 1)))))))))

(defun m2-backward-until-ident-delimeter ()
  "moves the cursor back until an ident delimeter is found."
  (interactive)
  (while (and (> (point) 1)
	      (save-excursion
		(forward-char -1)
		(not (looking-at "[ \n\t=:,;]"))))
    (forward-char -1))
  (if (and (> (point) 1)
	   (save-excursion
	     (forward-char -1)
	     (looking-at "[=:,;]")))
      (forward-char -1)))

(defun m2-backward-until-white ()
  "moves the cursor back until white space is found."
  (interactive)
  (while (and (> (point) 1)
	      (save-excursion
		(forward-char -1)
		(not (looking-at "[ \t\n]"))))
    (forward-char -1)))

(defun m2-backward-to-noncomment (lim)
  "moves back to the start of a non comment text."
  (interactive)
  (let (carry-on)
    (setq carry-on t)
    (while carry-on
      (skip-chars-backward " \t\n\f" lim)
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*)")))
	  (m2-run-back-over-comments lim))
      (setq carry-on (and (> (point) lim)
			  (or (and (>= (point) (+ 2 lim))
				   (save-excursion (forward-char -2) (looking-at "\\*)")))
			      (save-excursion (forward-char -1) (looking-at "[\n\t ]"))))))))

(defun m2-run-back-over-comments (lim)
  "moves over a comment."
  (interactive)
  (let (m2-comment-level)
    (forward-char -2)
    (if (looking-at "(\\*")
	(setq m2-comment-level 1)
      (if (looking-at "\\*)")
	  (setq m2-comment-level -1)
	(message "run-over-comments assumes it is on a comment")))
    (forward-char -1)
    (while (and
	    (not (= m2-comment-level 0))
	    (> (point) lim))
      (if (looking-at "(\\*")
	  (setq m2-comment-level (+ m2-comment-level 1)))
      (if (looking-at "\\*)")
	  (setq m2-comment-level (- m2-comment-level 1)))
      (forward-char -1))))

(defun m2-is-in-string ()
  "returns true if we are in a string."
  (interactive)
  (let (m2-in-quotes)
    (let (m2-point)
      (setq m2-in-quotes nil)
      (setq m2-point (point))
      (save-excursion
	(beginning-of-line)
	(while (< (point) m2-point)
	  (if (looking-at "\"")
	      (setq m2-in-quotes (not (m2-run-over-double-quotes m2-point)))
	    (if (looking-at "'")
		(setq m2-in-quotes (not (m2-run-over-single-quotes m2-point)))))
	  (forward-char 1)))
      m2-in-quotes)))

(defun m2-run-over-double-quotes (m2-opoint)
  "returns true if we see a another double quote before m2-opoint."
  (interactive)
  (forward-char 1)
  (while (and (< (point) m2-opoint)
	      (not (looking-at "\"")))
    (forward-char 1))
  (and (looking-at "\"") (< (point) m2-opoint)))

(defun m2-run-over-single-quotes (m2-opoint)
  "returns true if we see a another single quote before m2-opoint."
  (interactive)
  (forward-char 1)
  (while (and (< (point) m2-opoint)
	      (not (looking-at "'")))
    (forward-char 1))
  (and (looking-at "'") (< (point) m2-opoint)))

(defun m2-is-in-comment ()
  "returns true if we are in a comment."
  (interactive)
  (let (m2-in-comment)
    (let (m2-point)
      (setq m2-point (point))
      (setq m2-in-comment nil)
      (save-excursion
	(goto-char (point-min))
	(while (< (point) m2-point)
	  (if (looking-at "\"")
	      (m2-run-over-double-quotes m2-point)
	    (if (looking-at "'")
	    (m2-run-over-single-quotes m2-point)
	    (if (looking-at "(\\*")
		(setq m2-in-comment (not (m2-run-forward-over-comments m2-point))))))
      (forward-char 1)))
  m2-in-comment)))
	 
(defun m2-run-forward-over-comments (lim)
  "moves over a comment and returns true if we are not in a comment"
  (interactive)
  (let (m2-comment-level)
    (if (looking-at "(\\*")
	(setq m2-comment-level 1)
      (if (looking-at "\\*)")
	  (setq m2-comment-level -1)
	(message "run-over-comments assumes it is on a comment")))
    (forward-char 1)
    (while (and
	    (not (= m2-comment-level 0))
	    (< (point) (- lim 1)))
      (if (looking-at "(\\*")
	  (setq m2-comment-level (+ m2-comment-level 1)))
      (if (looking-at "\\*)")
	  (setq m2-comment-level (- m2-comment-level 1)))
      (forward-char 1))
    (forward-char 1)
    (= m2-comment-level 0)))

(defun m2-forward-to-token ()
  "moves forward to the beginning of the next token"
  (if (looking-at "[][=,;)--+#\\*\\|\\^\\%\\$@]")
      (forward-char 1)
    (if (or (looking-at ":=")
	    (looking-at "<=")
	    (looking-at ">=")
	    (looking-at "\\.\\.")
	    (looking-at "<>"))
	(forward-char 2)
      (if (looking-at "[><:\\.]")
	  (forward-char 1)
	(if (looking-at "[A-Z|a-z|0-9]")
	    (while (looking-at "[A-Z|a-z|0-9]")
	      (forward-char 1))
	  (if (and (looking-at "(") (not (looking-at "(\\*")))
	      (forward-char 1)
	    (if (looking-at "'")
		(progn
		  (m2-move-over-quotes)
		  (forward-char 1))))))))
  (while (or (looking-at "(\\*")
	     (looking-at "[ \t\n]"))
    (m2-forward-to-noncomment (point-max))))

(defun m2-forward-until-white (lim)
  "moves the cursor forward until white space is found."
  (while (and (< (point) lim)
	      (save-excursion
		(forward-char 1)
		(not (looking-at "[ \t\n]"))))
    (forward-char 1)))

(defun m2-forward-until-non-white (lim)
  "moves the cursor forward until non white space is found."
  (while (and (< (point) lim)
	      (save-excursion
		(forward-char 1)
		(looking-at "[ \t\n]|(\\*")))
    (forward-char 1)))

(defun m2-forward-to-noncomment (lim)
  "moves forward to the start of a non comment text."
  (interactive)
  (let (m2-point)
    (let (m2-carry-on)
      (setq m2-point (point))
      (setq m2-carry-on (< (point) lim))
      (while m2-carry-on
	(skip-chars-forward "[ \t\n]" lim)
	(setq m2-point (point))
	(if (and (< (point) lim)
		 (looking-at "(\\*"))
	    (m2-run-forward-over-comments lim))
	(setq m2-carry-on (and (< (point) lim)
			       (or (looking-at "(\\*")
				   (looking-at "[\n\t ]")))))
      (< (point) lim))))

(defun m2-is-token (string)
  "returns true if we can see a token string. It does not eat up the token."
  (let (m2-point)
    (let (m2-found-noncom)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (if (looking-at string)
	      t
	    nil)
	(progn
	  (goto-char m2-point)
	  nil)))))

(defun m2-token-is (string)
  "returns true if we can see a token string. It does eat the token."
  (let (m2-point)
    (let (m2-found-noncom)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (if (looking-at string)
	      (progn
		(forward-char (length string))
		t)
	    nil)
	(progn
	  (goto-char m2-point)
	  nil)))))

(defun m2-ident ()
  "derived from EBNF for ident"
  (let (m2-point)
    (let (m2-found-noncom)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (progn
	    (if (and (looking-at "[A-Za-z][A-Za-z]*[0-9]*")
		     (progn (skip-chars-forward "[A-Za-z][A-Za-z]*[0-9]*")
			    (or (looking-at "[ \n\t]")
				(looking-at "(\\*"))))
		(progn
		  (skip-chars-forward "[A-Za-z][A-Za-z]*[0-9]*")
		  t)
	      "Identifier"))
	(goto-char m2-point)
	"Identifier"))))

(defun m2-string ()
  "derived from EBNF for a modula-2 string"
  (let (m2-found-noncom)
    (let (m2-point)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (if (looking-at "'")
	      (m2-move-over-quotes)
	    nil)
	(progn
	  (goto-char m2-point)
	  nil)))))

(defun m2-move-over-quotes ()
  "moves the cursor over the quoted string, nil is returned if
   the string encounters a newline before the quote."
  (let (m2-qpoint)
    (let (m2-carry-on)
      (setq m2-qpoint (point))
      (if (looking-at "'")
	  (progn
	    (forward-char 1)
	    (setq m2-carry-on (not (looking-at "['\n]")))
	    (while m2-carry-on
	      (progn
		(forward-char 1)
		(setq m2-carry-on (not (looking-at "['\n]")))))
	    (if (looking-at "\n")
		(progn
		  (goto-char m2-qpoint)
		  "missing end quote before newline")
	      t))
	(progn
	  (forward-char 1)
	  (set m2-carry-on (not (looking-at "[\"\n]")))
	  (while m2-carry-on
	    (progn
	      (forward-char 1)
	      (setq m2-carry-on (not (looking-at "[\"\n]")))))
	  (if (looking-at "\n")
	      (progn
		(goto-char m2-qpoint)
		"missing end quote before newline")
	    t))))))

(defun m2-move-backward-over-quotes ()
  "moves the cursor over the quoted string, nil is returned if
   the string encounters a newline before the quote."
  (let (m2-qpoint)
    (let (m2-carry-on)
      (setq m2-qpoint (point))
      (if (looking-at "'")
	  (progn
	    (forward-char -1)
	    (setq m2-carry-on (not (looking-at "['\n]")))
	    (while m2-carry-on
	      (progn
		(forward-char -1)
		(setq m2-carry-on (not (looking-at "['\n]")))))
	    (if (looking-at "\n")
		(progn
		  (goto-char m2-qpoint)
		  "missing end quote before newline")
	      t))
	(progn
	  (forward-char -1)
	  (setq m2-carry-on (not (looking-at "[\"\n]")))
	  (while m2-carry-on
	    (progn
	      (forward-char -1)
	      (setq m2-carry-on (not (looking-at "[\"\n]")))))
	  (if (looking-at "\n")
	      (progn
		(goto-char m2-qpoint)
		"missing end quote before newline")
	    t))))))

(defun interactive-blink-matching-open ()
  "Indicate momentarily the start of sexp before point."
  (interactive)
  (let ((blink-matching-paren-distance (buffer-size))
	(blink-matching-paren t))
    (blink-matching-open)))


(defun g-mode ()
  "Major mode for editing M2 code. User definable variables:
   m2-indent-level
      Indentation of M2 statements within surrounding block."
  (interactive)
  (kill-all-local-variables)
  (setup-g-mode-keys)
  (use-local-map g-mode-map)
  (setq major-mode 'g-mode)
  (setq mode-name "G-M-2")
  (setq local-abbrev-table g-mode-abbrev-table)
  (set-syntax-table g-mode-syntax-table)
  (setq case-fold-search nil)
  (setq indent-tabs-mode nil)
  (setq g-mode-hook
	'(lambda () (progn (make-local-variable 'compile-command)
			   (setq compile-command (concat m2-compile-command " " (concat (substring (buffer-name) 0 -4) ".mod"))))))
  (run-hooks 'g-mode-hook))
