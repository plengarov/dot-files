;;; cppdoc.el --- Emacs config for doxygen comments in cpp code.
;;
;; Copyright © 2020 Pavel Lengarov
;;
;; Author: Pavel Lengarov <pavel.lengarov@gmail.com>
;; URL:
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License:
;; This code is based on doxygen.el written by Tom Emerson.
;; forrow is doxygen.el Copyright ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doxygen.el --- doxygen-style comments

;; Copyright (C) 2000-2001 Basis Technology, Corp.

;; Author: Tom Emerson <tree@basistech.com>
;; Keywords: languages comments doxygen
;; Version: 1.1
;;

;; Copyright (c) 2020, Pavel Lengarov
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms,
;; with or without modification,
;; are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation and/or
;;   other materials provided with the distribution.
;; * the names of its contributors may be used to endorse or promote products
;;   derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Code:

(defcustom cppdoc-keymap-prefix nil
  "Cppdoc keymap prefix."
  :group 'cppdoc
  :type 'string)

(defun cppdoc-insert-member-comment ()
  "Insert a Doxygen member comment at point."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen)
      (insert "//!<")))
  (end-of-line))

(defun cppdoc-insert-comment ()
  "Insert a Doxygen comment at point."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen)
      (insert "//!")))
  (end-of-line))

(defun cppdoc-insert-block-comment ()
  "Insert a Doxygen comment for class at point."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (save-restriction
      (widen)
      (insert (concat "//! \n"
                      "/*! \n"
                      " *  \n"
                      " */"))))
  (end-of-line))

(defun cppdoc-insert-function-comment ()
  "Insert a Doxygen comment for the function at point."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen)
      (let ((start-column (current-column)))
        (let ((start (point)))
          (let ((data (find-arg-list)))
            (let ((args (cdr (assoc 'args data))))
              (goto-char start)
              (insert (concat "//! \n"
                              "/*! \n"
                              " *  \n"))
              (when args
                (insert-args args)))
            (let ((rtype (cdr (assoc 'return data))))
              (unless (string= "void" rtype)
                (insert " * \\return \n"))
              (insert "*/")))
          (let ((end (point)))
            (indent-region start end start-column)
            (untabify start end))))))
  (end-of-line))


(defun insert-args (args)
  "Insert Doxygen parameters comment for parameters in ARGS."
  (mapcar (lambda (x)
            (insert
             (format " * \\param %s\t\n"
                            (extract-arg-name x))))
          args))

(defun extract-arg-name (arg)
  "Get argument name from ARG string - (type arg-name)."
  (if (string-match "\\([a-zA-Z0-9_]+\\)\\s-*\\(=\\s-*.+\\s-*\\)?$" arg)
      (substring arg (match-beginning 1) (match-end 1))
    arg))

(defun find-arg-list ()
  "Extract various bits of information from a C or C++ function declaration."
  (interactive "*")
  (let ((return-type (find-return-type)))
    (save-excursion
      (if (re-search-forward (concat
                              "\\([a-zA-Z0-9_:]+\\)\\s-*("    ; function name
                              "\\([^)]*\\))")                 ; argument list
                             nil t)
          (let ((rtype return-type))
            (let ((func-name (buffer-substring (match-beginning 1)
                                               (match-end 1))))
              (let ((args (split-string
                           (buffer-substring (match-beginning 2)
                                             (match-end 2)) ",")))

                (list (cons 'return rtype)
                      (cons 'function func-name)
                      (cons 'args (if (string= "" (car args))
                                      nil
                                    args))))))
        nil))))

(defun find-return-type ()
  "Extract the return type of a function.
If the function is a constructor, it returns void."
  (interactive "*")
  (save-excursion
    (let ((start (point)))
      (search-forward "(")
      (let ((bound (point)))
        (goto-char start)
        (if (re-search-forward
             (concat
              "\\(virtual \|static \|const \|explicit \\)*" ; opt. modifiers
              "\\([a-zA-Z0-9_:*&]+\\)\\s-+[a-zA-Z0-9_:*]+\\s-*(") ; return type
             bound t)
            (buffer-substring (match-beginning 2)(match-end 2))
          "void")))))


(defvar cppdoc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'cppdoc-insert-function-comment)
    (define-key map (kbd "b") #'cppdoc-insert-block-comment)
    (define-key map (kbd "c") #'cppdoc-insert-comment)
    (define-key map (kbd "m") #'cppdoc-insert-member-comment)
    map)
  "Keymap for Cppdoc commands after `cppdoc-keymap-prefix'.")
(fset 'cppdoc-command-map cppdoc-command-map)

(defvar cppdoc-mode-map
  (let ((map (make-sparse-keymap)))
    (when cppdoc-keymap-prefix
      (define-key map cppdoc-keymap-prefix 'cppdoc-command-map))
    (easy-menu-define cppdoc-mode-menu map
      "Menu for Cppdoc"
      '("Cppdoc"
        ["Insert function comment at point" cppdoc-insert-function-comment]
        ["Insert class comment at point" cppdoc-insert-block-comment]
        ["Insert brief comment at point" cppdoc-insert-comment]
        ["Insert member comment at point" cppdoc-insert-member-comment]
        ))
    map)
  "Keymap for Cppdoc mode.")

;;;###autoload
(define-minor-mode cppdoc-mode
  "Minor mode to assist project management and navigation.

When called interactively, toggle `cppdoc-mode'.  With prefix
ARG, enable `cppdoc-mode' if ARG is positive, otherwise disable
it.
When called from Lisp, enable `cppdoc-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `cppdoc-mode'.
Otherwise behave as if called interactively.
\\{cppdoc-mode-map}"
  :lighter cppdoc--mode-line
  :keymap cppdoc-mode-map
  :group 'cppdoc
  :require 'cppdoc
  :global t
  (cond
   (cppdoc-mode
    )))

(provide 'cppdoc)

;;; cppdoc ends here
