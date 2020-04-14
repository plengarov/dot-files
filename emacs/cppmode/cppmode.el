;;; cppmode.el --- Emacs Prelude: Pavel's configuration.
;;
;; Copyright (c) 2017 Pavel Lengarov
;;
;; Author: Pavel Lengarov <pavel.lengarov@gmail.com>
;; URL: https://github.com/plengarov/dot-files
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration for c++ develpment.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;; install additional package for C/C++ development.
;;; Code:
(defvar package-list)
(setq package-list '(clang-format
                     modern-cpp-font-lock
                     ggtags
                     google-c-style))

;; fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; install the new packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; set basic offset
;; (defun prelude-c++-mode-common-defaults ()
;;   (if (derived-mode-p 'c++-mode)
;;     (setq c-default-style "k&r"
;;           c-basic-offset 2)
;;     (setq c-default-style "k&r"
;;           c-basic-offset 4)
;;     (c-set-offset 'substatement-open 0)))
;; (setq prelude-c-mode-common-hook 'prelude-c++-mode-common-defaults)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Require flycheck to be present.
(require 'flycheck)
;; Force flycheck to always use c++11 support. We use
;; the clang language backend so this is set to clang.
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++11")))

;; clang-format can be triggered using C-c C-f.
(require 'clang-format)
(global-set-key (kbd "C-c C-v") 'clang-format-region)
(global-set-key (kbd "C-c C-f") 'clang-format-buffer)

;; Enable semantics mode for auto-completion
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(semantic-remove-system-include "/usr/include/" 'c++-mode)
(semantic-remove-system-include "/usr/local/include/" 'c++-mode)
(add-hook 'semantic-init-hooks
          'semantic-reset-system-include)

;; use global tags as default tagging.
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

;; we use moder editors and big scteen so limit line lenthg to 110
(setq whitespace-line-column 110)

;; enable modern fonts for c++
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;; workaround for projectile bug
(setq projectile-project-compilation-cmd "")

;; (scroll-bar-mode -1)

;; cppdoc - Doxygen comments
(require 'cppdoc)
(cppdoc-mode +1)
(define-key cppdoc-mode-map (kbd "s-c") 'cppdoc-command-map)
;; end cppdock

(provide 'cppmode)
;;; cppmode.el ends here
