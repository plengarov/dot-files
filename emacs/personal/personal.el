;;; personal.el --- Emacs Prelude: Pavel's configuration.
;;
;; Copyright (c) 2017 Pavel Lengarov
;;
;; Author: Pavel Lengarov <pavel.lengarov@gmail.com>
;; URL: https://github.com/plengarov/dot-files
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.  Nothing more, nothing less.

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

;;; Code:
(defvar package-list)
(setq package-list '(ag
                     org
                     org-bullets))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the new packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; cppmode
(require 'cppmode)

;; org mode
(require 'org)
(require 'org-bullets)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))
(setq org-hide-leading-stars t)
;; end org mode

;; disable scrollbar
(scroll-bar-mode -1)

(provide 'personal)
;;; personal.el ends here
