;;; calle24.el --- Emacs Toolbar Support for SF Symbols -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'tool-bar)
(require 'isearch)
(require 'info)
(require 'help-mode)
(require 'map)

(defcustom calle24-image-directory (concat user-emacs-directory "calle24/images")
  "Image directory location for calle24."
  :type 'directory
  :group 'image)

(defun calle24-update-tool-bar-appearance (&optional dark tb)
  "If DARK is non-nil, configure toolbar map TB images for that appearance.

This function walks through the toolbar items and replaces each
item's :image slot with the image expression appropriate to the
appearance."
  (let* ((tb (if tb tb tool-bar-map))
         (toolbar-items (cdr tb)))

    (mapc (lambda (item)
            (let* ((identifier (nth 0 item))
                   (item-type (nth 1 item))
                   ;; (item-label (if (eq item-type 'menu-item) (nth 2 item) nil))
                   ;; (item-command (if (eq item-type 'menu-item) (nth 3 item) nil))
                   (item-db (if (eq item-type 'menu-item) (nthcdr 4 item) nil))
                   (item-image (if (eq item-type 'menu-item) (map-elt item-db :image) nil)))

              (if item-image
                  ;; (setf (map-elt (nthcdr 4 item) :image) (tool-bar--image-expression "cut"))
                  (setf (map-elt (nthcdr 4 item) :image) (calle24--image-expression identifier dark))
                )))
          toolbar-items)))

(defun calle24-tool-bar-keys (&optional tb)
  "List keys in toolbar map TB."
  (let* ((tb (if tb tb tool-bar-map))
         (toolbar-items (cdr tb))
         (toolbar-keys (mapcar #'car toolbar-items)))
    toolbar-keys))

(defvar calle24-image-appearance-map
  '((Add\ bookmark . "bookmark_add")
    (Back\ in\ History . "left-arrow")
    (Back\ to\ previous\ page . "left-arrow")
    (Close\ browser . "close")
    (Copy\ page\ URL . "copy")
    (Exit . "exit")
    (Forward\ in\ History . "right-arrow")
    (Forward\ to\ next\ page . "right-arrow")
    (Go\ to\ Node... . "jump-to")
    (Lookup\ a\ String... . "index")
    (Next . "next-node")
    (Next \Topic  . "left-arrow")
    (Previous . "prev-node")
    (Previous\ Topic  . "right-arrow")
    (Reload . "refresh")
    (Search... . "search")
    (Top . "home")
    (Up . "up-node")
    (View\ page\ source . "show")
    (copy . "copy")
    (cut . "cut")
    (dired . "diropen")
    (isearch-cancel . "close")
    (isearch-delete-char . "undo")
    (isearch-describe-mode . "help")
    (isearch-exit . "exit")
    (isearch-forward . "search")
    (isearch-occur . "index")
    (isearch-query-replace . "search-replace")
    (isearch-repeat-backward . "left-arrow")
    (isearch-repeat-forward . "right-arrow")
    (kill-buffer . "close")
    (kill-compilation . "cancel")
    (new-file . "new")
    (next-error-no-select . "right-arrow")
    (open-file . "open")
    (paste . "paste")
    (previous-error-no-select . "left-arrow")
    (quit . "close")
    (recompile . "refresh")
    (save-buffer . "save")
    (search . "search")
    (undo . "undo"))
  "Alist map of keys used by certain toolbar maps built-in with Emacs.

The following toolbars are supported:

- `tool-bar-map' (global)
- `info-tool-bar-map'
- `isearch-tool-bar-map'
- `grep-mode-tool-bar-map'
- `eww-tool-bar-map'")

(defun calle24--image-expression (key &optional dark appearance-map)
  "DARK appearance image expression using KEY in APPEARANCE-MAP.

This function is used to configure an image expression for use in
toolbar. The image expression is late-binded code that is used to
search for the actual image file to render.

If DARK is non-nil, then the image expression is configured for
an OS-level dark appearance, otherwise a light appearance is
presumed.

If APPEARANCE-MAP is not specified, then
`calle24-image-appearance-map' is used for the KEY lookup. The
matching value is the basename of the image filename that is used
to build the image expression."
  (let* ((appearance-map (if appearance-map
                             appearance-map
                           calle24-image-appearance-map))
         (value (map-elt appearance-map key)))
    (if value
        (if dark
            (tool-bar--image-expression (concat value "_dark"))
          (tool-bar--image-expression value))
      value)))


(defun calle24-dark-mode ()
  "Configure tool bar images for OS dark mode appearance."
  (interactive)
  ;;(tool-bar-setup)
  (calle24-update-tool-bar-appearance t)
  (calle24-update-tool-bar-appearance t info-tool-bar-map)
  (calle24-update-tool-bar-appearance t isearch-tool-bar-map)
  (calle24-update-tool-bar-appearance t help-mode-tool-bar-map)
  ;;(calle24-update-tool-bar-appearance t eww-tool-bar-map)

  (tool-bar--flush-cache))

(defun calle24-light-mode ()
  "Configure tool bar images for OS light mode appearance."
  (interactive)
  ;;(tool-bar-setup)
  (calle24-update-tool-bar-appearance)
  (calle24-update-tool-bar-appearance nil info-tool-bar-map)
  (calle24-update-tool-bar-appearance nil isearch-tool-bar-map)
  (calle24-update-tool-bar-appearance nil help-mode-tool-bar-map)
  ;;(calle24-update-tool-bar-appearance nil eww-tool-bar-map)

  (tool-bar--flush-cache))


(defun calle24-grep-tool-bar-config ()
  "Configure an opinionated tool bar for grep/compilation mode."
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item
     "left-arrow" #'previous-error-no-select #'previous-error-no-select map
     :rtl "right-arrow"
     :help "Goto previous match")
    (tool-bar-local-item
     "right-arrow" #'next-error-no-select #'next-error-no-select map
     :rtl "left-arrow"
     :help "Goto next match")
    (tool-bar-local-item
     "cancel" #'kill-compilation #'kill-compilation map
     :enable '(let ((buffer (compilation-find-buffer)))
		(get-buffer-process buffer))
     :help "Stop grep")
    (tool-bar-local-item
     "refresh" #'recompile #'recompile map
     :help "Restart grep")
    map))

(provide 'calle24)
;;; calle24.el ends here
