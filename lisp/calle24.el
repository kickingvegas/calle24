;;; calle24.el --- Emacs Toolbar Support for SF Symbols -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/calle24
;; Keywords: tools
;; Version: 1.0.6-rc.1
;; Package-Requires: ((emacs "29.1"))

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

;; Calle 24 provides toolbar support for SF Symbols. It achieves this by
;; substituting Emacs image assets with SF Symbols by proxy.

;; This package is only intended to be used by Emacs (NS variant) running on
;; macOS.

;; INSTALLATION

;; Upon installation or subsequent updates of the package `calle24' from MELPA,
;; run the following command:

;; M-x `calle24-install'

;; You should restart Emacs after running the above command to see the tool bar
;; images replaced with their SF Symbols equivalent.

;; CONFIGURATION

;; Add the following code to your Emacs initialization to load
;; appearance-specific images.

;; (calle24-refresh-appearance)
;; (add-hook 'compilation-mode-hook #'calle24-refresh-appearance)

;; UNINSTALL

;; In the event that you do not wish to use Calle 24, perform the following
;; steps before deleting the calle24 package.

;; 1. Remove the directory `calle24-image-directory' in `user-emacs-directory'.
;; 2. Remove the `calle24-image-directory' from `image-load-path'.

;;; Code:
(require 'tool-bar)
(require 'isearch)
(require 'info)
(require 'help-mode)
(require 'map)
(require 'seq)
(require 'package)
(require 'compile)
(require 'doc-view)

(defvar user-emacs-directory) ; declared only to pass package lint

(defcustom calle24-image-directory (concat user-emacs-directory "calle24/images")
  "Image directory location for installed Calle 24 images.

This variable is added to `image-load-path' which is the path list used
by Emacs to find tool bar image files.

By default, this value is set to the directory ‘calle24/images’ in
`user-emacs-directory'."
  :type 'directory
  :group 'image)

(defun calle24-update-tool-bar-appearance (&optional dark tb)
  "If DARK is non-nil, configure the images in toolbar map TB accordingly.

This function walks through the toolbar items in TB and replaces each
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
                  (setf (map-elt (nthcdr 4 item) :image) (calle24--image-expression identifier dark)))))
          toolbar-items)))

(defun calle24--tool-bar-keys (&optional tb)
  "List keys in toolbar map TB."
  (let* ((tb (if tb tb tool-bar-map))
         (toolbar-items (cdr tb))
         (toolbar-keys (mapcar #'car toolbar-items)))
    toolbar-keys))

(defvar calle24--image-appearance-map
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
    (find-file . "new")
    (menu-find-file-existing . "open")
    (new-file . "new")
    (New\ Search . "search")
    (Search\ Backward . "left-arrow")
    (Search\ Forward . "right-arrow")
    (next-error-no-select . "right-arrow")
    (open-file . "open")
    (paste . "paste")
    (previous-error-no-select . "left-arrow")
    (quit . "close")
    (recompile . "refresh")
    (save-buffer . "save")
    (search . "search")
    (undo . "undo")
    (\ Apply\  . "index")
    (\ Apply\ and\ Save\  . "save")
    (\ Undo\ Edits\  . "refresh")
    (\ Reset\ Customizations\  . "undo")
    (\ Erase\ Customizations\  . "delete")
    (\ Toggle\ hiding\ all\ values\  . "hide")
    (\ Help\ for\ Customize\  . "help")
    (\ Exit\  . "exit")
    (Previous\ page . "last-page")
    (Next\ page . "next-page"))
  "Alist map of keys used by certain toolbar maps built-in with Emacs.

The following toolbars are supported:

- `tool-bar-map' (global)
- `info-tool-bar-map'
- `isearch-tool-bar-map'
- `grep-mode-tool-bar-map'
- `eww-tool-bar-map'
- `doc-view-tool-bar-map'")

(defun calle24--image-expression (key &optional dark appearance-map)
  "DARK appearance image expression using KEY in APPEARANCE-MAP.

This function is used to configure an image expression for use in
toolbar. The image expression is late-binded code that is used to
search for the actual image file to render.

If DARK is non-nil, then the image expression is configured for
an OS-level dark appearance, otherwise a light appearance is
presumed.

If APPEARANCE-MAP is not specified, then
`calle24--image-appearance-map' is used for the KEY lookup. The
matching value is the basename of the image filename that is used
to build the image expression."
  (let* ((appearance-map (if appearance-map
                             appearance-map
                           calle24--image-appearance-map))
         (value (map-elt appearance-map key)))
    (if value
        (if dark
            (tool-bar--image-expression (concat value "_dark"))
          (tool-bar--image-expression value))
      value)))

;;;###autoload (autoload 'calle24-dark-appearance "calle24" nil t)
(defun calle24-dark-appearance ()
  "Configure tool bar images for OS dark mode appearance."
  (interactive)
  (calle24--update-toolbars t))

;;;###autoload (autoload 'calle24-light-appearance "calle24" nil t)
(defun calle24-light-appearance ()
  "Configure tool bar images for OS light mode appearance."
  (interactive)
  (calle24--update-toolbars nil))

(defun calle24--update-toolbars (dark)
  "Update images for all Emacs tool bar instances for DARK appearance.

If DARK is non-nil, then all tool bars are updated for the macOS dark
appearance, otherwise light."
  (let* ((tblist ()))
    ;; !!!: Update tool bars for modes not instantiated.
    (calle24-update-tool-bar-appearance dark info-tool-bar-map)
    (calle24-update-tool-bar-appearance dark isearch-tool-bar-map)
    (calle24-update-tool-bar-appearance dark help-mode-tool-bar-map)
    (calle24-update-tool-bar-appearance dark doc-view-tool-bar-map)
    ;; (calle24-update-tool-bar-appearance nil eww-tool-bar-map)

    ;; !!!: Update tool bars already instantiated.
    ;; tblist is used to carry tool bars that have already been updated.
    (mapc (lambda (x)
            (with-current-buffer x
              (if (not (seq-contains-p tblist tool-bar-map))
                  (progn
                    (calle24-update-tool-bar-appearance dark)
                    (push tool-bar-map tblist)
                    (tool-bar--flush-cache)))))
          (buffer-list))))

(defun calle24-grep-tool-bar-config ()
  "Configure Calle 24 opinionated tool bar for grep/compilation mode."
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

(defun calle24-get-appearance ()
  "Get current OS appearance."
  (cond
   ((eq (window-system) 'ns)
    (calle24-get-macos-appearance))

   ((eq (window-system) 'pgtk)
    (calle24-get-pgtk-appearance))

   (t "light")))

(defun calle24-get-macos-appearance ()
  "Get current macOS appearance.
This function is intended to run only on macOS systems with swift
installed."
  (let ((results (process-lines "swift" "-e" "
import AppKit

switch NSApplication.shared.effectiveAppearance.name {
case .darkAqua:
    print(\"dark\")

default:
    print(\"light\")
}")))
    (car results)))

(defun calle24-get-pgtk-appearance ()
  "Get current pgtk appearance."
  (let ((result (process-lines
                  "gsettings"
                  "get"
                  "org.gnome.desktop.interface"
                  "color-scheme")))

    (if (string= (car result) "'prefer-dark'")
        "dark"
      "light")))

;;;###autoload (autoload 'calle24-refresh-appearance "calle24" nil t)
(defun calle24-refresh-appearance ()
  "Refresh OS appearance-dependent images from Calle 24."
  (interactive)
  (let ((appearance (calle24-get-appearance)))
    (cond
     ((string= appearance "dark") (calle24-dark-appearance))
     ((string= appearance "light") (calle24-light-appearance))
     (t (calle24-light-appearance)))))

(defun calle24-install ()
  "Install Calle 24 tool bar images.

This command will install the Calle 24 tool bar images into the
directory `calle24-image-directory' via the command line utility
‘rsync’.

In addition, this command will add `calle24-image-directory' to the
`image-load-path'.

Note that upon usage, it is recommended to restart Emacs to load the
Calle 24 images."

  (interactive)

  (if (y-or-n-p "This command will install Calle 24. Do you wish to proceed? ")
      (let* ((calle24-pkg (car (map-elt package-alist 'calle24)))
             (calle24-pkg-dir (package-desc-dir calle24-pkg))
             (src-dir (concat calle24-pkg-dir "/images/"))
             (dest-dir (if (string= (substring calle24-image-directory -1) "/")
                           calle24-image-directory
                         (concat calle24-image-directory "/")))
             (cmdList ()))

        (make-directory dest-dir t)
        (calle24-configure-image-load-path)
        (push "rsync" cmdList)
        (push "-avh" cmdList)
        (push src-dir cmdList)
        (push dest-dir cmdList)
        (message (shell-command-to-string (string-join (reverse cmdList) " "))))
    (message "Ok.")))

(defun calle24-configure-image-load-path ()
  "Add `calle24-image-directory' to `image-load-path' and persist.

This function will configure `image-load-path' to contain the install
directory for Calle 24 images `calle24-image-directory'."
  (let ((iload-path image-load-path)
        (image-directory calle24-image-directory))
    (if (not (seq-contains-p image-load-path image-directory))
        (progn
          (push image-directory iload-path)
          (customize-save-variable 'image-load-path iload-path)))))

(defun calle24-uninstall ()
  "Uninstall Calle 24 from this system.

Run this command before deleting the package ‘calle24’.

This will delete the contents of `calle24-image-directory' and remove
`calle24-image-directory' from `image-load-path'."
  (interactive)

  (if (y-or-n-p "This command will uninstall Calle 24 from your Emacs setup. Do you wish to proceed? ")
      (let ((calle24-directory (concat user-emacs-directory "calle24"))
            (iload-path (seq-remove
                         (lambda (x) (string= x calle24-image-directory))
                         image-load-path)))
        (delete-directory calle24-directory t t)

        (if (seq-contains-p image-load-path calle24-image-directory)
            (customize-save-variable 'image-load-path iload-path)))

    (message "Ok.")))

(provide 'calle24)
;;; calle24.el ends here
