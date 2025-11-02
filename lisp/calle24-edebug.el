;;; calle24-edebug.el --- Calle 24 Edebug Support    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

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

;; Support for Calle 24 Edebug toolbar icons.

;; INSTALLATION
;; Add the following Elisp to your initialization file.

;; (require 'calle24-edebug)
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (setq-local tool-bar-map (calle24-edebug-mode-tool-bar-map))
;;             (let ((appearance (calle24-get-appearance)))
;;               (cond
;;                ((string= appearance "dark")
;;                 (calle24-update-tool-bar-appearance t))
;;                ((string= appearance "light")
;;                 (calle24-update-tool-bar-appearance))
;;                (t (calle24-update-tool-bar-appearance))))))

;; (add-hook 'edebug-eval-mode-hook
;;           (lambda ()
;;             (setq-local tool-bar-map (calle24-edebug-eval-mode-tool-bar-map))
;;             (let ((appearance (calle24-get-appearance)))
;;               (cond
;;                ((string= appearance "dark")
;;                 (calle24-update-tool-bar-appearance t))
;;                ((string= appearance "light")
;;                 (calle24-update-tool-bar-appearance))
;;                (t (calle24-update-tool-bar-appearance))))))

;; (add-hook 'edebug-eval-mode-hook #'window-tool-bar-mode)

;;; Code:

;; -------------------------------------------------------------------
;; Edebug Support
(require 'calle24)
(require 'edebug)

(defun calle24-edebug-eval-mode-tool-bar-map ()
  "Configure Edebug Tool Bar Map."
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item
     "edebug/update-eval-list"
     #'edebug-update-eval-list #'edebug-update-eval-list map
     :help "Add Symbol")

    (tool-bar-local-item
     "edebug/delete-eval-item"
     #'edebug-delete-eval-item #'edebug-delete-eval-item map
     :help "Delete Symbol")

    (tool-bar-local-item
     "edebug/eval-last-sexp"
     #'edebug-eval-last-sexp #'edebug-eval-last-sexp map
     :help "Eval Last")

    (tool-bar-local-item
     "edebug/eval-print-last-sexp"
     #'edebug-eval-print-last-sexp #'edebug-eval-print-last-sexp map
     :help "Print Eval Last")

    (tool-bar-local-item
     "edebug/where"
     #'edebug-where #'edebug-where map
     :help "Resume")

    map))

(defun calle24-edebug-mode-tool-bar-map ()
  "Configure Edebug Tool Bar Map."
  (let ((map (make-sparse-keymap)))
    ;; Edebug
    (tool-bar-local-item
     "edebug/eval-defun"
     #'eval-defun #'eval-defun map
     :visible '(not (edebug-mode-p))
     :help "Go (eval-defun)")

    (tool-bar-local-item
     "edebug/goto-here"
     #'edebug-goto-here #'edebug-goto-here map
     :visible '(edebug-mode-p)
     :help "Here")

    (tool-bar-local-item
     "edebug/set-mode"
     #'edebug-set-initial-mode #'edebug-set-initial-mode map
     :visible '(not (edebug-mode-p))
     :help "Set Initial Mode")

    (tool-bar-local-item
     "edebug/next-mode"
     #'edebug-next-mode #'edebug-next-mode map
     :visible '(edebug-mode-p)
     :help "Next Mode")

    (tool-bar-local-item
     "edebug/continue-mode"
     #'edebug-continue-mode #'edebug-continue-mode map
     :visible '(edebug-mode-p)
     :help "Continue Mode")

    (tool-bar-local-item
     "edebug/trace-mode"
     #'edebug-trace-mode #'edebug-trace-mode map
     :visible '(edebug-mode-p)
     :help "Trace Mode")

    (tool-bar-local-item
     "edebug/go-mode"
     #'edebug-go-mode #'edebug-go-mode map
     :visible '(edebug-mode-p)
     :help "Go Mode")

    (tool-bar-local-item
     "edebug/step-mode"
     #'edebug-step-mode #'edebug-step-mode map
     :visible '(edebug-mode-p)
     :help "Step Mode")

    (tool-bar-local-item
     "edebug/forward-sexp"
     #'edebug-forward-sexp #'edebug-forward-sexp map
     :visible '(edebug-mode-p)
     :help "Forward sexp (Step Over)")

    (tool-bar-local-item
     "edebug/step-in"
     #'edebug-step-in #'edebug-step-in map
     :visible '(edebug-mode-p)
     :help "Step in sexp")

    (tool-bar-local-item
     "edebug/step-out"
     #'edebug-step-out #'edebug-step-out map
     :visible '(edebug-mode-p)
     :help "Step out sexp")

    (tool-bar-local-item
     "edebug/eval-expression"
     #'edebug-eval-expression #'edebug-eval-expression map
     :visible '(edebug-mode-p)
     :help "Evaluate Expression")

    (tool-bar-local-item
     "edebug/previous-result"
     #'edebug-previous-result #'edebug-previous-result map
     :visible '(edebug-mode-p)
     :help "Previous Result")

    (tool-bar-local-item
     "edebug/view-outside"
     #'edebug-view-outside #'edebug-view-outside map
     :visible '(edebug-mode-p)
     :help "View Outside")

    (tool-bar-local-item
     "edebug/visit-eval-list"
     #'edebug-visit-eval-list #'edebug-visit-eval-list map
     :visible '(edebug-mode-p)
     :help "Watchlist")

    (tool-bar-local-item
     "edebug/set-breakpoint"
     #'edebug-set-breakpoint #'edebug-set-breakpoint map
     :visible '(edebug-mode-p)
     :help "Set Breakpoint")

    (tool-bar-local-item
     "edebug/conditional-breakpoint"
     #'edebug-set-conditional-breakpoint #'edebug-set-conditional-breakpoint map
     :visible '(edebug-mode-p)
     :help "Set Conditional Breakpoint")

    (tool-bar-local-item
     "edebug/next-breakpoint"
     #'edebug-next-breakpoint #'edebug-next-breakpoint map
     :visible '(edebug-mode-p)
     :help "Next Breakpoint")

    (tool-bar-local-item
     "edebug/unset-breakpoint"
     #'edebug-unset-breakpoint #'edebug-unset-breakpoint map
     :visible '(edebug-mode-p)
     :help "Unset Breakpoint")

    (tool-bar-local-item
     "edebug/unset-all-breakpoints"
     #'edebug-unset-breakpoints #'edebug-unset-breakpoints map
     :visible '(edebug-mode-p)
     :help "Unset All Breakpoints")

    (tool-bar-local-item
     "edebug/stop"
     #'edebug-stop #'edebug-stop map
     :visible '(edebug-mode-p)
     :help "Stop")

    (tool-bar-local-item
     "edebug/top-level"
     #'top-level #'top-level map
     :visible '(edebug-mode-p)
     :help "Quit Edebug")

    (tool-bar-local-item
     "edebug/top-level-nonstop"
     #'edebug-top-level-nonstop #'edebug-top-level-nonstop map
     :visible '(edebug-mode-p)
     :help "Quit Edebug Nonstop")

    (tool-bar-local-item
     "help"
     #'edebug-help #'edebug-help map
     :visible '(edebug-mode-p)
     :help "Help")

    (tool-bar-local-item-from-menu #'find-file "new" map nil
                                   :label "New File"
                                   :visible '(not (edebug-mode-p)))

    (tool-bar-local-item-from-menu #'menu-find-file-existing "open" map nil
			           :label "Open"
                                   :visible '(not (edebug-mode-p)))
    (tool-bar-local-item-from-menu #'dired "diropen" map nil
                                   :visible '(not (edebug-mode-p)))

    (if (or (eq window-system 'x)
            (eq window-system 'pgtk))
        (tool-bar-local-item
         "unchecked"
         #'calle24-edebug-ignore #'calle24-edebug-ignore map
         :visible '(not (edebug-mode-p))
         :help "Unchecked"))

    (tool-bar-local-item-from-menu #'save-buffer "save" map nil
			           :label "Save"
                                   :visible '(not (edebug-mode-p)))

    (tool-bar-local-item-from-menu #'undo "undo" map nil
                                   :visible '(not (edebug-mode-p)))
    ;; (define-key-after tool-bar-map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			           "cut" map nil
                                   :visible '(not (edebug-mode-p)))
    ;; (define-key-after tool-bar-map [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			           "copy" map nil
                                   :visible '(not (edebug-mode-p)))

    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			           "paste" map nil
                                   :visible '(not (edebug-mode-p)))

    (tool-bar-local-item-from-menu 'isearch-forward "search"
			           map nil
                                   :label "Search"
                                   :visible '(not (edebug-mode-p)))

    (tool-bar-local-item-from-menu #'kill-this-buffer "close" map nil
                                   :visible '(not (edebug-mode-p)))
    map))

(defun calle24-edebug-ignore ()
  "Hack to avoid pressing kill buffer tool bar button."
  (interactive)
  (message "Blank space to avoid pressing kill buffer button."))

(defvar calle24-edebug--image-appearance-map
  '((edebug-update-eval-list . "edebug/update-eval-list")
    (edebug-delete-eval-item . "edebug/delete-eval-item")
    (edebug-eval-last-sexp . "edebug/eval-last-sexp")
    (edebug-eval-print-last-sexp . "edebug/eval-print-last-sexp")
    (edebug-where . "edebug/where")
    (eval-defun . "edebug/eval-defun")
    (edebug-goto-here . "edebug/goto-here")
    (edebug-set-initial-mode . "edebug/set-mode")
    (edebug-next-mode . "edebug/next-mode" )
    (edebug-continue-mode . "edebug/continue-mode")
    (edebug-trace-mode . "edebug/trace-mode")
    (edebug-go-mode . "edebug/go-mode")
    (edebug-step-mode . "edebug/step-mode")
    (edebug-forward-sexp . "edebug/forward-sexp")
    (edebug-step-in . "edebug/step-in")
    (edebug-step-out . "edebug/step-out")
    (edebug-eval-expression . "edebug/eval-expression")
    (edebug-previous-result . "edebug/previous-result")
    (edebug-view-outside . "edebug/view-outside")
    (edebug-visit-eval-list . "edebug/visit-eval-list")
    (edebug-set-breakpoint . "edebug/set-breakpoint")
    (edebug-set-conditional-breakpoint . "edebug/conditional-breakpoint")
    (edebug-next-breakpoint . "edebug/next-breakpoint")
    (edebug-unset-breakpoint . "edebug/unset-breakpoint")
    (edebug-unset-breakpoints . "edebug/unset-all-breakpoints")
    (edebug-stop . "edebug/stop")
    (top-level . "edebug/top-level")
    (edebug-top-level-nonstop . "edebug/top-level-nonstop")
    (edebug-help . "help")
    (calle24-edebug-ignore . "unchecked"))
  "Alist map of keys used by Edebug toolbar map.")

(setq calle24--image-appearance-map
      (append calle24--image-appearance-map
              calle24-edebug--image-appearance-map))

(provide 'calle24-edebug)
;;; calle24-edebug.el ends here
