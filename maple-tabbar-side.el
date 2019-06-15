;;; maple-tabbar-side.el ---  maple tabbar side configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-tabbar-side

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; maple tabbar side configuration.
;;

;;; Code:
(require 'subr-x)
(require 'maple-tabbar)

(defvar maple-tabbar-side-name "*maple-tabbar*")
(defvar maple-tabbar-side-face-remap nil)

(defgroup maple-tabbar-side nil
  "Display maple tabbar in window side."
  :group 'maple-tabbar)

(defcustom maple-tabbar-side-display-action '(maple-tabbar-side-display-buffer)
  "Display buffer action."
  :type 'list
  :group 'maple-tabbar-side)

(defcustom maple-tabbar-side-display-alist '((side . top)
                                             (slot . 0)
                                             (window-height . 1))
  "Used by `display-buffer-in-side-window`."
  :type 'alist
  :group 'maple-tabbar-side)

(defmacro maple-tabbar-side-with-window (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(when-let ((window (maple-tabbar-side-window)))
     (with-selected-window window ,@body)))

(defmacro maple-tabbar-side-with-buffer (&rest body)
  "Execute the forms in BODY with buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (get-buffer-create maple-tabbar-side-name)))
     (with-current-buffer buffer ,@body)))

(defun maple-tabbar-side-buffer-configure ()
  "Buffer configuration."
  (maple-tabbar-side-with-buffer
    (display-buffer maple-tabbar-side-name maple-tabbar-side-display-action)
    (setq truncate-lines nil)))

(defun maple-tabbar-side-keymap(action)
  "Make keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    map))

(defun maple-tabbar-side-window-configure ()
  "Window configuration."
  (maple-tabbar-side-with-window
    (let ((window (selected-window)))
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-other-window t)
      (set-window-scroll-bars window 0 nil 0 nil)

      (set-window-fringes window 0 1)
      (set-window-margins window 0 0)

      (setq maple-tabbar-side-face-remap
            (face-remap-add-relative
             'default (list :background (face-attribute 'header-line :background))))
      (setq window-size-fixed 'height))))

(defun maple-tabbar-side-display-buffer (buffer _alist)
  "Display BUFFER _ALIST."
  (display-buffer-in-side-window buffer maple-tabbar-side-display-alist))

(defun maple-tabbar-side-window ()
  "Whether show maple tabbar."
  (get-buffer-window maple-tabbar-side-name t))

(defun maple-tabbar-side-refresh ()
  "Refresh maple tabbar."
  (save-excursion
    (read-only-mode -1)
    (erase-buffer)
    (insert (format-mode-line (mapcar 'concat (maple-tabbar-buffer))))
    (read-only-mode 1)))

(defun maple-tabbar-side-update ()
  "Update maple tabbar."
  (maple-tabbar-update)
  (when (maple-tabbar-side-window)
    (maple-tabbar-side-with-buffer
      (maple-tabbar-side-refresh)
      (maple-tabbar-side-window-configure))))

(defun maple-tabbar-side-mode-off ()
  "Hide maple tabbar."
  (when-let ((window (maple-tabbar-side-window)))
    (with-selected-window window
      (face-remap-remove-relative maple-tabbar-side-face-remap))
    (delete-window window))
  (advice-remove 'maple-tabbar-keymap 'maple-tabbar-side-keymap)
  (remove-hook 'window-configuration-change-hook 'maple-tabbar-side-update))

(defun maple-tabbar-side-mode-on ()
  "Show maple tabbar."
  (maple-tabbar-side-with-buffer
    (maple-tabbar-side-buffer-configure)
    (maple-tabbar-side-refresh)
    (maple-tabbar-side-window-configure))
  (advice-add 'maple-tabbar-keymap :override 'maple-tabbar-side-keymap)
  (add-hook 'window-configuration-change-hook 'maple-tabbar-side-update))

;;;###autoload
(define-minor-mode maple-tabbar-side-mode
  "maple line side mode"
  :group      'maple-tabbar-side
  :global     t
  (if maple-tabbar-side-mode (maple-tabbar-side-mode-on) (maple-tabbar-side-mode-off)))

(provide 'maple-tabbar-side)
;;; maple-tabbar-side.el ends here
