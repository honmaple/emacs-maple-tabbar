;;; maple-tabbar.el --- show tabbar.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-tabbar

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
;; show tabbar.
;;

;;; Code:
(require 'maple-xpm)

(defgroup maple-tabbar nil
  "Display tabbar in header line."
  :group 'maple)

(defcustom maple-tabbar-ignore '("^.*\\*" "^magit")
  "Ignore some buffer name."
  :type 'list
  :group 'maple-tabbar)

(defcustom maple-tabbar-number 6
  "Max show number."
  :type 'number
  :group 'maple-tabbar)

(defcustom maple-tabbar-sep 'maple-xpm-draw
  "Buffer separator."
  :type 'func
  :group 'maple-tabbar)

(defface maple-tabbar-active `((t (:inherit header-line-highlight :height 0.9 :box nil :background ,(face-attribute 'default :background))))
  "Tabbar active face."
  :group 'maple-tabbar)

(defface maple-tabbar-inactive `((t (:inherit header-line :height 0.9 :box nil)))
  "Tabbar inactive face."
  :group 'maple-tabbar)

(defvar maple-tabbar-active-buffer nil)

(defun maple-tabbar-refresh()
  "Refresh buffer list."
  (interactive)
  (setq header-line-format (maple-tabbar--init)))

(defun maple-tabbar-face(&optional buffer)
  "Get face when active or not with BUFFER."
  (if (eq (buffer-name buffer) maple-tabbar-active-buffer)
      'maple-tabbar-active
    'maple-tabbar-inactive))

(defun maple-tabbar-display(index buffer)
  "Display with INDEX and BUFFER."
  (concat
   (funcall maple-tabbar-sep 'header-line (maple-tabbar-face buffer) t)
   (propertize
    (concat index "." (buffer-name buffer))
    'face (maple-tabbar-face buffer)
    'pointer 'hand
    'help-echo "select buffer"
    'local-map (maple-tabbar-keymap
                'mouse-1
                `(lambda (event) (interactive "e") (switch-to-buffer ,buffer))))
   (propertize
    " ×"
    'face (maple-tabbar-face buffer)
    'help-echo "kill this buffer"
    'pointer 'hand
    'local-map (maple-tabbar-keymap
                'mouse-1
                `(lambda (event) (interactive "e") (kill-buffer ,buffer))))
   (funcall maple-tabbar-sep (maple-tabbar-face buffer) 'header-line)))

(defun maple-tabbar-buffer()
  "Get buffer list."
  (let* ((index 0)
         (buffers (cl-loop for buffer in (reverse (buffer-list)) collect
                           (unless (maple-tabbar-ignore-p buffer)
                             (setq index (+ index 1))
                             (maple-tabbar-display (int-to-string index) buffer))))
         (buffers (remove nil buffers)))
    (cl-subseq buffers 0 (min maple-tabbar-number (length buffers)))))

(defun maple-tabbar-ignore-p(buffer)
  "Ignore BUFFER name."
  (or (window-minibuffer-p)
      (catch 'ignored
        (dolist (regex maple-tabbar-ignore)
          (when (string-match regex (buffer-name buffer))
            (throw 'ignored t))))))

(defun maple-tabbar-keymap(mouse function)
  "Make keymap with MOUSE and FUNCTION."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    map))

(defun maple-tabbar--init()
  "Init."
  (mapcar 'concat (maple-tabbar-buffer)))

(defun maple-tabbar-update()
  "Hook when buffer change."
  (unless (maple-tabbar-ignore-p (current-buffer))
    (setq maple-tabbar-active-buffer (buffer-name))))

(defun maple-tabbar-init()
  "Make keymap with MOUSE and FUNCTION."
  (maple-tabbar-update)
  (set-face-attribute 'header-line nil :box nil)
  (add-hook 'window-configuration-change-hook 'maple-tabbar-update)
  (setq-default header-line-format `("%e" (:eval (maple-tabbar--init)))))

;;;###autoload
(define-minor-mode maple-tabbar-mode
  "Maple tabbar mode"
  :group      'maple-tabbar
  :global     t
  (if maple-tabbar-mode (maple-tabbar-init)
    (setq-default header-line-format nil)
    (remove-hook 'window-configuration-change-hook 'maple-tabbar-update)))

(provide 'maple-tabbar)
;;; maple-tabbar.el ends here
