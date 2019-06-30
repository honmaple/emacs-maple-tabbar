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
(require 'maple-tabbar-group)

(defgroup maple-tabbar nil
  "Display tabbar in header line."
  :group 'maple)

(defcustom maple-tabbar-ignore '("^.*\\*" "^magit")
  "Ignore some buffer name."
  :type 'list
  :group 'maple-tabbar)

(defcustom maple-tabbar-adjust t
  "Whether make width auto ajust."
  :type 'boolean
  :group 'maple-tabbar)

(defcustom maple-tabbar-icon (display-graphic-p)
  "Whether show icon."
  :type 'boolean
  :group 'maple-tabbar)

(defcustom maple-tabbar-sep 'maple-xpm-draw
  "Buffer separator."
  :type 'function
  :group 'maple-tabbar)

(defcustom maple-tabbar-select 'maple-tabbar-default-select
  "Tabbar select buffer func."
  :type 'function
  :group 'maple-tabbar)

(defcustom maple-tabbar-kill 'maple-tabbar-default-kill
  "Tabbar kill buffer func."
  :type 'function
  :group 'maple-tabbar)

(defface maple-tabbar-active `((t (:inherit header-line-highlight :box nil :background ,(face-attribute 'default :background))))
  "Tabbar active face."
  :group 'maple-tabbar)

(defface maple-tabbar-inactive `((t (:inherit header-line :height 0.95 :box nil)))
  "Tabbar inactive face."
  :group 'maple-tabbar)

(defvar maple-tabbar-active-buffer nil)
(defvar maple-tabbar-buffer-list nil)

(defun maple-tabbar-face(&optional buffer reverse)
  "Get face when active or not with BUFFER and REVERSE."
  (with-current-buffer maple-tabbar-active-buffer
    (if (and (eq (buffer-name buffer) (buffer-name)) (not reverse))
        'maple-tabbar-active
      'maple-tabbar-inactive)))

(defun maple-tabbar-keymap(action)
  "Make keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] action)
    map))

(defun maple-tabbar-default-select(buffer)
  "Select BUFFER."
  (when maple-tabbar-active-buffer
    (pop-to-buffer maple-tabbar-active-buffer))
  (switch-to-buffer buffer))

(defun maple-tabbar-default-kill(buffer)
  "Kill BUFFER."
  (when maple-tabbar-active-buffer
    (pop-to-buffer maple-tabbar-active-buffer))
  (kill-buffer buffer))

(defun maple-tabbar-icon-for-mode (mode &optional face-overrides)
  "Apply `all-the-icons-for-mode' on MODE with FACE-OVERRIDES but either return an icon or nil."
  (let* ((icon (cdr (or (assoc mode all-the-icons-mode-icon-alist)
                        (assoc (get mode 'derived-mode-parent) all-the-icons-mode-icon-alist))))
         (args (cdr icon))
         (arg-overrides '(:height 0.9 :v-adjust -0.1))
         (face (plist-get (cdr args) :face))
         (face-overrides (when face-overrides `(:background ,(face-attribute face-overrides :background nil t)))))
    (when face
      (setq face-overrides (append `(:inherit ,face) face-overrides)))
    (when face-overrides
      (setq arg-overrides (append arg-overrides (list :face face-overrides))))
    (setq args (append `(,(car args)) arg-overrides (cdr args)))
    (let ((icon (if icon (apply (car icon) args) mode)))
      (unless (symbolp icon) icon))))

(defun maple-tabbar-concat(index buffer &optional face)
  "Display `INDEX` `BUFFER` `FACE` icon with all-the-icons."
  (if (and maple-tabbar-icon (featurep 'all-the-icons))
      (format "%s %s.%s"
              (propertize "\t" 'display
                          (or (maple-tabbar-icon-for-mode (buffer-local-value 'major-mode buffer) face)
                              (maple-tabbar-icon-for-mode 'text-mode face)))
              index (buffer-name buffer))
    (concat index "." (buffer-name buffer))))

(defun maple-tabbar-display(index buffer)
  "Display with INDEX and BUFFER."
  (let ((face (maple-tabbar-face buffer))
        (face1 (maple-tabbar-face buffer t))
        (name (buffer-name buffer)))
    (concat
     (funcall maple-tabbar-sep face1 face t)
     (propertize
      (maple-tabbar-concat index buffer face)
      'face face
      'pointer 'hand
      'help-echo name
      'keymap (maple-tabbar-keymap
               `(lambda (event) (interactive "e") (funcall maple-tabbar-select ,buffer))))
     (propertize
      " ×"
      'face face
      'help-echo (format "kill %s" name)
      'pointer 'hand
      'keymap (maple-tabbar-keymap
               `(lambda (event) (interactive "e") (funcall maple-tabbar-kill ,buffer))))
     (funcall maple-tabbar-sep face face1))))

(defun maple-tabbar-buffers()
  "Get buffer list."
  (let* ((index 0)
         (group (maple-tabbar-current-group maple-tabbar-active-buffer))
         (buffers (maple-tabbar-group-buffers group))
         (buffers (cl-loop for buffer in buffers collect
                           (when (buffer-live-p buffer)
                             (setq index (+ index 1))
                             (maple-tabbar-display (int-to-string index) buffer)))))
    (remove nil buffers)))

(defun maple-tabbar-ignore-p(buffer)
  "Ignore BUFFER name."
  (or (window-minibuffer-p)
      (catch 'ignored
        (dolist (regex maple-tabbar-ignore)
          (when (string-match regex (buffer-name buffer))
            (throw 'ignored t))))))

(defun maple-tabbar-init()
  "Init."
  (let* ((buffers (maple-tabbar-buffers))
         (p (string-join buffers))
         (width (+ (window-width)
                   (or (cdr (window-margins)) 0)
                   (or (car (window-margins)) 0))))
    (when maple-tabbar-adjust
      (while (> (string-width p) width)
        (setq buffers (butlast buffers))
        (setq p (string-join buffers))))
    p))

(defun maple-tabbar-refresh()
  "Refresh buffer list."
  (setq header-line-format (maple-tabbar-init)))

(defun maple-tabbar-update()
  "Hook when buffer change."
  (unless (maple-tabbar-ignore-p (current-buffer))
    (setq maple-tabbar-active-buffer (current-buffer)))
  (maple-tabbar-group-buffer maple-tabbar-active-buffer)
  (setq maple-tabbar-buffer-list
        (remove-if-not (lambda(x) (buffer-live-p (car x))) maple-tabbar-buffer-list)))

(defun maple-tabbar-mode-on()
  "Show maple tabbar."
  (setq maple-tabbar-buffer-list '())
  (dolist (buffer (buffer-list))
    (unless (maple-tabbar-ignore-p buffer) (maple-tabbar-group-buffer buffer)))
  (maple-tabbar-update)
  (set-face-attribute 'header-line nil :box nil)
  (add-hook 'window-configuration-change-hook 'maple-tabbar-update)
  (setq-default header-line-format `("%e" (:eval (maple-tabbar-init)))))

(defun maple-tabbar-mode-off()
  "Hide maple tabbar."
  (remove-hook 'window-configuration-change-hook 'maple-tabbar-update)
  (setq-default header-line-format nil))

;;;###autoload
(define-minor-mode maple-tabbar-mode
  "Maple tabbar mode"
  :group      'maple-tabbar
  :global     t
  (if maple-tabbar-mode (maple-tabbar-mode-on) (maple-tabbar-mode-off)))

(provide 'maple-tabbar)
;;; maple-tabbar.el ends here
