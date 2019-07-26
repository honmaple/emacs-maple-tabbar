;;; maple-tabbar-group.el --- show tabbar.	-*- lexical-binding: t -*-

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
;; group tabbar.
;;

;;; Code:
(defvar maple-tabbar-buffer-list)
(defvar maple-tabbar-select)

(defun maple-tabbar-group(buffer)
  "Group buffers with BUFFER name."
  (with-current-buffer buffer
    (cond ((and (featurep 'projectile) (projectile-project-p))
           (projectile-project-name))
          (t "default"))))

(defun maple-tabbar-group-buffer(buffer)
  "Add BUFFER to group."
  (unless (assoc buffer maple-tabbar-buffer-list)
    (add-to-list 'maple-tabbar-buffer-list (cons buffer (maple-tabbar-group buffer)) t)))

(defun maple-tabbar-group-buffers(group)
  "Get tabbar buffers with GROUP."
  (remove nil (cl-loop for buffer in maple-tabbar-buffer-list collect
                       (when (equal (cdr buffer) group)
                         (car buffer)))))

(defun maple-tabbar-groups()
  "Group buffers with BUFFER name."
  (sort (delete-dups (mapcar 'cdr maple-tabbar-buffer-list)) #'string<))

(defun maple-tabbar-current-group(&optional buffer)
  "Get current tabbar group with BUFFER."
  (cdr (assoc (or buffer (current-buffer)) maple-tabbar-buffer-list)))

(defun maple-tabbar-select-group(group)
  "Select tabbar GROUP."
  (funcall maple-tabbar-select (car (maple-tabbar-group-buffers group))))

(defun maple-tabbar-previous-group()
  "Get previous tabbar group."
  (interactive)
  (maple-tabbar-next-group t))

(defun maple-tabbar-next-group(&optional forward)
  "Get next tabbar group if FORWARD previous."
  (interactive)
  (let* ((groups (maple-tabbar-groups))
         (current-group (maple-tabbar-current-group))
         (index (position current-group groups :test 'equal)))
    (if forward (setq index (- index 1)) (setq index (+ index 1)))
    (maple-tabbar-select-group
     (cond ((and forward (<= index 0))
            (last groups))
           ((and (>= index (length groups)))
            (car groups))
           (t (nth index groups))))))

(defun maple-tabbar-switch-group(&optional group)
  "Switch GROUP."
  (interactive "P")
  (maple-tabbar-select-group
   (completing-read "Select tabbar group: " (maple-tabbar-groups) nil nil group)))

(provide 'maple-tabbar-group)
;;; maple-tabbar-group.el ends here
