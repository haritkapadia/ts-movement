;;; ts-movement.el --- Movement commands using treesit syntax tree -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Harit Kapadia <haritkapadia@outlook.com>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords:
;; URL:

;;; Commentary:
;; Movement commands using treesit syntax tree.
;; Optionally depends on `hydra'.

;;; Code:
(require 'treesit)

(defvar-local tsm/-overlays (make-hash-table :test #'eq))

(defun tsm/-find-overlay-at-point (point)
  "Find any overlay in tsm/-overlays containing POINT."
  (seq-find (lambda (o) (gethash o tsm/-overlays)) (overlays-at point)))

(defun tsm/-overlay-at-node (node)
  "Create overlay of NODE and add to `tsm/-overlays'"
  (let ((overlay (make-overlay (treesit-node-start node) (treesit-node-end node))))
    (overlay-put overlay 'face 'lazy-highlight)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'node node)
    ; Emacs documentation says integer must be nonnegative, but -1 seems to work...
    ; (i.e. puts it below multiple-cursors region overlay)
    (overlay-put overlay 'priority '(nil . -1))
    (puthash overlay overlay tsm/-overlays)
    overlay))

(defun tsm/-overlay-at-point (point)
  "Get overlay at POINT, or make one and add to `tsm/-overlays' if it does not exist."
  (or (tsm/-find-overlay-at-point point)
      (tsm/-overlay-at-node (treesit-node-on point point))))

(defun tsm/delete-overlay-at-point (point)
  "Delete node indicator at POINT."
  (interactive "d")
  (let ((overlay (tsm/-find-overlay-at-point point)))
    (when overlay
      (delete-overlay overlay)
      (remhash overlay tsm/-overlays))))

(defun tsm/-go-to-closest-overlay (point key)
  (let* ((overlays (hash-table-keys tsm/-overlays))
         (target-overlay
          (seq-reduce (lambda (min overlay)
                        (if (< (funcall key overlay) (funcall key min)) overlay min))
                      (cdr overlays)
                      (car overlays))))
    (goto-char (treesit-node-start (tsm/-get-node target-overlay)))))

(defun tsm/backward-overlay (point)
  "Move POINT backward to the closest node indicator."
  (interactive "d")
  (cl-flet ((key (overlay)
              (let ((value (- point (treesit-node-start (tsm/-get-node overlay)))))
                (if (> value 0) value 1.0e+INF))))
    (tsm/-go-to-closest-overlay point #'key)))

(defun tsm/forward-overlay (point)
  "Move POINT forward to the closest node indicator."
  (interactive "d")
  (cl-flet ((key (overlay)
              (let ((value (- (treesit-node-start (tsm/-get-node overlay)) point)))
                (if (> value 0) value 1.0e+INF))))
    (tsm/-go-to-closest-overlay point #'key)))

(defun tsm/node-prev (point)
  "Go to previous sibling of node at POINT and go to start of node."
  (interactive "d")
  (let* ((overlay (tsm/-overlay-at-point point))
         (node (overlay-get overlay 'node))
         (next (treesit-node-prev-sibling node)))
    (when next
      (overlay-put overlay 'node next)
      (move-overlay overlay (treesit-node-start next) (treesit-node-end next))
      (goto-char (treesit-node-start next)))))

(defun tsm/node-next (point)
  "Select next sibling of node at POINT and go to start of node."
  (interactive "d")
  (let* ((overlay (tsm/-overlay-at-point point))
         (node (overlay-get overlay 'node))
         (next (treesit-node-next-sibling node)))
    (when next
      (overlay-put overlay 'node next)
      (move-overlay overlay (treesit-node-start next) (treesit-node-end next))
      (goto-char (treesit-node-start next)))))

(defun tsm/node-parent (point)
  "Select parent of indicated node at POINT."
  (interactive "d")
  (let* ((overlay (tsm/-overlay-at-point point))
         (node (overlay-get overlay 'node))
         (next (treesit-node-parent node)))
    (when next
      (overlay-put overlay 'node next)
      (move-overlay overlay (treesit-node-start next) (treesit-node-end next)))))

(defun tsm/node-child (point)
  "Select child containing POINT of indicated node."
  (interactive "d")
  (let* ((overlay (tsm/-overlay-at-point point))
         (node (overlay-get overlay 'node))
         (next (treesit-node-first-child-for-pos node point)))
    (when next
      (overlay-put overlay 'node next)
      (move-overlay overlay (treesit-node-start next) (treesit-node-end next)))))

(defun tsm/node-start (point)
  "Go to start of node at POINT."
  (interactive "d")
  (goto-char (treesit-node-start (overlay-get (tsm/-overlay-at-point point) 'node))))

(defun tsm/node-end (point)
  "Go to end of node at POINT."
  (interactive "d")
  (goto-char (1- (treesit-node-end (overlay-get (tsm/-overlay-at-point point) 'node)))))

(defun tsm/node-mark (point)
  "Mark node at POINT."
  (interactive "d")
  (let ((node (overlay-get (tsm/-overlay-at-point point) 'node)))
    (push-mark (treesit-node-end node) nil t)
    (goto-char (treesit-node-start node))))

(defun tsm/node-children (point)
  "Select all immediate children of node at POINT that are TYPE."
  (interactive "d")
  (let* ((overlay (tsm/-overlay-at-point (point)))
         (node (overlay-get overlay 'node)))
    (cl-loop for i from 0 below (treesit-node-child-count node)
          for child = (treesit-node-child node i)
          do (tsm/-overlay-at-node child))
    (delete-overlay overlay)
    (remhash overlay tsm/-overlays)))

(defun tsm/-treesit-node-children (node)
  "Get all children of NODE."
  (cl-loop for i from 0 below (treesit-node-child-count node)
        collect (treesit-node-child node i)))

(defun tsm/-collect-types (list-of-nodes)
  (delete-dups (mapcar #'treesit-node-type list-of-nodes)))

(defun tsm/-get-node (overlay)
  (overlay-get overlay 'node))

(defun tsm/node-children-of-type (type)
  "Select all immediate children of node at POINT that are TYPE."
  (interactive
   (list (completing-read "Type: " (tsm/-collect-types (tsm/-treesit-node-children (tsm/-get-node (tsm/-overlay-at-point (point))))))))
  (let* ((overlay (tsm/-overlay-at-point (point)))
         (node (tsm/-get-node overlay)))
    (cl-loop for i from 0 below (treesit-node-child-count node)
          for child = (treesit-node-child node i)
          if (string= type (treesit-node-type child))
          do (tsm/-overlay-at-node child))
    (delete-overlay overlay)
    (remhash overlay tsm/-overlays)))

(defun tsm/clear-overlays-of-type (type)
  "Remove all overlays of TYPE."
  (interactive
   (list (completing-read "Type: " (tsm/-collect-types (mapcar #'tsm/-get-node (hash-table-keys tsm/-overlays))))))
  (dolist (overlay (hash-table-keys tsm/-overlays))
    (when (string= type (treesit-node-type (tsm/-get-node overlay)))
      (delete-overlay overlay)
      (remhash overlay tsm/-overlays))))

;;;###autoload
(defun tsm/clear-overlays (&optional beg end)
  "Remove all overlays. BEG and END are unused."
  (interactive)
  (ignore beg end)
  (dolist (overlay (hash-table-keys tsm/-overlays))
    (delete-overlay overlay))
  (clrhash tsm/-overlays))

(defun tsm/mc/mark-all-overlays (point)
  "Add a multiple-cursor cursor at the start of each node overlay"
  (interactive "d")
  (cl-flet ((key (overlay)
              (abs (- point (treesit-node-start (tsm/-get-node overlay))))))
    (let* ((overlays (hash-table-keys tsm/-overlays))
           (current-overlay (tsm/-overlay-at-point point))
           (target-overlay
            (or current-overlay
                (seq-reduce (lambda (min overlay)
                              (if (< (key overlay) (key min)) overlay min))
                            (cdr overlays)
                            (car overlays)))))
      (maphash
       (lambda (overlay _)
         (ignore _)
         (unless (eql overlay target-overlay)
           (goto-char (treesit-node-start (tsm/-get-node overlay)))
           (mc/create-fake-cursor-at-point)))
       tsm/-overlays)
      (goto-char (treesit-node-start (tsm/-get-node target-overlay)))
      (if (> (mc/num-cursors) 1)
          (multiple-cursors-mode 1)
        (mc/disable-multiple-cursors-mode)))))

(when (require 'multiple-cursors nil 'noerror)
  (push 'mc/mark-all-overlays mc--default-cmds-to-run-once))

;;;###autoload
(defvar ts-movement-map (make-sparse-keymap "Tree Sitter Movement"))

;;;###autoload
(define-minor-mode ts-movement-mode
  "Movement and editing commands using treesit syntax tree."
  :keymap ts-movement-map
  (setq-local before-change-functions (cons #'tsm/clear-overlays before-change-functions)))

(provide 'ts-movement)
;;; ts-movement.el ends here
