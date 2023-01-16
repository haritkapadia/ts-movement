;;; ts-movement.el --- Movement commands using treesit syntax tree -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Harit Kapadia <haritkapadia@outlook.com>
;; Version: 0.1
;; Package-Requires: ((treesit))
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

(defun tsm/-overlay-at-point (point)
  "Get overlay at POINT, or make one and add to `tsm/-overlays' if it does not exist."
  (or (tsm/-find-overlay-at-point point)
      (let* ((node (treesit-node-on point point))
             (overlay (make-overlay (treesit-node-start node) (treesit-node-end node))))
        (overlay-put overlay 'face 'highlight)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'node node)
        (puthash overlay overlay tsm/-overlays)
        overlay)))

(defun tsm/delete-overlay-at-point (point)
  "Delete node indicator at POINT."
  (interactive "d")
  (let ((overlay (tsm/-find-overlay-at-point point)))
    (when overlay
      (delete-overlay overlay)
      (remhash overlay tsm/-overlays))))

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

(defun tsm/clear-overlays (&optional beg end)
  "Remove all overlays. BEG and END are unused."
  (interactive)
  (ignore beg end)
  (dolist (overlay (hash-table-keys tsm/-overlays))
    (delete-overlay overlay))
  (clrhash tsm/-overlays))

(defvar ts-movement-map
  (let ((map (make-sparse-keymap "Tree Sitter Movement")))
    (cond ((require 'hydra nil 'noerror)
           (defhydra tsm/hydra ()
             "TS Movement"
             ("d" #'tsm/delete-overlay-at-point)
             ("b" #'tsm/node-prev)
             ("f" #'tsm/node-next)
             ("p" #'tsm/node-parent)
             ("n" #'tsm/node-child)
             ("a" #'tsm/node-start)
             ("e" #'tsm/node-end)
             ("m" #'tsm/node-mark))
           (define-key map (kbd "C-c .") #'tsm/hydra/body))
          (t
           (define-key map (kbd "C-c . d") #'tsm/delete-overlay-at-point)
           (define-key map (kbd "C-c . b") #'tsm/node-prev)
           (define-key map (kbd "C-c . f") #'tsm/node-next)
           (define-key map (kbd "C-c . p") #'tsm/node-parent)
           (define-key map (kbd "C-c . n") #'tsm/node-child)
           (define-key map (kbd "C-c . a") #'tsm/node-start)
           (define-key map (kbd "C-c . e") #'tsm/node-end)
           (define-key map (kbd "C-c . m") #'tsm/node-mark)))
    map))

;;;###autoload
(define-minor-mode ts-movement-mode
  "Movement and editing commands using treesit syntax tree."
  :keymap ts-movement-map
  (setq-local before-change-functions (cons #'tsm/clear-overlays before-change-functions)))

(provide 'ts-movement)
;;; ts-movement.el ends here
