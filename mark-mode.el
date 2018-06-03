;;; mark-mode.el --- Highlight or search the words you want ;; -*- lexical-binding: t -*-
;; --------------------------------------------------------
;; Copyright (C) 2010, Trey Jackson <bigfaceworm(at)gmail(dot)com>
;; Copyright (C) 2010, Ivan Korotkov <twee(at)tweedle-dee(dot)org>
;; Copyright (C) 2012, Le Wang
;; Copyright (C) 2018, Salt Ho <asche34(at)outlook(dot)com>
;;
;; Homepage: https://github.com/lewang/fic-mode
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;;; Code:


(defgroup mark-mode nil
  "Highlight and search what to mark"
  :tag "MARK"
  :group 'tools
  :group 'font-lock
  :group 'faces)

(defcustom mark-target-keywords '("FIXME" "TODO" "BUG")
  "Words to highlight or search for."
  :group 'mark-mode)

(defcustom mark-activated-faces
  '(font-lock-doc-face font-lock-string-face font-lock-comment-face)
  "Faces to words of `mark-targe-words'."
  :group 'mark-mode)

(defface mark-face
  '((((class color))
     (:background "white" :foreground "blue" :weight bold :slant italic))
    (t (:weight blod)))
  "Face to fontify `mark-target-keywords'."
  :group 'mark-mode)

(defvar mark-mode-font-lock-keywords
  '((mark-search-for-keywords (1 'mark-face t)))
  "Font Lock keywords for variable ‘mark-mode’.")

(defun mark-search-regexp ()
  "Return Regexp to search words contained in `mark-target-keywords' dynamically."
  (concat "\\<" (regexp-opt mark-target-keywords t) "\\>"))

(defun mark-in-doc/comment-region (pos)
  "Determine if the word is in the doc or comment region.
Checking if face of word in POS is in `mark-activated-faces'."
  (memq (get-char-property pos 'face)
	mark-activated-faces))

(defun mark-search-for-keywords (limit)
  "Implement according to the variable `font-lock-keywords'.
Argument LIMIT , read the doc about `font-lock-keywords'."
  (let (match-data-to-set)
    (save-match-data
      (while (and (null match-data-to-set)
		  (re-search-forward (mark-search-regexp) limit t))
	(when (and (mark-in-doc/comment-region (match-beginning 0))
		   (mark-in-doc/comment-region (match-end 0)))
	  (setq match-data-to-set (match-data)))))
    (when match-data-to-set
      (set-match-data match-data-to-set)
      (goto-char (match-end 0))
      t)))

(defun mark--search-marked-keywords (&optional buffer limit)
  "Get the positions of marked keywords in BUFFER.
Search on current buffer if BUFFER is nil.
LIMIT is the bound of BUFFER where you can search words."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-match-data
	(let (pos)
	  (goto-char (point-min))
	  (while (re-search-forward (mark-search-regexp) limit t)
	    (pcase (match-data)
	      (`(,s ,e . ,_)
	       (when (eq (get-char-property s 'face) 'mark-face)
		 (add-to-list 'pos e)))))
	  (reverse pos))))))

(defun mark--line-in-position (marker)
  "Get the whole line in location MARKER."
  (let ((frombuf (marker-buffer marker))
	(pos (marker-position marker)))
    (if (not (buffer-live-p frombuf))
	(message "Buffer %s is not alive"  (buffer-name frombuf))
      (with-current-buffer frombuf
	(goto-line (line-number-at-pos pos))
	(buffer-substring (line-beginning-position) (line-end-position))))))

(defun mark--jump-to (marker)
  "Jump to the marker."
  (let ((tobuf (marker-buffer marker))
	(pos (marker-position marker)))
    (if (not (buffer-live-p tobuf))
	(message "Buffer %s is not alive" (buffer-name tobuf))
      (progn
	(switch-to-buffer tobuf)
	(goto-char pos)))))

;; insert all lines into a new read-only buffer
;; following the doc (info "Clickable Text")
;; indicates that lines are clickable

(defun mark--write-line-to-buffer (&optional buffer)
  "Put the captured lines into BUFFER.
By default, BUFFER is \"*Marked Words*\"."
  (let* ((oldbuf (current-buffer))
	 (newbuf (get-buffer-create (or buffer "*Marked Words*")))
	 (markers (mark--search-marked-keywords oldbuf)))
	 ;;(lines (mapcar #'mark--line-in-position markers)))
    (if (with-current-buffer oldbuf
	  (bound-and-true-p mark-mode))
	(progn
	  (with-current-buffer newbuf
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (dolist (marker markers)
		(let ((beg (point)))
		  (insert (mark--line-in-position marker))
		  (make-button
		   beg (point) 'action
		   ((lambda (mkr) (lambda (x) (mark--jump-to mkr)))
		    marker)))
		(insert "\n"))))
	  (view-buffer (get-buffer newbuf)))
      (message "Mark mode is disabled in this buffer."))))

;;;###autoload
(define-minor-mode mark-mode
  "Mark mode -- minor for highlight and search words of `mark-target-keywords' in doc/comment."
  :group 'mark-mode
  (let ((kwlist mark-mode-font-lock-keywords))
    (or
     (and mark-mode (font-lock-add-keywords nil kwlist 'append))
     (font-lock-remove-keywords nil kwlist))
    (font-lock-fontify-buffer)))

(provide 'mark-mode)

;;; mark-mode.el ends here
