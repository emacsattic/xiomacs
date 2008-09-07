;;; xiomacs.el --- xiomacs control library for GNU Emacs

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: extensions, x11

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)
(require 'rx)

;;; Running STUN

(defvar *stun-program* "/home/dto/xiomacs/stun.lisp")

(defvar *stun-inferior-lisp-program* "sbcl")

(defvar *stun-output-buffer* nil)

(defvar *stun-process* nil)

(defun stun-print-message (string)
  (with-current-buffer *stun-output-buffer*
    (insert string "\n")))

(defvar *stun-message-string-handler-function* #'stun-print-message)

(defvar *stun-partial-message-string* nil)

(defun stun-process-filter (process string)
  (let ((partial-message (or *stun-partial-message-string* "")))
    (setf partial-message (concat partial-message string))
    ;; do we have a complete command?
    (let ((pos (position ?\n partial-message)))
      (if (numberp pos)
	  ;; yes, handle it.
	  (let ((message (subseq partial-message (1- pos))))
	    (insert (format "%s -- %s\n" (current-time-string) string))
	    (funcall *stun-message-string-handler-function* message)
	    ;; any more input? 
	    (setf *stun-partial-message-string*
		  (if (> (length partial-message) (1+ pos))
		      (subseq partial-message (1+ pos))
		      "")))
	  ;; not a complete command. just buffer it.
	  (setf *stun-partial-message-string*
		(concat *stun-partial-message-string* partial-message))))))

(defun stun-start-sbcl-process ()
  (setf *stun-process* (start-process "*stun-process*"
				      nil
				      *stun-inferior-lisp-program*
				      "--load" *stun-program*
				      "--eval" "(stun:stun)")))

(defvar *stun-start-lisp-function* #'stun-start-sbcl-process)
	     
(defun stun-start ()
  (interactive)
  (setf *stun-output-buffer* (get-buffer-create "*stun-output*"))
  (funcall *stun-start-lisp-function*))

(defun stun-stop () 
  (interactive)
  (delete-process *stun-process*))

;;; Sending command strings to STUN

(defvar *stun-xprop-program* "xprop"
  "Name of the stun command.")

(defvar *stun-default-target* "STUN_COMMAND") ;; :. xprop > 

(defun* stun-send (command &optional (target *stun-default-target*))
  (let* ((arguments (list *stun-xprop-program* 
			  "-root"
			  "-f" target "8s"
			  "-set" target
			  (format "%S" command)))
	 (script (mapconcat #'identity arguments " ")))
    (prog1 script
      (when (not (= 0 (shell-command script)))
	(error "Could not complete xprop call.")))))

;;; Determining XINERAMA head layout					    

(defvar *stun-xdpyinfo-command* "DISPLAY=:0.0 xdpyinfo -ext XINERAMA")

(defstruct stun-head height width x y)

(defvar *stun-head-alist* nil)

(defun stun-get-head-layout ()
  (with-temp-buffer 
    (shell-command *stun-xdpyinfo-command* t)
    (labels ((matched-integer (n)
	       (car (read-from-string (match-string-no-properties n)))))
      (setf *stun-head-alist* nil)
      ;; check for XINERAMA
      (goto-char (point-min))
      (if (search-forward "XINERAMA")
	  (progn 
	    (goto-char (point-min))
	    (while (re-search-forward (rx "head #" 
					  ;; 1. head number
					  (group (one-or-more digit))
					  ":" (one-or-more space)
					  ;; 2. width
					  (group (one-or-more digit))
					  "x" 
					  ;; 3. height
					  (group (one-or-more digit))
					  (one-or-more space) "@" (one-or-more space)
					  ;; 4. x offset
					  (group (one-or-more digit))
					  ","
					  ;; 5. y offset
					  (group (one-or-more digit))) 
				      nil :noerror)
	      (setf *stun-head-alist* 
		    (acons (matched-integer 1)
			   (make-stun-head :width (matched-integer 2)
					   :height (matched-integer 3)
					   :x (matched-integer 4)
					   :y (matched-integer 5))
			   *stun-head-alist*))))
	  ;; no xinerama.
	  (goto-char (point-min))
	  (when (re-search-forward (rx "dimensions:" (one-or-more space)
				       (group (one-or-more digit))
				       "x" (group (one-or-more digit)))
				   nil :noerror)
	    (setf *stun-head-alist*
		  (cons 1 (make-stun-head :width (matched-integer 1)
					  :height (matched-integer 2)
					  :x 0 :y 0)))))
      ;; did we learn anything?
      (when (null *stun-head-alist*)
	(error "Cannot get head layout data.")))))

(defun stun-head-relative-x (head x)
  (+ x (stun-head-x head)))

(defun stun-head-relative-y (head y)
  (+ y (stun-head-y head)))

(defun stun-head-relative-xy (head x y)
  (values (stun-head-relative-x head x)
	  (stun-head-relative-y head y)))

(provide 'stun)
;;; stun.el ends here