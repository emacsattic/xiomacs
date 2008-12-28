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

;;; Running XIOMACS

(defvar *xiomacs-program* "/home/dto/xiomacs/xiomacs.lisp")

(defvar *xiomacs-inferior-lisp-program* "sbcl")

(defvar *xiomacs-output-buffer* nil)

(defvar *xiomacs-error-buffer* nil)

(defvar *xiomacs-process* nil)

(defun xiomacs-print-message (string)
  (with-current-buffer *xiomacs-output-buffer*
    (insert string "\n")))

(defvar *xiomacs-message-string-handler-function* #'xiomacs-print-message)

(defvar *xiomacs-partial-message-string* nil)

(defun xiomacs-process-filter (process string)
  (let ((partial-message (or *xiomacs-partial-message-string* "")))
    (setf partial-message (concat partial-message string))
    ;; do we have a complete command?
    (let ((pos (position ?\n partial-message)))
      (if (numberp pos)
	  ;; yes, handle it.
	  (let ((message (subseq partial-message (1- pos))))
	    (insert (format "%s -- %s\n" (current-time-string) string))
	    (funcall *xiomacs-message-string-handler-function* message)
	    ;; any more input? 
	    (setf *xiomacs-partial-message-string*
		  (if (> (length partial-message) (1+ pos))
		      (subseq partial-message (1+ pos))
		      "")))
	  ;; not a complete command. just buffer it.
	  (setf *xiomacs-partial-message-string*
		(concat *xiomacs-partial-message-string* partial-message))))))

(defun xiomacs-start-sbcl-process ()
  (setf *xiomacs-process* (start-process "*xiomacs-process*"
				      nil
				      *xiomacs-inferior-lisp-program*
				      "--load" *xiomacs-program*
				      "--eval" "(xiomacs:xiomacs)")))

(defvar *xiomacs-start-lisp-function* #'xiomacs-start-sbcl-process)
	     
(defun xiomacs-start ()
  (interactive)
  (setf *xiomacs-output-buffer* (get-buffer-create "*xiomacs-output*"))
  (setf *xiomacs-error-buffer* (get-buffer-create "*xiomacs-error*"))
  (funcall *xiomacs-start-lisp-function*))

(defun xiomacs-stop () 
  (interactive)
  (delete-process *xiomacs-process*))

;;; Sending command strings to XIOMACS

(defvar *xiomacs-xprop-program* "xprop"
  "Name of the xiomacs command.")

(defvar *xiomacs-default-target* "XIOMACS_COMMAND") ;; :. xprop > 

(defun* xiomacs-send (command &optional (target *xiomacs-default-target*))
  (let* ((arguments (list *xiomacs-xprop-program* 
			  "-root"
			  "-f" target "8s"
			  "-set" target
			  (format "%S" command)))
	 (script (mapconcat #'identity arguments " ")))
    (prog1 script
      (when (not (= 0 (shell-command script)))
	(error "Could not complete xprop call.")))))

;;; Determining XINERAMA head layout					    

(defvar *xiomacs-xdpyinfo-command* "DISPLAY=:0.0 xdpyinfo -ext XINERAMA")

(defstruct xiomacs-head height width x y)

(defvar *xiomacs-head-alist* nil)

(defun xiomacs-get-head-layout ()
  (with-temp-buffer 
    (shell-command *xiomacs-xdpyinfo-command* t)
    (labels ((matched-integer (n)
	       (car (read-from-string (match-string-no-properties n)))))
      (setf *xiomacs-head-alist* nil)
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
	      (setf *xiomacs-head-alist* 
		    (acons (matched-integer 1)
			   (make-xiomacs-head :width (matched-integer 2)
					   :height (matched-integer 3)
					   :x (matched-integer 4)
					   :y (matched-integer 5))
			   *xiomacs-head-alist*))))
	  ;; no xinerama.
	  (goto-char (point-min))
	  (when (re-search-forward (rx "dimensions:" (one-or-more space)
				       (group (one-or-more digit))
				       "x" (group (one-or-more digit)))
				   nil :noerror)
	    (setf *xiomacs-head-alist*
		  (cons 1 (make-xiomacs-head :width (matched-integer 1)
					  :height (matched-integer 2)
					  :x 0 :y 0)))))
      ;; did we learn anything?
      (when (null *xiomacs-head-alist*)
	(error "Cannot get head layout data.")))))

(defun xiomacs-head-relative-x (head x)
  (+ x (xiomacs-head-x head)))

(defun xiomacs-head-relative-y (head y)
  (+ y (xiomacs-head-y head)))

(defun xiomacs-head-relative-xy (head x y)
  (values (xiomacs-head-relative-x head x)
	  (xiomacs-head-relative-y head y)))

(provide 'xiomacs)
;;; xiomacs.el ends here
