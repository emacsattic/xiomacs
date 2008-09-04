;;; stun.el --- X11 inter-client communication and display info

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: extensions

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

    
					    
					    


(provide 'stun)
;;; stun.el ends here
