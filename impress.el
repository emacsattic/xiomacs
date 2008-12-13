;;; impress.el --- group, process, and publish files

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: tools, files

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

;; TODO groups
;; TODO formats
;; TODO paths
;; TODO timestamps
;; TODO processors
;; TODO preamble/postamble div generation

;;; Code:

(require 'cl)
(require 'org)

;;; Groups

(defvar *impress-groups* nil)

(defstruct impress-group 
  name description files base-directory format include exclude)

(defun impress-match-regexp-or-list (filename regexp-or-list)
  (if (null regexp-or-list)
      nil
      (etypecase regexp-or-list
	(string (string-match regexp-or-list filename))
	(list (member filename regexp-or-list)))))

(defun impress-filter-files (files regexp-or-list)
  (labels ((match (filename)
	     (impress-match-regexp-or-list filename regexp-or-list)))
    (remove-if #'match files)))

(defun impress-create-group (&rest forms)
  (apply #'make-impress-group forms))

(defun impress-get-group-files (group)
  (let ((dir (impress-group-base-directory group)))
    (labels ((expand (filename)
	       (expand-file-name filename dir)))
      (let* ((include (impress-group-include group))
	     (files (etypecase include
		      (string (directory-files dir nil include))
		      (list (mapcar #'expand include)))))
	(mapcar #'expand 
		(remove-duplicates 
		 (impress-filter-files files (impress-group-exclude group))
		 :test 'equal))))))

(defun impress-load-group-files (group)
  (setf (impress-group-files group)
	(impress-get-group-files group)))

;;; Paths

(defvar *impress-paths nil)

(defstruct impress-path 
  name source-format destination-format description
  destination-directory processor publisher file-namer options)

(defun impress-create-path (&rest forms)
  (apply #'make-impress-path forms))

(defun impress-initialize ()
  (interactive)
  (setf *impress-groups* (make-hash-table :test 'equal)
	*impress-paths* (make-hash-table :test 'equal)))

(defun impress-publish-org->html (filename destination-dir &optional options)
  (find-file filename)
  (org-export-as-html nil :hidden options nil nil destination-dir))

;;; Publishing a group to a path

(defun impress-publish (group path)
  (let ((files (impress-group-files group))
	(destination (impress-path-destination-directory path))
	(options (impress-path-options path))
	(publisher (impress-path-publisher path)))
    (dolist (file files)
      (save-window-excursion
	(funcall publisher file destination options)))))

;;; well it works...      

(provide 'impress)
;;; impress.el ends here
