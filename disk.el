;;; disk.el --- disk mounter module for STUN

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@monad.lab>
;; Keywords: hardware
;; $Id: disk.el,v 0.7 2007/11/05 03:23:28 dto Exp dto $
;; Time-stamp: <2008-09-04 18:08:42 dto>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This program provides an alternative disk mounting tool. It is
;; implemented as a STUN module.

;; It would probably not be hard to make this configurable so that it
;; works with other operating systems. If you use another operating
;; system and would like to help me get it working there, let me know.
;;
;; Features:
;;  - Scan for attached USB Mass Storage devices and find their UUID's
;;    (see `disk-scan-disks')
;;  - Assign unique names to devices (see `disk-disks-alist')
;;  - Automatically create and configure mount points and UUID's in /etc/fstab
;;    (see `disk-write-fstab', `disk-scan-and-configure')
;;  - Mount/unmount disks interactively, with auto-completion
;;    (see `disk-mount' and `disk-unmount')

;; Future:
;;  - TODO Report new disks when hotplugged? (periodic scan, automount?)
;;  - TODO Configure USB Audio devices with ALSA/JACK. use jack.el!
;;  - TODO fix mount point permissions

;;; Code:

(require 'cl)
(require 'rx)

;;; Finding disks by their UUID's

(defvar disk-uuids ()
  "List of UUID's found during last scan.")

(defvar disk-new-uuids ()
  "List of newly attached UUID's, if any.")

(defvar disk-uuid-directory "/dev/disk/by-uuid"
  "Directory to read when scanning for UUID's.")

(defun* disk-current-uuids (&optional (file disk-uuid-directory))
  "Obtain a list of currently attached UUID's."
  (remove-if #'(lambda (file-name)
		 (string-match (rx ?. (* anything)) file-name))
	    (directory-files disk-uuid-directory)))

(defun disk-format-uuids (uuids &optional show-names)
  (mapconcat (lambda (uuid)
	       (if show-names
		   (let ((name (disk-mount-point-from-uuid uuid)))
		     (format "[%s %s]" (or name "???") uuid))
		 uuid))
	     uuids
	     " "))

(defun disk-scan-disks ()
  "Scan for attached disks and attempt to detect any new disks."
  (interactive)
  (message "Scanning for attached disks...")
  (let* ((uuids (disk-current-uuids))
	 (new-uuids (set-difference uuids disk-uuids :test 'equal)))
    (message "Scanning for attached disks... Done.")
    ;;
    (when uuids
      (message "Found UUIDs: %s" (disk-format-uuids uuids :show-names)))
    (setf disk-uuids uuids)
    ;;
    (when new-uuids 
      (message "New UUIDs: %s" (disk-format-uuids new-uuids :show-names)))
    (setf disk-new-uuids new-uuids)
    uuids))
  
;;; Naming your disks

(defvar disk-disks-alist ()
  "Association list mapping disk names (i.e. mount points) to
property lists whose contents describe the disks.

The list should contain an entry for each of the disks (i.e. DISK
Mass Storage Class devices) that you will be using, assigning a
unique name to each. (If you set `disk-write-fstab-p' to a
non-nil value, then these names will also be used for the
filesystem mount points.)

The following keys are valid in the property lists:

 :uuid             UUID string. 
 :file-system      The file system type. The default is \"xfs\".
 :options          fstab options (i.e. \"user,noauto,quiet\").

The use of disk UUID's makes it less likely that you will mix up
similar-looking devices (such as SD cards). In addition, we don't
have to bother figuring out which device name the disk got
assigned to, because in /etc/fstab you can specify a UUID to
mount, rather than a device file.

If you choose short names for your disks, you can label your SD
cards with one of those permanent markers that writes on dark
surfaces. (I write on the back of the card where there is no
sticker.)

You can find out your devices' UUIDs with the function
`disk-scan-disks', and use `disk-fstab-entries' to generate fstab
entries. The function `disk-write-fstab' will add appropriate
entries to /etc/fstab (via TRAMP by default; use the variable
`disk-fstab-file' to customize this behavior.)

Even simpler, you can use the interactive function
`disk-scan-and-configure' to do both jobs for you. In this case,
changes to /etc/fstab will only be made when the variable
`disk-write-fstab-p' has a non-nil value. (You should set
`disk-disks-alist' before doing this.)

Example:

  (setf disk-disks-alist
        '((\"/zoom/1\" :uuid \"3734-3937\" :file-system \"vfat\")
	  (\"/zoom/2\" :uuid \"2931-A206\" :file-system \"vfat\")
  	  (\"/red500\" :uuid \"e828ddbe-7bd8-442d-8523-a83518bed4de\" 
                       :fstype \"xfs\")))
  (setf disk-write-fstab-p t)
  (disk-scan-and-configure)

After you enter your root password for TRAMP, /etc/fstab will
be updated.

To mount and unmount the disks, use `disk-mount' and `disk-unmount'.
")

;;; Automatically configuring /etc/fstab and mount points

(defun disk-create-mount-point (mount-point)
  (message (format "Creating mount point %s..." mount-point))
  (make-directory (concat "/su::" mount-point) :parents))

(defun disk-create-mount-point-maybe (mount-point)
  (when (not (file-exists-p mount-point))
    (disk-create-mount-point mount-point)))

(defun disk-mount-point-from-uuid (uuid)
  (car-safe (find-if #'(lambda (entry)
			 (string= uuid (getf (cdr entry) :uuid)))
		     disk-disks-alist)))
		     
(defun disk-uuid-from-mount-point (mount-point)
  (getf (cdr-safe (assoc mount-point disk-disks-alist)) :uuid))

(defvar disk-write-fstab-p nil
  "When non-nil, attempt to update /etc/fstab when scanning devices.")

(defvar disk-fstab-file "/su::/etc/fstab" 
  "Tramp address for updating /etc/fstab.")

(defvar disk-fstab-default-option-string "user,noauto")

(defun* disk-fstab-entry-string (mount-point &optional &key 
					    uuid 
					    (file-system "vfat")
					    (options disk-fstab-default-option-string))
  (format "UUID=%s\t%s\t%s\t%s\t0\t0\n"
	  (or uuid (error "Cannot configure mount point with null UUID."))
	  mount-point file-system options))

(defun* disk-fstab-entries (&optional (disks disk-disks-alist))
  "Generate fstab entries for the disk names in the alist NAMES."
  (with-temp-buffer
    (dolist (disk disks)
      (insert (apply #'disk-fstab-entry-string disk)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun* disk-write-fstab ()
  "Add (or replace) fstab entries for the UUID's in `disk-disks-alist'."
  (with-temp-buffer
    (insert-file-contents-literally "/etc/fstab")
    (message "Backing up /etc/fstab...")
    (write-file (concat disk-fstab-file ".bak"))
    (message "Backing up /etc/fstab... Done.")
    (message "Updating /etc/fstab...")
    ;; 
    ;; remove any entries that we are updating...
    (dolist (uuid (mapcar #'(lambda (d)
			      (getf (cdr d) :uuid))
			  disk-disks-alist))
      (disk-create-mount-point-maybe (disk-mount-point-from-uuid uuid))
      (goto-char (point-min))
      (while (re-search-forward (concat "^UUID=" uuid "\\(.\\|\n\\)*$") nil :noerror)
	(replace-match "")))
    ;;
    ;; now append new entries
    (goto-char (point-max))
    (insert (disk-fstab-entries))
    (write-file disk-fstab-file)
    (message "Updating /etc/fstab... Done.")))

(defun disk-write-fstab-maybe (&optional force)
  (when (or force disk-write-fstab-p)
    (when (null disk-disks-alist)
      (error "You must set `disk-disks-alist' before attempting to update /etc/fstab."))
    (disk-write-fstab)))

;;; Choosing and mounting disks

(defvar disk-mount-command-string "mount")

(defvar disk-unmount-command-string "umount")

(defun disk-mounted-disks ()
  "Return a list of currently mounted DISK disks."
  (with-temp-buffer 
    (shell-command disk-mount-command-string t)
    (let (mounted)
      (dolist (disk (mapcar #'car disk-disks-alist))
	(goto-char (point-min))
	(when (search-forward disk nil :noerror)
	  (push disk mounted)))
      mounted)))

(defvar disk-choose-disk-prompt "Choose DISK Mass Storage Class disk: ")

(defun* disk-choose-disk (&optional (choices (mapcar #'car disk-disks-alist)))
  (completing-read disk-choose-disk-prompt choices nil :require-match))

(defun* disk-mount (&optional (mount-point (disk-choose-disk))
			     (command :mount))
  "Mount (or unmount) the device corresponding to MOUNT-POINT.
If no MOUNT-POINT is specified, the user is prompted for a
device (with completion.) 

Mount if COMMAND is :mount, otherwise unmount. The default is :mount."
  (interactive)
  (let ((command-string (if (eq command :mount)
			    disk-mount-command-string
			  disk-unmount-command-string)))
    (destructuring-bind (status output)
	(with-temp-buffer 
	  (list 
	   (call-process command-string nil t nil
			 mount-point)
	   (buffer-substring-no-properties (point-min) (point-max))))
      (message (format "[%s %s] %d %s: %s"
		       command-string mount-point
		       status
		       (if (= 0 status) "OK" "FAILED")
		       output)))))

(defun* disk-unmount (&optional (mount-point (disk-choose-disk)))
  (interactive)
  (disk-mount mount-point :unmount))

(defun disk-scan-and-configure ()
  (interactive)
  (disk-scan-disks)
  (disk-write-fstab-maybe))
  
(provide 'disk)
;;; disk.el ends here
