;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :xiomacs-asd)

(in-package :xiomacs-asd)

(asdf:defsystem xiomacs
  :name "XIOMACS"
  :version "0.001"
  :maintainer "David O'Toole"
  :author "David O'Toole"
  :license "General Public License (GPL) Version 3"
  :description "A desktop hyperenvironment in Emacs Lisp and Common Lisp."
  :serial t
  :depends-on (:clx)
  :components ((:file "stun")
	       (:file "clon")))

