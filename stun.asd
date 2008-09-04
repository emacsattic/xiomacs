;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :stun-asd)

(in-package :stun-asd)

(asdf:defsystem stun
  :name "STUN"
  :version "0.001"
  :maintainer "David O'Toole"
  :author "David O'Toole"
  :license "General Public License (GPL) Version 3"
  :description "A desktop hyperenvironment in Emacs Lisp and Common Lisp."
  :serial t
  :depends-on (:xlib)
  :components ((:file "stun")))

