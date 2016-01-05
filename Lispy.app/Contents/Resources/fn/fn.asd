(defpackage #:fn-asd
    (:use :cl :asdf))

(in-package :fn-asd)

(defsystem fn
    :name "fn"
    :version "0.0.1"
    :maintainer "soura.jagat@gmail.com"
    :author "Sourav Datta"
    :licence "MIT"
    :description "A simple package for functional programming with CL"
    :serial t
    :components ((:file "fn")))

