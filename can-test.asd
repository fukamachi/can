#|
  This file is a part of can project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage can-test-asd
  (:use :cl :asdf))
(in-package :can-test-asd)

(defsystem can-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:can
               :mito
               :alexandria
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "can"))))
  :description "Test system for can"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
