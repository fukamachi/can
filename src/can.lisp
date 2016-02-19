(in-package :cl-user)
(defpackage can
  (:use #:cl)
  (:export #:can
           #:rolep

           #:user-roles
           #:user-roles-for-resource
           #:resource-allowed-p))
(in-package :can)

(defgeneric user-roles (user)
  (:method (user)
    '()))

(defgeneric user-roles-for-resource (user resource)
  (:method (user resource)
    '()))

(defgeneric resource-allowed-p (resource action role)
  (:method (resource action role)
    nil))

(defun can (user action resource)
  (flet ((allowed-role-p (role)
           (resource-allowed-p resource action role)))
    (let ((roles (user-roles user)))
      (when (some #'allowed-role-p roles)
        (return-from can t))
      (let ((resource-roles
              (user-roles-for-resource user resource)))
        (when (some #'allowed-role-p resource-roles)
          (return-from can t))

        ;; For users having no roles
        (when (and (null roles)
                   (null resource-roles))
          (allowed-role-p nil))))))

(defun rolep (user role &key (test #'eql))
  (and (find role (user-roles user) :test test)
       t))
