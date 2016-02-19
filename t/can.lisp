(in-package :cl-user)
(defpackage can-test
  (:use #:cl
        #:can
        #:prove)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :can-test)

(plan 9)

;;
;; Initializing...

(defparameter *test-db*
  (asdf:system-relative-pathname :can #P"t/test.db"))

(defvar *previous-logger-stream* nil)
(setf *previous-logger-stream* (mito:logger-stream))
(setf (mito:logger-stream) t)

(when (probe-file *test-db*)
  (delete-file *test-db*))

(mito:connect-toplevel :sqlite3 :database-name *test-db*)

;;
;; DAO table definitions

(defclass user ()
  ((name :col-type (or (:varchar 120) :null)
         :initarg :name
         :initform ""
         :accessor user-name))
  (:metaclass mito:dao-table-class))

;; Relation table for managing user roles
(defclass user-role ()
  ((user :col-type user
         :initarg :user
         :accessor user-role-user)
   (role :col-type (:varchar 50)
         :initarg :role
         :inflate (compose #'make-keyword #'string-upcase)
         :deflate #'string-downcase
         :accessor user-role-role))
  (:metaclass mito:dao-table-class)
  (:primary-key user role)
  (:auto-pk nil)
  (:record-timestamps nil))

;; Retrieving roles from "user-role" table
(defmethod user-roles ((user user))
  (mapcar #'user-role-role
          (mito:retrieve-dao 'user-role :user user)))

(defun add-role (user role)
  (mito:create-dao 'user-role :user user :role role))

;; resource
(defclass entry ()
  ((author :col-type user
           :initarg :author
           :accessor entry-author))
  (:metaclass mito:dao-table-class))

;; Return (:owner) if the user is an author of the entry
(defmethod user-roles-for-resource ((user user) (resource entry))
  (cond
    ((mito:object= user (entry-author resource))
     (list :owner))
    (t
     nil)))

;; All users can see the entry
(defmethod resource-allowed-p ((resource entry) (action (eql :show)) role)
  t)

;; Owner can edit the entry
(defmethod resource-allowed-p ((resource entry) (action (eql :edit)) (role (eql :owner)))
  t)

;; Owner can delete the entry
(defmethod resource-allowed-p ((resource entry) (action (eql :delete)) (role (eql :owner)))
  t)

;; Administrators can delete the entry
(defmethod resource-allowed-p ((resource entry) (action (eql :delete)) (role (eql :admin)))
  t)

(mapc #'mito:ensure-table-exists '(user user-role entry))

(let ((user (mito:create-dao 'user :name "Eitaro")))
  (mito:create-dao 'entry :author user))
(mito:create-dao 'user :name "Yoshimi")

(let* ((owner (mito:find-dao 'user :name "Eitaro"))
       (user (mito:find-dao 'user :name "Yoshimi"))
       (entry (first (mito:retrieve-dao 'entry :author owner))))
  (is (user-roles owner) '()
      "User has no roles")
  (is (user-roles-for-resource owner entry) '(:owner)
      "User is an owner for the entry")
  (ok (resource-allowed-p entry :edit :owner)
      "Entry can be editted by owner")
  (ok (can owner :edit entry)
      "Owner can edit the entry")
  (ok (can user :show entry)
      "Other user can see the entry")
  (ok (not (can user :edit entry))
      "Other user cannot edit the entry")

  ;; Make the user an administrator
  (add-role user :admin)

  (ok (not (can user :edit entry))
      "Now the other user (= admin) can edit the entry")

  (ok (not (rolep owner :admin))
      "Owner is not an administrator")
  (add-role owner :admin)
  (ok (rolep owner :admin)
      "Now owner is an administrator"))

(mito:disconnect-toplevel)
(setf (mito:logger-stream) *previous-logger-stream*)

(finalize)
