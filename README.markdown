# Can

Can is a role-based access right control library.

## Usage

```common-lisp
(defclass user () ()
  (:metaclass mito:dao-table-class))

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

(defclass entry ()
  ((author :col-type user
           :initarg :author
           :accessor entry-author))
  (:metaclass mito:dao-table-class))

;; Return (:owner) if the user is an author of the entry
(defmethod user-roles-for-resource ((user user) (resource entry))
  (cond
    ((object= user (entry-author resource))
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

(let* ((user (mito:find-dao 'user :id 1))
       (entry (first (mito:retrieve-dao 'entry :author user))))
  (can user :edit entry))
;=> T
```

## Installation

```common-lisp
(ql:quickload :can)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
