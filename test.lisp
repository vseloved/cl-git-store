(defpackage :cl-git-store-test
  (:use :common-lisp
        :rutils
        :nuts-core
        :cl-fad
        :cl-ppcre
        :cl-git-store))

(in-package :cl-git-store-test)


(defmacro w/usual-td (&body body)
  `(w/teardown (progn
                 (setf cl-git-store::*cur-store* nil)
                 (cl-fad:delete-directory-and-files "/tmp/.git")
                 (delete-file "/tmp/.initial__"))
     ,@body))



(deftest test-git (cmd)
  (check-t (git cmd)))

;; simple test (todo: add if-exists check)
(deftest test-init (store-name
                    &optional (if-exists :supersede) (teardown t))
  (w/usual-td
    (check-t (probe-file (init-store store-name "/tmp/" nil
                                     :if-exists if-exists)))))

#|(deftest test-select (&optional (teardown t))
    (w/usual-td
      (init-store "test1" "/tmp/")
      (init-store "test2" "/tmp/")
      (check-t (probe-file (select-store "test1")))))|#

(deftest test-select (store-name &optional (teardown t))
  (and (check-errs (select-store store-name "/tmp/" :if-does-not-exist :error))
       (w/usual-td
         (check-t (probe-file (select-store store-name "/tmp/"
                                            :if-does-not-exist :create))))))

#|(check #'(lambda (x y) (string-equal x (string-right-trim '(#\Space) y)))
       (git :status)
       "#
# Initial commit
#
nothing to commit")))|#


(deftest test-add-obj (str)
  (check #`(typep _ '(string 40))
         (add-objs '((str . "test")))))

(deftest test-get-obj (str sha1)
  (check #'string= (get-obj sha1) str))

(deftest test-list-objs ()
  (add-obj '(("test" . "test-for-list")))
  (check-t (find '"test-for-list" (list-objs) &key #'cdr)))

(deftest test-rem-obj (sha1)
  (rem-obj sha1)
  (check #'null (find sha1 (list-objs) :key #'car)))


; run

(deftest test-throughout (&optional (teardown t))
  (w/usual-td
    (test-select "test" nil)
    (test-list-objs "test")
    (test-rem-obj "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))



(run-tests (test-git :status)
           #+nil (test-init "test-init")
           (test-throughout))