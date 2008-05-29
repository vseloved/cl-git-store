(in-package :cl-user)

(defpackage :cl-git-store
  (:use :common-lisp :vs.utils :cl-ppcre)
  (:export :git
	   :git-store
	   :*cur-store*
	   :init-store
	   :select-store
	   :get-obj
	   :rem-obj
	   :add-objs
	   :list-objs))

(in-package :cl-git-store)


(defclass git-store ()
  ((branch)
   (path)))


(defparameter *cur-store* nil)
(defparameter *store-path* nil)


(defun git (command &rest args)
  "Run git <_:arg command /> with <_:arg args /> and return its string output. If :input is present, it should be a string or a stream which is redirected to git process' stdin"
  (let ((input (ignore-errors (getf args :input))))
    (when (and input (not (or (input-stream-p input) (stringp input))))
      (error "Arg :input is not an output stream, but a: ~a" (type-of input)))
    (ignore-errors (remf args :input))
    (string-right-trim '(#\Newline)
		       (with-output-to-string (out)
			 (sb-ext:run-program "git" (mapcar #`(format nil "~(~a~)" _)
							   (nconc (list command) args))
					     :search t :output out
					     :input (if (stringp input)
							(with-input-from-string (in input)
							  in)
							input))))))

(symbol-macrolet ((store-ref `(concatenate 'string ".git/refs/heads/" *cur-store*)))


; initialization

  (defun init-store (store-name store-path &optional initial-data*)
    "Initialize git in the directory <_:arg store-path />, unless it's already initialized in it. Create a branch of the name <_:arg store-name />, or purge such branch, if it already exist, and checkout. Afterwards apply <_:fun add-objs /> to <_:arg initial-data* /> list"
    (w/dir (store-path)
      (if (sb-posix:opendir #p".git/")	; database initialized
	  (git :branch "-d" store-name)	; clear this branch if it exists
	  (git :init-db)))
    (sb-ext:run-program "touch" store-ref) ; create the branch ref
    (setf *cur-store* store-name *store-path* store-path)
    (add-objs initial-data*))
 
  (defun select-store (store-name &optional store-path)
    "Change the branch of an already initialized git store to <_:arg store-name />, either in <_:var *store-path* /> or in <_:arg store-path /> directory"
    (let ((dir (or store-path *store-path*)))
      (w/dir (dir)
	(unless (sb-posix:opendir #p".git/")
	  (error "Git store in the directory: ~a -- not initialized" dir))
	(git :checkout "-f" *cur-store* "deleted-uids"))
      (setf *cur-store* store-name *store-path* dir)))


; object ops

  (macrolet ((ensure-store-selected (&body body)
	       `(if *cur-store* (w/dir (*store-path*)
				  ,@body)
		    (error "No git store selected"))))

    (defun get-obj (sha1)
      "Get a blob object by its <_:arg sha1 /> from a git branch <_:var *cur-store* /> in the <_:var *store-path* /> directory"
      (declare (type (string 40) sha1))
    (ensure-store-selected (git :cat-file sha1)))

    (defun rem-obj (sha1)
      "Remove a blob object by its <_:arg sha1 /> from a git branch <_:var *cur-store* /> in the <_:var *store-path* /> directory. Actually only add object's sha1 to the deleted-uids file not to process it in the future"
      (declare (type (string 40) sha1))
      (ensure-store-selected
       (with-open-file (deleted-uids #p"deleted-uids" :direction :output :if-exists :append :if-does-not-exist :create)
	 (write-string deleted-uids sha1))))
;      (sb-ext:run-program "rm -f" (format nil ".git/ojects/~a/~a" (subseq sha1 0 2) (subseq sha1 2)))))

    (macrolet ((w/current-content (&body body)
		 `(ensure-store-open
		   (let ((show-msg (git :show "--pretty=raw" (with-open-file (ref store-ref)
							       (read ref)))))
		     (cl-ppcre:register-groups-bind (tree-uid) ("tree ([0-9a-f]{40})" :sharedp t)
		       ,@body)))))

      (defun add-objs (obj-id-pairs)
	"Add a bunch of objects with the arbitrary chosen ids for them, given as a list of cons cells <_:arg obj-id-pairs /> to the current store"
	(w/current-content
	  (git :update-ref (concatenate 'string ".git/refs/heads/" *cur-store*)
	       (let ((sha1s nil))
		 (unwind-protect
		      (git :commit-tree
			   (push
			    (git :mktree
				 :input (format nil "040000 tree ~a\tprev\n~a100644 blob ~a\tdeleted-uids"
						tree-uid ; previous tree
						(with-output-to-string (new-content) ; new objects
						  (mapc #`(format new-content "100644 blob ~a\t~a\n"
								  (with-input-from-string (in _)
								    (push (git :hash-object "-w --stdin" :input in) sha1s))
								  (cdr _))
							(car _)))
						(push (git :hash-object "-w" "deleted-uids") sha1s)))) ; deleted-uids file
			   sha1s)

		   ; clean-up the repository, if an error happened, before the tree was commited
		   (mapc #`(sb-ext:run-program "rm -f" (format nil ".git/ojects/~a/~a"
							       (subseq _ 0 2) (subseq _ 2)))
			 sha1s))))))


      (defun list-objs ()
	"List sha1's of all not marked as deleted blob objects in the store"
	(labels ((get-sha1s (tree-uid)
		   (let ((uids (cl-ppcre:all-matches-as-strings "[0-9a-f]{40}"
								(git :ls-tree tree-uid))))
		     (when-it (cdr uids) ; the first object in any tree, except the initial one, is the previous tree, all other objects are blobs. The last object in any tree is the deleted-uids file 
		       (append it (get-sha1s (car uids)))))))

	  (w/current-content
	    (loop for uid in (sort (mapcar #`(parse-integer _ :radix 16) (get-sha1s tree-uid)))
	          with del-uids-left = (sort (with-open-file (deleted-uids #p"deleted-uids")
					       (loop for sha1 = (read in nil) while sha1
						  collect (parse-integer sha1 :radix 16))))
	         if (< uid (car del-uids-left)) collect uid
	         else if (= uid (car del-uids-left))
	           do (setf del-uids-left (cdr del-uids-left)))))))))