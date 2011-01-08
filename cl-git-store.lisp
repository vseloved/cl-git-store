(in-package :cl-user)

(defpackage :cl-git-store
  (:use :common-lisp :rutils.usr :cl-ppcre)
  (:export :git
           :init-store
           :select-store
           :get-obj
           :rem-obj
           :add-objs
           :list-objs))

(in-package :cl-git-store)

(locally-enable-literal-syntax :sharp-backq)

(defmacro w/dir ((path) &body body)
  "cd to the dir, specified by PATH (unless it's nil), if not in it already,
and afterwards return"
  (w/uniqs (old-dir new-dir)
    `(let ((,new-dir ,path))
       (when ,new-dir
         (let ((,old-dir (sb-posix:getcwd)))
           (unless (equalp ,old-dir ,new-dir)
             (sb-posix:chdir ,new-dir)
             (unwind-protect
                  (progn ,@body)
               (sb-posix:chdir ,old-dir))))))))



(defparameter *cur-store* nil)
(defparameter *store-path* nil)
(defparameter *deleted-uids* nil)

(defun git (command &rest args)
  "Run git COMMAND with ARGS and return its string output.
If :input is present, it should be a string or a stream which is redirected
to git process' stdin"
  (let ((input (ignore-errors (getf args :input))))
    (when (and input
               (not (or (and (streamp input)
                             (input-stream-p input))
                        (stringp input))))
      (error "Arg :input is not an input stream or string, but a ~a"
             (type-of input)))
    (ignore-errors (remf args :input))
    (string-right-trim '(#\Newline)
                       (with-output-to-string (out)
                         (sb-ext:run-program "git"
                                             (mapcar #`(format nil "~(~a~)" _)
                                                     (cons command args))
                                             :search t :output out
                                             :input (if (stringp input)
                                                        (make-string-input-stream input)
                                                        input))))))

(defun git-initialized? (path)
  (fad:directory-exists-p (format nil "~a/.git/" path)))

(symbol-macrolet ((store-ref (format nil ".git/refs/heads/~a" *cur-store*)))

  (defun init-store (store-name store-path &optional initial-data
                     &key (if-exists :supersede))
    "Initialize git in the directory STORE-PATH, unless it's already
initialized in it. Create a branch of the name STORE-NAME, if it doesn't
exist (if it does, depending on the value of IF-EXISTS:
* :supersede - purge it
* :append - do nothing
* :rename - save the old branch with the name with the suffix -old
Afterwards apply ADD-OBJS to INITIAL-DATA list.
Returns the filepath of the git reference to a branch"
    (setf *cur-store* store-name
          *store-path* store-path)
    (w/dir (store-path)
      (if (git-initialized? store-path) ; TODO: add handler for incorrect store
          (ecase if-exists
            (:supersede (git :branch "-D" store-name))
            (:append nil)
            (:rename (sb-ext:run-program "mv"
                                         (list "-f" store-ref
                                               (format nil "~a-old" store-ref))
                                         :search t)))
          (progn
            (git :init)
            ;; initial commit
            (with-open-file (initial (format nil "~a/.initial__" store-path)
                                     :direction :output
                                     :if-does-not-exist :create :if-exists nil))
                      (git :add ".initial__")
                      (git :commit "-m" "initial commit")))
           (git :branch store-name)
           ; mark the initial dummy file for removal
           (rem-obj "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391" :commit nil)
           (add-objs initial-data))
    (format nil "~a/.git/refs/heads/~a" store-path store-name))

  (defun select-store (store-name &optional store-path
                       &key (if-does-not-exist :error))
    "Change the branch of an already initialized git store to STORE-NAME,
either in *store-path*> or in STORE-PATH directory. If the store isn't
initialized, depending on IF-DOES-NOT-EXIST, :error or :create.
Returns the filepath of the git reference to a branch"
    (let ((dir (or store-path *store-path*)))
      (w/dir (dir)
             (unless (git-initialized? dir) ; TODO: add handler for incorrect store
               (ecase if-does-not-exist
                 (:error (error "Git store in the directory: ~a -- not initialized" dir))
                 (:create (init-store store-name dir))))
             (setf *cur-store* store-name
                   *store-path* dir)
             (format nil "~a/.git/refs/heads/~a" dir store-name))))

  (macrolet ((ensure-store-selected (&body body)
               `(if *cur-store*
                    (w/dir (*store-path*)
                      ,@body)
                    (error "No git store selected")))
             (w/current-content (&body body)
               `(ensure-store-selected
                 (ppcre:register-groups-bind (tree-uid)
                     ("tree ([0-9a-f]{40})" (git :show "--pretty=raw"
                                                 (with-open-file (ref (format nil "~a/~a" *store-path* store-ref))
                                                   (read ref nil)))
                                            :sharedp t)
                   ,@body)))
             (commit-tree (tree-str)
               `(w/instr (commit-rez (git :commit-tree
                                          (git :mktree :input ,tree-str)))
                  (read-line commit-rez) ; skip 1 line
                  (let ((rez (read-line commit-rez)))
                    (git :update-ref (format nil "refs/heads/~a" *cur-store*) rez)
                    rez))))

    (defun get-obj (sha1)
      "Get a blob object by its SHA1 from a git branch *cur-store*
in the  *store-path* directory"
      (declare (type (string 40) sha1))
      (ensure-store-selected (git :cat-file "blob" sha1)))

    (defun get-id (sha1)
      "Get a blob object id by its SHA1 from a git branch *cur-store*
in the *store-path* directory"
      (declare (type (string 40) sha1))
      (w/current-content
       (ppcre:register-groups-bind (id)
           ((strcat sha1 "\\s+(\\w*)") (git :ls-tree tree-uid) :sharedp t)
         id)))

    (defun rem-obj (sha1 &key (commit t) rm)
      "Remove a reference to an object by its SHA1 from a git branch *cur-store*
in the *store-path* directory. If :commit, commit the changes, otherwise wait
for the next commit. If :rm remove the object"
      (declare (type (string 40) sha1))
      (w/current-content
        (push sha1 *deleted-uids*)
        (when commit
          (commit-tree (with-output-to-string (new-tree)
                         (loop :with raw-data := (make-string-input-stream
                                                  (git :ls-tree tree-uid))
                               :for line := (read-line raw-data nil) :while line
                               :unless (find (ppcre:scan-to-strings "[0-9a-f]{40}" line)
                                             *deleted-uids* :test #'string=)
                               :do (write-line line new-tree))))
          (setf *deleted-uids* nil)))
      (when rm
        (delete-file (format nil "~a/.git/objects/~a/~a"
                             *store-path* (subseq sha1 0 2) (subseq sha1 2)))))

    (defun rem-objs (&rest sha1s)
      "Remove a group of objects by their SHA1S from a git branch *cur-store*
in the *store-path* directory and commit the changes"
      (mapcar #'rem-obj (butlast sha1s))
      (rem-obj (last1 sha1s) :commit t))

    (defun add-objs (&rest obj-id-pairs)
      "Add a bunch of objects with the arbitrary chosen ids for them,
given as an alist OBJ-ID-PAIRS to the current store.
Returns the sha1 of the commit object"
      (flet ((emit-obj-record (stream obj-id-pair)
               (format stream "100644 blob ~a~a~a~%"
                       (car obj-id-pair) #\Tab (cdr obj-id-pair))))

        (when *deleted-uids*
          (rem-obj (last1 *deleted-uids*)))

        (when obj-id-pairs
          (w/current-content
            (commit-tree (with-output-to-string (new-tree)
                           (mapc #`(emit-obj-record new-tree _)
                                 (remove-duplicates
                                  (append (list-objs)
                                          (mapcar #`(cons (git :hash-object "-w" "--stdin" :input (car _))
                                                          (cdr _))
                                                  obj-id-pairs))
                                  :test #'equalp))))))))

    (defun list-objs ()
      "List pairs of sha1s and ids of all objects in the store"
      (w/current-content
        (mapcar #`(apply #'cons (ppcre:split "\\s+" _))
                (ppcre:all-matches-as-strings "[0-9a-f]{40}\\s+\\w+"
                                              (git :ls-tree tree-uid)))))))
