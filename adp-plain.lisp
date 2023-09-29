
(in-package #:adp-plain)


(adp:define-adp-operation adp-plain-op)


(defmacro define-plain-function (name args &body body)
  (check-type name symbol)
  (check-type args list)
  (let ((adp-name (intern (symbol-name name) "ADP-USER"))
        (args-sym (gensym "ARGS")))
    `(progn
       (defun ,adp-name ,args
         ,@body)
       (defmacro ,name (&rest ,args-sym)
         (when adp:*adp*
           `(adp:add-element (,',adp-name ,@,args-sym)))))))

(defclass output-file-element ()
  ((path :initarg :path
         :reader output-file-path
         :type pathname)))

;; (defun adp-user::output-file (path)
;;   (check-type path (or pathname stream string))
;;   (make-instance 'output-file-element :path (pathname path)))

;; (defmacro output-file (path)
;;   (when adp:*adp*
;;     `(adp:add-element (adp-user::output-file ,path))))

(define-plain-function output-file (path)
  (check-type path (or pathname stream string))
  (make-instance 'output-file-element :path (pathname path)))


(defvar *output-path* nil)

(defgeneric process-element (element stream)
  (:method ((element t) stream)
    (princ element stream))
  (:method ((element output-file-element) stream)
    (setf *output-path* (output-file-path element))))


(defun ensure-relative-path (path default-name default-type)
  (let ((target-dir (let ((dir (pathname-directory path)))
                      (cons :relative (cdr dir))))
        (target-name (or (pathname-name path) default-name))
        (target-type (or (pathname-type path) default-type)))
    (make-pathname :directory target-dir :name target-name :type target-type)))


(defmethod adp:export-content ((op adp-plain-op) files system)
  (maphash (lambda (file-path file)
             (let* ((*output-path* #p"")
                    (content (with-output-to-string (stream)
                               (loop for element across (adp:file-elements file)
                                     do (process-element element stream))))
                    (relative-path (ensure-relative-path *output-path* (pathname-name file-path) (pathname-type file-path)))
                    (target-path (merge-pathnames relative-path (asdf:system-source-directory system))))
               (warn "Exporting ~a" target-path)
               (with-open-file (file-str target-path :direction :output :if-exists :supersede
                                                     :if-does-not-exist :create)
                 (princ content file-str))
               (warn "Completed")))
           files))
