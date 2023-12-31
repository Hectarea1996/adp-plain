
(in-package #:adp-plain)


(adp:define-adp-operation adp-plain-op)


(defclass output-file-element ()
  ((path :initarg :path
         :reader output-file-path
         :type pathname)))

(defun output-file (path)
  (make-instance 'output-file-element :path path))

(defvar *output-path* nil)

(defgeneric print-element (element stream)
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
  (loop for file across files
        do (let* ((*output-path* #p"")
                  (content (with-output-to-string (stream)
                             (loop for element across (adp:file-elements file)
                                   do (print-element element stream))))
                  (file-path (asdf:component-pathname (adp:file-component file)))
                  (relative-path (ensure-relative-path *output-path* (pathname-name file-path) (pathname-type file-path)))
                  (target-path (merge-pathnames relative-path (asdf:system-source-directory system))))
             (warn "Exporting ~a" target-path)
             (with-open-file (file-str target-path :direction :output :if-exists :supersede
                                                   :if-does-not-exist :create)
               (princ content file-str))
             (warn "Completed"))))
