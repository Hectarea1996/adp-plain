

# Add Documentation, Please... with Plain text!

Welcome to `adp-plain`, an extension to the ADP project.

This extension lets you chose the file to be stored (even its extension). Also it exports some facilities to let you define your own adp functions.

## Installation

Add [Ultralisp](https://ultralisp.org/) to Quicklisp:

```common-lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```

The project will be installed automatically when you `:defsystem-depends-on` it. See below.

## Usage

In your system definition, add `adp-plain` to the `:defsystem-depends-on` list.

```common-lisp
(defsystem "my-system"
  :defsystem-depends-on ("adp-plain")
  :components ((:file "package")
               (:file "functions.lisp")
               (:scribble "README.scrbl")))
```

Imagine now that we want to use the @-syntax in our README file. Create the `README.scrbl` file and add the following line:

```common-lisp
@output-file["README.md"]
```

As you can see, we are using the function `output-file`. It accepts a pathname relative to your system's root directory. In this case, the file is created in the same place the source file `README.scrbl` is.

You can use this function in your lisp files as well, but remember to use the package prefix in this case:

```common-lisp
;;; functions.lisp

(in-package "my-package")

@adp-plain:output-file["reference.md"]

;; more-code
...
```

## Defining your own adp functions

Defining an adp function is done with `adp-plain:define-plain-function`. You should return the object to be printed. For example, if we want to print in our `README.md` file italic sentences we can define the following function:

```common-lisp
(adp-plain:define-plain-function italic (text)
  (format nil "*~a*" text))
```

The syntax of this macro is exactly the same as `defun`.

Now, you can use `@italic{My text}` wherever you want, in both scribble and lisp files.

If you need more control about how to print something, you can define a class and specialize the method `adp-plain:process-element`. The `italic` example would be:

```common-lisp
(defclass italic-element ()
  ((text :initarg :text
         :reader italic-text)))

(adp-plain:define-plain-function italic (text)
  (make-instance 'italic-element :text text))

(defmethod adp-plain:process-element ((element italic-element) stream)
  (format stream "*~a*" (italic-text element)))
```

As you can see, `process-element` receives the element to be printed and a stream where to print in the contents.

That's all! :D