

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

The best way to use ADP-PLAIN is to create a subsystem:

```common-lisp
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-plain")
  :build-operation "adp-plain-op"
  :depends-on ("my-system")
  :components ((:scribble "README.scrbl")))
```

First, we need to add `adp-plain` to the `:defsystem-depends-on` list. Second, is really recommended to specify the build operation as `"adp-plain-op"`. This operation is the responsible on generating the files. Thirs, add your main system to the `:depends-on` list. You can add more project if you want. And lastly, we specify the components. In this case we are specifying a single scribble file.

Imagine now that we want to use the @-syntax in our README file. Create the `README.scrbl` file and add the following lines:

```common-lisp
(in-package #:adp-plain)

@output-file["README.md"]
```

As you can see, we are using the `(in-package #:adp-plain)` to make that package the current one. Also, we are using the function `output-file`. It accepts a pathname relative to your system's root directory. In this case, the file is created in the same place the source file `README.scrbl` is.

You can use this function inside lisp files as well:

```common-lisp
;;; functions.lisp

(in-package "my-package")

@adp-plain:output-file["reference.md"]

;; more-code
...
```

## Defining your own functions

ADP-PLAIN just `princ`s every object it receives. So, we can define functions that return a string and it will be printed. For example, we may want a function to print code blocks:

```common-lisp
(in-package #:my-package)

(defun code-block (&rest elements)
  (with-output-to-string (stream)
    (princ "```" stream)
    (terpri stream)
    (loop for element in elements
          do (princ element stream))
    (terpri stream)
    (princ "```" stream)))
```

Now, if you export the symbol `code-block`, then you can just write the following:

```
(in-package #:adp-plain)

@output-file["README.md"]

@my-package:code-block{
All this text will be inside
a code block
statement.
}
```

And you will see this:

```
All this text will be inside
a code block
statement.
```

There is another way to print objects if you need more control. ADP-PLAIN exports the method `adp-plain:print-element`. It receives the elements to be printed and a stream where to print these elements.

The same example with the code block would be:

```common-lisp
(defclass code-block ()
  ((elements :initarg :elements
             :reader code-block-elements)))

(defun code-block (&rest elements)
  (make-instance 'code-block :elements elements))

(defmethod print-element ((element code-block) stream)
  (let ((code-elements (code-block-elements element)))
    (princ "```" stream)
    (terpri stream)
    (loop for code-element in code-elements
          do (princ code-element stream))
    (terpri stream)
    (princ "```" stream)))
```

It is defined different, but it is used the same as the previous example. The advantage of doing this, is that ADP first gather all the data, and later on, it starts to print. You can take advantage of this because you can know what will be printed in the future (storing the data in an array or hash-table).


## Some last words

The examples above are using `output-file` to create a `md` file. But you can use whatever extension you want. In particular, you can create C files. So, you can think of ADP-PLAIN as a way to program in the language you want with the power of lisp. You could make a function that accepts a string denoting a C variable and print a block that open a file, inserts some code, and then closes the file. Something like this:

```common-lisp
(defun with-file (var file &rest elements)
  (format nil
"{
  FILE* ~a = fopen(~s);
  ~{~a~}
  fclose(~a);
}"
          var file elements var))
```

And use it like this:

```c
int main(){
  int k = 0;
  @with-file["file" "~/path-to/my-file.txt"]{
    fprintf(file, "%d", k);
    k++;
    fprintf(file, "One more: %d", k);
  }
  return 0;
}
```

It will be converted to:

```c
int main(){
  int k = 0;
  {
    FILE* file = fopen("~/path-to/my-file.txt");
    fprintf(file, "%d", k);
    k++;
    fprintf(file, "One more: %d", k);
    fclose(file);
  }
  return 0;
}
```

That's all! :D