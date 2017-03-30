
(asdf:defsystem :cl-jpeg-tests
  :name "cl-jpeg-tests"
  :description "Test library for cl-jpeg. Kept separate from cl-jpeg to keep cl-jpeg size down."
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :depends-on (cl-jpeg fiveam)
  :serial t
  :components
  ((:file "package")
   (:file "cl-jpeg-tests"))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :cl-jpeg)))
