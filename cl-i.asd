(defsystem "cl-i"
  :version "0.5.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "arrows"
               "alexandria"
               "com.inuoe.jzon"
               "dexador"
               "quri"
               "cl-ppcre"
               "trivial-features"
               "trivial-package-local-nicknames"
               )
  :components ((:module "src"
          :components
          ((:file "main"))))
  :description "CLI library for Common Lisp. Handles args, envvars, and conf"
  :in-order-to ((test-op (test-op "cl-i/tests"))))

(defsystem "cl-i/tests"
  :version "0.5.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "cl-i"
      "alexandria"
      "cl-ppcre"
      "rove"
      "cl-ppcre")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-i"
  :perform (test-op (op c) (symbol-call :rove :run c)))
