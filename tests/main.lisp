;;;; tests/main.lisp -- Tests for the cl-i package.
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT


;;; REPL help.
#+(or)
(progn
  (asdf:load-system :com.djhaskin.cl-i)
  (declaim (optimize (speed 0) (space 0) (debug 3))))

(asdf:load-system "parachute")

(asdf:load-system "com.djhaskin.cl-i")

;;; Package definition.
(in-package #:cl-user)
(defpackage #:com.djhaskin.cl-i/tests
  (:use #:cl)
  (:import-from
    #:com.djhaskin.cl-i)
  (:import-from
    #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:is-values
    #:isnt-values
    #:of-type
    #:finish
    #:test)
  (:import-from
    #:com.djhaskin.nrdl)
  (:local-nicknames (#:cl-i #:com.djhaskin.cl-i)
                    (#:nrdl #:com.djhaskin.nrdl)))

(in-package #:com.djhaskin.cl-i/tests)

(defparameter *test-config-file*
  (merge-pathnames
    (make-pathname
      :directory
      (list
        :relative
        "Code"
        "lisp"
        "cl-i"
        "tests")
      :name
      ".cl-i"
      :type
      "nrdl")
    (cl-i:os-specific-home #'uiop/os:getenv)))

(defparameter *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "com.djhaskin.cl-i")
      'asdf/component:absolute-pathname)))

;(def-suite cl-i-main
;           :description "Main functions in test suite."
;           :in cl-i/tests:cl-i)
;(in-suite cl-i-main)

; For the REPL:
;(setf fiveam:*run-test-when-defined* t)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))

(defun broken-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(defmacro signals (&body body)
  `(handler-case
       (progn
         ,@body
         (fail "Should have thrown an error"))
     (t (sig) (true sig))))

(define-test helper-function-tests)

(define-test repeatedly-eq
  :parent helper-function-tests
  (signals (cl-i::repeatedly-eq #'broken-dec 3))
  (is equal (cl-i::repeatedly-eq #'positive-dec 3) '(3 2 1 0)))

(define-test repeatedly
  :parent helper-function-tests
  (signals
    (cl-i::repeatedly
                          #'positive-dec
                          3
                          (lambda (thing) (< thing 0))))
  (is equal (cl-i::repeatedly
               #'positive-dec
               3
               (lambda (item)
                 (<= item 0)))
             '(3 2 1)))

(define-test config-supporting-functions)

(define-test basic-find-file
  :parent config-supporting-functions
  ;;; TODO: Finish him!
  (is equal
      (slot-value
        (asdf:find-system "cl-i")
        'asdf/component:absolute-pathname)
      (cl-i:find-file
        *tests-dir*
        "cl-i"))
  (is eq
      nil
      (cl-i:find-file
        (merge-pathnames
          (slot-value
            (asdf:load-system "cl-i" :force t)
            'asdf/component:absolute-pathname)
          #P"/leaves-of-grass")
        "600dc0d36077a10ada600dd3a10fda7a")))

(define-test basic-slurp-stream
  :parent config-supporting-functions
  (is equal
      (with-open-file
          (f
            (merge-pathnames
              #P".cl-i.nrdl"
              *tests-dir*)
            :direction :input
            :external-format :utf-8)
        (cl-i:slurp-stream f))
      "{ \"hoo\" \"haa\" }"))

(define-test
  base-slurp
  :parent config-supporting-functions
  (is equal
      (cl-i::base-slurp
        *test-config-file*)
      "{ \"hoo\" \"haa\" }"))

(define-test slurp
  :parent config-supporting-functions)

(define-test "slurp paths"
  :parent slurp
    (is equal
        (cl-i:data-slurp
          *test-config-file*)
        "{ \"hoo\" \"haa\" }"))

(define-test "slurp file URL"
  :parent slurp
    (is equal
        (cl-i:data-slurp
          (concatenate 'string
                       "file://"
                       (namestring *test-config-file*)))
        "{ \"hoo\" \"haa\" }"))

(define-test slurp-http-urls
  :parent slurp)

(define-test slurp-http-url-noauth
  :parent slurp-http-urls
    (is
      equal
        (cl-i:data-slurp
          "https://localhost:8443/noauth/complete.txt"
          :insecure t)
        (format nil "noauth complete")))

(define-test slurp-http-url-basic
  :parent slurp-http-urls
  (is
    equal
    (cl-i:data-slurp
      "https://mode:code@localhost:8443/basic/complete.txt"
      :insecure t)
    (format nil "basic complete")))

(define-test slurp-http-url-header
  :parent slurp-http-urls
    (is equal
        (cl-i:data-slurp
          "https://Authorization=Bearer%20600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete")))

(define-test slurp-http-url-token
  :parent slurp-http-urls
    (is equal
        (cl-i:data-slurp
          "https://600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete")))


;;; TODO finish moving these over.
(deftest
  consume-arguments
  (testing
    "other-args"
    (multiple-value-bind (opts other-args)
        (cl-i:consume-arguments
          '("--enable-dark-mode"
            "--reset-dark-mode"
            "--add-dark-mode"
            "crying"
            "--add-dark-mode"
            "firm"
            "well-done"
            "medium-well"
            "--join-my"
            "pride=hurt"
            "--join-my"
            "start=great"
            "--nrdl-fight"
            "15.0"
            "--file-stride"
            "tests/.cl-i.nrdl"))
      (ok (equal
            (nrdl:nested-to-alist opts)
            '((:DARK-MODE "firm" "crying")
             (:FIGHT . 15.0)
             (:MY (:PRIDE . "hurt")
                  (:START . "great"))
             (:STRIDE ("hoo" . "haa")))
            ))
      (ok (equal
            '("well-done" "medium-well")
            other-args)
          "Other-args handling of consume-arguments")))
  (testing
    "empty"
    (ok
      (equal (cl-i:generate-string
               (cl-i:consume-arguments
                 '()))
             (format nil "{~%}"))
      "Empty argument parsing"))
  (testing
    "basic"
    (ok
      (equal (nrdl:nested-to-alist
               (cl-i:consume-arguments
                 '("--enable-foo" "--disable-bar" "baz" "--nrdl-force" "15" "--set-quux" "farquad")))
             '((:BAR) (:FOO . T) (:FORCE . 15) (:QUUX . "farquad")))
      "Basic argument parsing")))

(deftest
  consume-environment
  (testing
    "Basic"
    (ok
      (equal
        '((:FINES ("key" . 155.5))
          (:FORESIGHT . T)
          (:FORKS . "whenceandwhither")
          (:MAPLE "1" "2" "3" "4" "5"))
        (nrdl:nested-to-alist
          (cl-i:consume-environment
            "hello"
            (alexandria:alist-hash-table
              '(("HELLO_LIST_MAPLE" . "1,2,3,4,5")
                ("HELLO_FANGLE_DOG" . "12345")
                ("VARS" . "xtreem")
                ("HELLO_NRDL_FINES" . "{ \"key\": 155.5 }")
                ("HELLO_FLAG_FORESIGHT" . "0")
                ("HELLO_ITEM_FORKS" . "whenceandwhither")))))))))

(defun blank-command
  (options)
  (format t "Options:~&  ~A~&" (nrdl:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :successful))))

(defun error-command
  (options)
  (format t "Options:~&  ~A~&" (nrdl:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :general-error))))

(defun io-error
  (options)
  (setf (gethash :status options) :input-output-error)
  options)

(deftest config-file-options
         (testing
           "typical invocation"
           (ok
             (equal
               (nrdl:nested-to-alist
                 (cl-i:config-file-options
                   "hi"
                   (alexandria:alist-hash-table
                     #+windows
                     '(
                       ("USERPROFILE" . "C:\\Users\\djh")
                       )
                     #-windows
                     '(
                       ("HOME" . "/home/skin")
                       )
                     :test #'equal
                     )
                   (make-hash-table)))
               '((:CHIVES
                  (:LOVE . 15)
                  (:SORE_LOSERS
                   (:COOL . :BEANS)
                   (:STATE . "virginia"))
                  (:SPICES . T))
                 (:DOT . nil)
                 (:GARY . 3)
                 (:HAIRY . 4)
                 (:SLASH . cl:null))))))

(deftest
  execute-program
  (testing
    "empty cases"
    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          (make-hash-table)
          nil)
        'cl-i:necessary-env-var-absent)
      "Necessary environment variables absent")
    (multiple-value-bind (code outcome)
        (cl-i:execute-program
          "Halo"
          (alexandria:alist-hash-table
            #+windows
            '(
              ("USERPROFILE" . "C:\\Users\\djh")
              )
            #-windows
            '(("HOME" . "/home/djha-skin")
              )
            :test #'equal)
          nil)
      (ok
        (equal
          (nrdl:nested-to-alist outcome)
          '((:ERROR-MESSAGE . "The subcommand `` has no actions defined for it.")
            (:ERROR-TYPE . "CL-I:INVALID-SUBCOMMAND") (:GIVEN-SUBCOMMAND)
            (:STATUS . :CL-USAGE-ERROR))))
      (ok
        (equal
          code
          64))))
  (testing
    "typical invocation"
    (multiple-value-bind (code outcome)
      (cl-i:execute-program
        "hi"
        (alexandria:alist-hash-table
          #+windows
          '(
            ("USERPROFILE" . "C:\\Users\\djh")
            ("HI_ITEM_FOUR" . "square")
            ("HI_LIST_LOVERS" . "so,many,lovers")
            ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
            )
          #-windows
          '(
          ("HOME" . "/home/djha-skin")
          ("HI_ITEM_FOUR" . "square")
          ("HI_LIST_LOVERS" . "so,many,lovers")
          ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
          )
        :test #'equal
        )
      `((nil . ,#'blank-command)
        (("error") . ,#'error-command)
        (("io-error") . ,#'io-error))
      :cli-arguments
      '(
        "--join-deals" "a=jhyghyjub"
        "--join-deals" "c=d"
        "--add-barf" "1"
        "--add-barf" "2"
        "--add-barf" "3"
        "--enable-gary"
        "--reset-gary"
        "--set-gary" "four"
        "--disable-all-the-things"
        "io-error"
        ))
      (list
        code
        (nrdl:nested-to-alist outcome))
    (ok (equal (nrdl:nested-to-alist outcome)

               '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
                                   (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . :BEANS) (:STATE . "virginia"))
                                            (:SPICES . T))
                                   (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT . nil) (:FOUR . "square")
                                   (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
                                   (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
                                        (:CONTENTS . "lots"))
                                   (:SLASH . cl:null)
                                   (:STATUS . :INPUT-OUTPUT-ERROR)
                                   ))
        "Typical invocation hash table check")
    (ok (equal code 74)
        "Typical invocation exit code check"))))

;;#(defmacro write-or-check-nrdl (thing strm file expected actual)
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#(deftest
;;#  default-help
;;#  (testing
;;#    "empty cases"
;;#    (let ((strstrm (string-stream
;;#    (with-output-to-string (out)
;;#      (default-help
;;#        out 
;;#        "halo"
;;#        nil
;;#        nil
;;#        nil
;;#        (uiop/os:getcwd)
;;#        nil 
;;#        ","
;;#        "="))
;;#  (testing
;;#    "typical case"
;;#    (alexandria:hash-table-alist
;;#      (cl-i::default-help
;;#        t
;;#        "halo"
;;#        (alexandria:alist-hash-table
;;#          '((:a . 1)
;;#            (:b . 2)
;;#            (:c . 3)))
;;#        '("hello" "world")
;;#        nil
;;#        (uiop/os:getcwd)
;;#        '(
;;#          (("hello" "world") . "
;;#                             This is nonsense.
;;#                             "))
;;#                             ","
;;#                             "="))
;;#
;;#
;;#    (ok
;;#      (signals
;;#        (cl-i:execute-program
;;#          "Halo"
;;#          (make-hash-table)
;;#          nil)
;;#        'cl-i:necessary-env-var-absent)
;;#      "Necessary environment variables absent")
;;#    (ok
;;#      (signals
;;#        (cl-i:execute-program
;;#          "Halo"
;;#          (alexandria:alist-hash-table
;;#            #+windows
;;#            '(
;;#              ("USERPROFILE" . "C:\\Users\\djh")
;;#              )
;;#            #-windows
;;#            '(("HOME" . "/home/djha-skin")
;;#              )
;;#            :test #'equal)
;;#          nil)
;;#        'cl-i:invalid-subcommand)
;;#      "No test provided in hash table args"))
;;#  (testing
;;#    "typical invocation"
;;#    (multiple-value-bind (code outcome)
;;#      (cl-i:execute-program
;;#        "hi"
;;#        (alexandria:alist-hash-table
;;#          #+windows
;;#          '(
;;#            ("USERPROFILE" . "C:\\Users\\djh")
;;#            ("HI_ITEM_FOUR" . "square")
;;#            ("HI_LIST_LOVERS" . "so,many,lovers")
;;#            ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
;;#            )
;;#          #-windows
;;#          '(
;;#          ("HOME" . "/home/djha-skin")
;;#          ("HI_ITEM_FOUR" . "square")
;;#          ("HI_LIST_LOVERS" . "so,many,lovers")
;;#          ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
;;#          )
;;#        :test #'equal
;;#        )
;;#      `((nil . ,#'blank-command)
;;#        (("error") . ,#'error-command)
;;#        (("io-error") . ,#'io-error))
;;#      :cli-arguments
;;#      '(
;;#        "--join-deals" "a=jhyghyjub"
;;#        "--join-deals" "c=d"
;;#        "--add-barf" "1"
;;#        "--add-barf" "2"
;;#        "--add-barf" "3"
;;#        "--enable-gary"
;;#        "--reset-gary"
;;#        "--set-gary" "four"
;;#        "--disable-all-the-things"
;;#        "io-error"
;;#        ))
;;#    (ok (equal (nrdl:nested-to-alist outcome)
;;#
;;#               '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
;;#                                   (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . :BEANS) (:STATE . "virginia"))
;;#                                            (:SPICES . T))
;;#                                   (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT . nil) (:FOUR . "square")
;;#                                   (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
;;#                                   (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
;;#                                        (:CONTENTS . "lots"))
;;#                                   (:SLASH)
;;#                                   (:STATUS . :INPUT-OUTPUT-ERROR)
;;#                                   )))
;;#    (ok (equal code 74)))))
