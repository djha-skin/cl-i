;;;; errors.lisp -- Error and exit status integration for CL-I.
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT

;;; Some golly fluff.
#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; Package definition.
(in-package #:cl-user)
(defpackage
  #:com.djhaskin.cl-i/errors
  (:use #:cl)
  (:import-from #:com.djhaskin.nrdl)
  (:local-nicknames (#:nrdl #:com.djhaskin.nrdl))
  (:documentation
    "
    Package that defines errors that CL-I knows the exit codes for.
    Also defines exit codes for well-known CL errors.

    These errors are intended to be used to define other conditions
    in programs calling CL-I.  The exit codes are defined in the
    `*exit-codes*` hash table.  The keys are the keywords that
    represent the errors and the values are the exit codes.

    The exit codes are taken from the /usr/include/sysexits.h file.
    ")
  (:export
    *exit-codes*
    exit-status
    exit-map-members))
(in-package #:com.djhaskin.cl-i/errors)

(defparameter *exit-codes*
  ;; taken from /usr/include/sysexit.h
  (alexandria:alist-hash-table
    '(
      (:unknown-error . 128)
      (:general-error . 1)
      (:successful . 0)
      (:cl-usage-error . 64)
      (:data-format-error . 65)
      (:no-input-error . 66)
      (:no-user-error . 67)
      (:no-host-error . 68)
      (:service-unavailable . 69)
      (:internal-software-error . 70)
      (:system-error . 71)
      (:os-file-error . 72)
      (:cant-create-file . 73)
      (:input-output-error . 74)
      (:temporary-failure . 75)
      (:protocol-error . 76)
      (:permission-denied . 77)
      (:configuration-error . 78))))

(defgeneric exit-status (condition)
  (:documentation
   "Return the exit code for the given condition."))

(defgeneric exit-map-members (condition)
  (:documentation
   "Return an alist of items to be added to the exit map of CL-I."))

;;; By default, we don't add anything to the exit map.
(defmethod exit-map-members ((condition condition))
  `(
    (:error-type . ,(prin1-to-string (type-of condition)))))

;;; We define exit codes for the standard CL conditions.
;;; Condition Type SERIOUS-CONDITION
(defmethod exit-status ((condition serious-condition))
  :general-error)

;;; Condition Type ARITHMETIC-ERROR
;;; Condition Type DIVISION-BY-ZERO
;;; Condition Type FLOATING-POINT-INVALID-OPERATION
;;; Condition Type FLOATING-POINT-OVERFLOW
;;; Condition Type FLOATING-POINT-UNDERFLOW
(defmethod exit-status ((condition arithmetic-error))
  (gethash :internal-software-error
           *exit-codes*))

(defmethod exit-map-members ((condition arithmetic-error))
  (let ((operands (arithmetic-error-operands condition))
        (operation (arithmetic-error-operation condition)))
    (concatenate
      'list
      `((:operands . ,(mapcar #'prin1-to-string operands))
        (:operation .
         ,(etypecase operation
            (list
              (mapcar #'prin1-to-string operation))
            (t
              (prin1-to-string operation)))))
      (call-next-method condition))))

;;; Condition Type CELL-ERROR
;;; Condition Type UNBOUND-VARIABLE
;;; Condition Type UNDEFINED-FUNCTION
(defmethod exit-status ((condition cell-error))
  :internal-software-error)

(defmethod exit-map-members ((condition cell-error))
  (let ((name (cell-error-name condition)))
    (concatenate
      'list
      `((:error-cell-name . ,(prin1-to-string name)))
      (call-next-method condition))))

;;; Condition Type CONTROL-ERROR
(defmethod exit-status ((condition cell-error))
  :internal-software-error)

;;; Condition Type FILE-ERROR
(defmethod exit-status ((condition file-error))
  (gethash :input-output-error
           *exit-codes*))

(defmethod exit-map-members ((condition file-error))
  (let ((pathname (file-error-pathname condition)))
    (concatenate
      'list
      `((:error-pathname . ,(namestring pathname)))
      (call-next-method condition))))

;;; Condition Type PACKAGE-ERROR
(defmethod exit-status ((condition package-error))
  :internal-software-error)

(defmethod exit-map-members ((condition package-error))
  (let ((package (package-error-package condition)))
    (concatenate
      'list
      `((:error-package . ,(package-name package)))
      (call-next-method condition))))

;;; Condition Type PARSE-ERROR
;;; Condition Type READER-ERROR
(defmethod exit-status ((condition parse-error))
  :data-format-error)

;;; Condition Type PRINT-NOT-READABLE
(defmethod exit-status ((condition print-not-readable))
  ;; This one is ambiguous. Can I not print because of a bad return value?
  ;; Or because the object is not printable?
  (gethash :internal-software-error
           *exit-codes*))

(defmethod exit-map-members ((condition print-not-readable))
  (let ((object (print-not-readable-object condition)))
    (concatenate
      'list
      `((:error-object . ,(prin1-to-string object)))
      (call-next-method condition))))

;;; Condition Type PROGRAM-ERROR
(defmethod exit-status ((condition program-error))
  :internal-software-error)

;;; Condition Type TYPE-ERROR
(defmethod exit-status ((condition type-error))
  :data-format-error)

(defmethod exit-map-members ((condition type-error))
  (let ((datum (type-error-datum condition))
        (expected-type (type-error-expected-type condition)))
    (concatenate
      'list
      `((:error-datum . ,(prin1-to-string datum))
        (:error-expected-type . ,(prin1-to-string expected-type)))
      (call-next-method condition))))

;;; Condition Type STORAGE-CONDITION
(defmethod exit-status ((condition storage-condition))
  :system-error)

;;; Condition Type STREAM-ERROR
;;; Condition Type END-OF-FILE
(defmethod exit-status ((condition stream-error))
  :input-output-error)

(defmethod exit-map-members ((condition stream-error))
  (let ((stream (stream-error-stream condition)))
    (concatenate
      'list
      `((:error-stream . ,(prin1-to-string stream)))
      (call-next-method condition))))
