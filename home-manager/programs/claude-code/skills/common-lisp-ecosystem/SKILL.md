---
name: Common Lisp Ecosystem
description: This skill should be used when the user asks to "write common lisp", "CLOS", "ASDF", "defpackage", "defsystem", or works with Common Lisp, SBCL, or Coalton. Provides comprehensive Common Lisp ecosystem patterns and best practices.
version: 0.1.0
---

<purpose>
Provide comprehensive patterns for Common Lisp, CLOS, ASDF system definition, SBCL-specific features, and Coalton integration.
</purpose>

<common_lisp_fundamentals>
<concept name="s_expressions">
Code and data share the same syntax (homoiconicity).
Enables powerful macro systems for code transformation.
</concept>

<concept name="symbols">
First-class named objects used for identifiers.
Interned in packages, can have value, function, and property list.
</concept>

<concept name="multiple_values">
Functions can return multiple values.
<pattern name="return">values, multiple-value-bind, multiple-value-list</pattern>
</concept>

<concept name="dynamic_binding">
Special variables with dynamic scope using defvar/defparameter.
Convention: *earmuffs* for special variables.
</concept>
</common_lisp_fundamentals>

<clos>
<description>Common Lisp Object System - Generic functions and multiple dispatch</description>

<pattern name="defclass">
<example>
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age))
  (:documentation "Represents a person."))
</example>
<slot_options>:initarg, :initform, :accessor, :reader, :writer, :type, :documentation</slot_options>
</pattern>

<pattern name="defgeneric_defmethod">
<example>
(defgeneric greet (entity)
  (:documentation "Greet an entity."))

(defmethod greet ((p person))
  (format t "Hello, ~a!~%" (person-name p)))
</example>
</pattern>

<pattern name="method_combination">
<qualifiers>:before, :after, :around</qualifiers>
<example>
(defmethod greet :before ((p person))
  (format t "Preparing to greet...~%"))

(defmethod greet :around ((p person))
  (format t "[Start]~%")
  (call-next-method)
  (format t "[End]~%"))
</example>
</pattern>

<pattern name="multiple_inheritance">
<example>
(defclass employee (person job-holder)
  ((employee-id :initarg :id :accessor employee-id)))
</example>
<note>C3 linearization for method resolution order</note>
</pattern>
</clos>

<conditions>
<description>Common Lisp condition system - Restarts and handlers</description>

<pattern name="handler_case">
<description>Handle conditions similar to try-catch</description>
<example>
(handler-case
    (/ 1 0)
  (division-by-zero (c)
    (format t "Caught: ~a~%" c)
    0))
</example>
</pattern>

<pattern name="handler_bind">
<description>Handle conditions without unwinding stack</description>
<example>
(handler-bind
    ((error #'(lambda (c)
                (format t "Error occurred: ~a~%" c)
                (invoke-restart 'use-value 0))))
  (restart-case
      (error "Something went wrong")
    (use-value (v) v)))
</example>
</pattern>

<pattern name="restart_case">
<description>Define recovery points</description>
<example>
(defun parse-entry (entry)
  (restart-case
      (parse-integer entry)
    (use-value (v)
      :report "Use a different value"
      :interactive (lambda () (list (read)))
      v)
    (skip-entry ()
      :report "Skip this entry"
      nil)))
</example>
</pattern>

<pattern name="define_condition">
<example>
(define-condition invalid-input (error)
  ((value :initarg :value :reader invalid-input-value))
  (:report (lambda (c stream)
             (format stream "Invalid input: ~a"
                     (invalid-input-value c)))))
</example>
</pattern>
</conditions>

<packages>
<pattern name="defpackage">
<example>
(defpackage #:my-project
  (:use #:cl)
  (:import-from #:alexandria #:when-let #:if-let)
  (:export #:main
           #:process-data))
</example>
</pattern>

<pattern name="package_local_nicknames">
<example>
(defpackage #:my-project
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)))
</example>
</pattern>

<best_practices>
<practice>Use #: for uninterned symbols in defpackage</practice>
<practice>Export only the public API</practice>
<practice>Use package-local-nicknames for shorter references</practice>
<practice>Avoid :use except for :cl</practice>
</best_practices>
</packages>

<asdf>
<description>Another System Definition Facility - Build system for Common Lisp</description>

<pattern name="basic_defsystem">
<example>
(defsystem "my-project"
  :description "My project description"
  :version "0.1.0"
  :author "Author Name"
  :license "MIT"
  :depends-on ("alexandria" "cl-ppcre")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "main" :depends-on ("utils"))))
</example>
</pattern>

<pattern name="module_organization">
<example>
(defsystem "my-project"
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "core" :depends-on ("package"))))
   (:module "tests"
    :depends-on ("src")
    :components ((:file "test-suite")))))
</example>
</pattern>

<pattern name="package_inferred_system">
<description>Infer dependencies from defpackage forms</description>
<example>
(defsystem "my-project"
  :class :package-inferred-system
  :depends-on ("my-project/main"))

;; In my-project/main.lisp:
(defpackage #:my-project/main
  (:use #:cl)
  (:import-from #:my-project/utils #:helper))
</example>
</pattern>

<pattern name="test_system">
<example>
(defsystem "my-project/test"
  :depends-on ("my-project" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam '#:run!
               (uiop:find-symbol* '#:my-test-suite :my-project/test))))
</example>
</pattern>

<project_structure>
my-project/
├── my-project.asd
├── src/
│   ├── package.lisp
│   ├── utils.lisp
│   └── main.lisp
└── tests/
    └── test-suite.lisp
</project_structure>
</asdf>

<sbcl>
<description>Steel Bank Common Lisp - High-performance implementation</description>

<pattern name="save_executable">
<example>
(defun main ()
  (format t "Hello, World!~%")
  (sb-ext:exit :code 0))

(sb-ext:save-lisp-and-die "my-app"
  :toplevel #'main
  :executable t
  :compression t)
</example>
</pattern>

<pattern name="threading">
<example>
(defvar *result* nil)

(let ((thread (sb-thread:make-thread
                (lambda ()
                  (setf *result* (heavy-computation)))
                :name "worker")))
  (sb-thread:join-thread thread))

;; Mutex
(defvar *lock* (sb-thread:make-mutex))
(sb-thread:with-mutex (*lock*)
  (critical-section))
</example>
</pattern>

<pattern name="foreign_function">
<example>
(sb-alien:define-alien-routine "strlen" sb-alien:int
  (str sb-alien:c-string))

(strlen "hello") ; => 5
</example>
</pattern>

<optimization>
<example>
(defun fast-add (x y)
  (declare (type fixnum x y)
           (optimize (speed 3) (safety 0)))
  (the fixnum (+ x y)))
</example>
<declaration_options>type, ftype, inline, optimize</declaration_options>
</optimization>

<extensions>
<item name="sb-ext:*posix-argv*">Command-line arguments</item>
<item name="sb-ext:run-program">Execute external programs</item>
<item name="sb-ext:gc">Trigger garbage collection</item>
<item name="sb-posix">POSIX interface</item>
<item name="sb-bsd-sockets">Network sockets</item>
</extensions>
</sbcl>

<coalton>
<description>Statically typed functional programming on Common Lisp</description>

<pattern name="basic_types">
<example>
(coalton-toplevel
  (define-type (Maybe a)
    None
    (Some a))

  (declare safe-div (Integer -> Integer -> (Maybe Integer)))
  (define (safe-div x y)
    (if (== y 0)
        None
        (Some (/ x y)))))
</example>
</pattern>

<pattern name="type_classes">
<example>
(coalton-toplevel
  (define-class (Printable a)
    (print-it (a -> String)))

  (define-instance (Printable Integer)
    (define (print-it x)
      (into x))))
</example>
</pattern>

<integration>
<note>Coalton compiles to efficient Common Lisp code</note>
<note>Interoperable with regular CL code</note>
<note>Use coalton-toplevel for type-safe code sections</note>
</integration>
</coalton>

<context7_libraries>
<library name="Common Lisp Docs" id="/lisp-docs/lisp-docs.github.io" trust="4.7" snippets="580" />
<library name="ASDF" id="/websites/asdf_common-lisp_dev" trust="7.5" snippets="190" />
<library name="SBCL" id="/sbcl/sbcl" trust="8.0" snippets="86" />
<library name="CFFI" id="/websites/cffi_common-lisp_dev" trust="7.5" snippets="198" />
<library name="FiveAM" id="/websites/fiveam_common-lisp_dev" trust="7.5" snippets="164" />
<library name="Coalton" id="/coalton-lang/coalton" trust="6.6" snippets="568" />

<usage>
Use resolve-library-id then get-library-docs for latest documentation.
<example>
;; Get ASDF documentation
mcp__context7__get-library-docs
  context7CompatibleLibraryID="/websites/asdf_common-lisp_dev"
  topic="defsystem"
</example>
</usage>
</context7_libraries>

<common_patterns>
<pattern name="with_macro">
<description>Resource management with unwind-protect</description>
<example>
(defmacro with-open-socket ((var host port) &body body)
  `(let ((,var (make-socket ,host ,port)))
     (unwind-protect
          (progn ,@body)
       (close-socket ,var))))
</example>
</pattern>

<pattern name="loop_macro">
<example>
(loop for item in list
      for i from 0
      when (evenp i)
        collect item into evens
      finally (return evens))
</example>
</pattern>

<pattern name="format_directives">
<common_directives>
~a - aesthetic (princ)
~s - standard (prin1)
~d - decimal integer
~f - floating point
~% - newline
~{ ~} - iteration
~[ ~] - conditional
</common_directives>
</pattern>

<pattern name="documentation">
<example>
(defun my-function (arg)
  "Docstring describing the function.
ARG is the argument description."
  (process arg))
</example>
</pattern>
</common_patterns>

<best_practices>
<practice>Use *earmuffs* for special variables</practice>
<practice>Use +plus-signs+ for constants</practice>
<practice>Prefer functional style, minimize mutation</practice>
<practice>Use appropriate condition types, not just error</practice>
<practice>Provide restarts for recoverable situations</practice>
<practice>Document exported symbols</practice>
<practice>Use check-type for argument validation</practice>
<practice>Prefer ASDF package-inferred-system for new projects</practice>
</best_practices>

<anti_patterns>
<avoid name="global_state">Minimize global mutable state</avoid>
<avoid name="bare_use">Avoid :use except for :cl in defpackage</avoid>
<avoid name="ignore_conditions">Handle conditions appropriately</avoid>
<avoid name="deep_nesting">Refactor deeply nested code</avoid>
<avoid name="eval_usage">Avoid eval in application code</avoid>
<avoid name="read_macros_overuse">Use reader macros sparingly</avoid>
</anti_patterns>
