---
name: common-lisp-ecosystem
description: "Use when working with Common Lisp, 'CLOS', 'ASDF', 'defpackage', 'defsystem', 'SBCL', 'Coalton', 'Roswell', or 'Qlot'. Provides Common Lisp ecosystem patterns including CLOS multiple dispatch, condition/restart system, ASDF build configuration, and SBCL-specific features."
---

Comprehensive patterns for Common Lisp development: CLOS object system, condition/restart error handling, ASDF system definitions, SBCL-specific features, and Coalton static typing.

## Core Concepts

- **S-expressions**: Homoiconic syntax enabling powerful macro systems -- code and data share the same structure
- **CLOS**: Generic functions with multiple dispatch; method combination (`:before`, `:after`, `:around`)
- **Conditions**: `handler-case` for catching, `restart-case` for recovery -- more powerful than exceptions
- **Packages**: Namespace management with `defpackage`; prefer `:import-from` or local-nicknames over bare `:use`

## CLOS (Common Lisp Object System)

### Class Definition

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age))
  (:documentation "Represents a person."))
```

Slot options: `:initarg`, `:initform`, `:accessor`, `:reader`, `:writer`, `:type`, `:documentation`.

### Generic Functions and Methods

Use `defgeneric`/`defmethod` when polymorphic behavior based on multiple types is needed:

```lisp
(defgeneric greet (entity)
  (:documentation "Greet an entity."))

(defmethod greet ((p person))
  (format t "Hello, ~a!~%" (person-name p)))

;; Method combination for aspect-oriented programming
(defmethod greet :before ((p person))
  (format t "Preparing to greet...~%"))

(defmethod greet :around ((p person))
  (format t "[Start]~%")
  (call-next-method)
  (format t "[End]~%"))
```

Multiple inheritance uses C3 linearization for method resolution order.

## Condition System

### handler-case (Catching)

```lisp
(handler-case
    (/ 1 0)
  (division-by-zero (c)
    (format t "Caught: ~a~%" c)
    0))
```

### restart-case (Recovery)

The agent should provide restarts when errors can be recovered interactively or programmatically:

```lisp
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
```

### Custom Conditions

```lisp
(define-condition invalid-input (error)
  ((value :initarg :value :reader invalid-input-value))
  (:report (lambda (c stream)
             (format stream "Invalid input: ~a"
                     (invalid-input-value c)))))
```

## Packages

```lisp
(defpackage #:my-project
  (:use #:cl)
  (:import-from #:alexandria #:when-let #:if-let)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum))
  (:export #:main #:process-data))
```

## ASDF System Definition

### Basic System

```lisp
(defsystem "my-project"
  :description "My project description"
  :version "0.1.0"
  :author "Author Name"
  :license "MIT"
  :depends-on ("alexandria" "cl-ppcre")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "main" :depends-on ("utils"))))
```

### Package-Inferred System (Modern)

Use for automatic dependency inference from package definitions:

```lisp
(defsystem "my-project"
  :class :package-inferred-system
  :depends-on ("my-project/main"))

;; In my-project/main.lisp:
(defpackage #:my-project/main
  (:use #:cl)
  (:import-from #:my-project/utils #:helper))
```

### Test System

```lisp
(defsystem "my-project/test"
  :depends-on ("my-project" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam '#:run!
               (uiop:find-symbol* '#:my-test-suite :my-project/test))))
```

### Recommended Project Structure

```
my-project/
├── my-project.asd
├── src/
│   ├── package.lisp
│   ├── utils.lisp
│   └── main.lisp
└── tests/
    └── test-suite.lisp
```

## SBCL-Specific Features

### Standalone Executable

```lisp
(sb-ext:save-lisp-and-die "my-app"
  :toplevel #'main
  :executable t
  :compression t)
```

### Threading

```lisp
(let ((thread (sb-thread:make-thread
                (lambda () (heavy-computation))
                :name "worker")))
  (sb-thread:join-thread thread))

(defvar *lock* (sb-thread:make-mutex))
(sb-thread:with-mutex (*lock*)
  (critical-section))
```

### Performance Optimization

```lisp
(defun fast-add (x y)
  (declare (type fixnum x y)
           (optimize (speed 3) (safety 0)))
  (the fixnum (+ x y)))
```

### Extensions

`sb-ext:*posix-argv*` for CLI args, `sb-ext:run-program` for external processes, `sb-ext:gc` for garbage collection, `sb-posix` for POSIX interface, `sb-bsd-sockets` for networking.

## Coalton (Static Typing)

```lisp
(coalton-toplevel
  (define-type (Maybe a)
    None
    (Some a))

  (declare safe-div (Integer -> Integer -> (Maybe Integer)))
  (define (safe-div x y)
    (if (== y 0) None (Some (/ x y)))))
```

Coalton provides Hindley-Milner type inference with type classes, compiles to efficient CL code, and is fully interoperable with regular Common Lisp.

## Common Patterns

### Resource Management

```lisp
(defmacro with-open-socket ((var host port) &body body)
  `(let ((,var (make-socket ,host ,port)))
     (unwind-protect
         (progn ,@body)
       (close-socket ,var))))
```

### Format Directives

`~a` (aesthetic), `~s` (standard), `~d` (decimal), `~f` (float), `~%` (newline), `~{~}` (iteration), `~[~]` (conditional).

## Modern Tooling

- **Qlot**: Per-project dependency manager (`qlot install`, `qlot exec ros run`)
- **Roswell**: Lisp implementation manager (`ros install sbcl`, `ros build myapp.ros`)

## Anti-Patterns to Avoid

- **Global mutable state** -- pass state explicitly or use closures
- **Bare `:use`** -- use `:import-from` or package-local-nicknames instead
- **Ignoring conditions** -- handle with `handler-case`/`handler-bind` and provide restarts
- **Deep nesting** -- extract helper functions, use early returns
- **`eval` in application code** -- use macros or first-class functions instead
- **Overusing reader macros** -- makes code harder to read; document when necessary

## Workflow

1. **Analyze**: Check ASDF system definition, review macros and patterns, identify CLOS hierarchies
2. **Implement**: Use appropriate abstraction level, follow condition system, design macros carefully
3. **Validate**: Load system with ASDF, run tests, check for compilation warnings

## Best Practices

- Use `*earmuffs*` for special variables, `+plus-signs+` for constants
- Prefer functional style, minimize mutation
- Provide restarts for recoverable situations
- Document exported symbols with docstrings
- Use `check-type` for argument validation
- Prefer ASDF `package-inferred-system` for new projects
- Use ASDF for all system definitions; never load files directly
