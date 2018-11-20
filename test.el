(load-file "./putica-uselint.el")

;; ---- utilities

(defmacro test-group (name &rest exprs)
  (declare (indent 1))
  `(progn (message "\n---- Test: %s" ,name) ,@exprs))

(defmacro test (name a b)
  (declare (indent 1))
  `(let (a b)
     (condition-case e
         (setq a ,a b ,b)
       (error (error "[NG] %s (got an error `%s`)" ,name e)))
     (if (equal a b)
         (message "[OK] %s" ,name)
       (error "[NG] %s (expected: `%s`, got: `%s`)" ,name b a))))

(defmacro test-error (name a &optional message)
  (declare (indent 1))
  `(let (res err)
     (condition-case e (setq res ,a) (error (setq err (error-message-string e))))
     (cond ((null err)
            (error "[NG] %s (expected to fail but got `%s`)" ,name res))
           ((and ,message (not (string= ,message err)))
            (error "[NG] %s (got an error `%s`)" ,name err))
           (t
            (message "[OK] %s" ,name)))))

(defmacro test-success (name a)
  (declare (indent 1))
  `(condition-case e
       (progn ,a (message "[OK] %s" ,name))
     (error (error "[NG] %s (got an error `%s`)" ,name e))))

;; ---- environment object

(test-group "Environment object"
  (let ((env (uselint-environment-create)))
    ;; global
    (test-success "Add an imported subroutine"
      (uselint-environment-add-declaration env "now"))
    (test-success "Add a package name as \"already-referred\" (since a subroutine is imported)"
      (uselint-environment-add-declaration env "DateTime" t))
    (test-success "Add global variable $hoge"
      (uselint-environment-add-declaration env "$hoge"))
    (test "Refer global variable $hoge"
      (uselint-environment-refer-declaration env "$hoge") 1)
    (test "Refer global variable $hoge again (reference count incremented)"
      (uselint-environment-refer-declaration env "$hoge") 2)
    (test "Refer global variable DateTime (count as \"already referred\")"
      (uselint-environment-refer-declaration env "DateTime") 2)
    (test-error "Cannot refer undefined variable $fuga"
      (uselint-environment-refer-declaration env "$fuga")
      "Declaration missing: $fuga")
    ;; local
    (test-success "Push a new scope"
      (uselint-environment-push-scope env))
    (test "Refer global definition of $hoge"
      (uselint-environment-refer-declaration env "$hoge") 3)
    (test-success "Shadow the global definition of $hoge"
      (uselint-environment-add-declaration env "$hoge"))
    (test-success "Add a local variable $fuga"
      (uselint-environment-add-declaration env "$fuga"))
    (test "Refer the local definition of $hoge"
      (uselint-environment-refer-declaration env "$hoge") 1)
    (test-error "Cannot pop a scope (unused variable exists)"
      (uselint-environment-pop-scope env)
      "Unused declaration(s): $fuga")
    (test "Refer local variable $fuga"
      (uselint-environment-refer-declaration env "$fuga") 1)
    (test-success "Pop a scope (all variables are used now)"
      (uselint-environment-pop-scope env))
    ;; global
    (test "Refer global definition of $hoge (local definition is popped)"
      (uselint-environment-refer-declaration env "$hoge") 4)
    (test-error "Cannot refer local variable $fuga in the popped scope"
      (uselint-environment-refer-declaration env "$fuga")
      "Declaration missing: $fuga")
    (test-error "Cannot pop a scope (an imported subroutine is unused)"
      (uselint-environment-pop-scope env)
      "Unused declaration(s): now")
    (test "Refer imported subroutine"
      (uselint-environment-refer-declaration env "now") 1)
    (test-error "Assertion \"all scopes are popped\" fails (since the global scope still lives)"
      (uselint-environment-assert-all-popped env)
      "Unexpected Error: Parse ended with unpopped scope(s).")
    (test-success "Pop the global scope"
      (uselint-environment-pop-scope env))
    (test-success "Assertion \"all scopes are popped\" succeeds"
      (uselint-environment-assert-all-popped env))))

---- parser

(test-group "Char parser"
  (with-temp-buffer
    (save-excursion (insert "ab"))
    (test "Parse succeed"
      (funcall ($char ?a)) ?a)
    (test-error "Parse fail"
      (funcall ($char ?a))
      "Parse Error: Failed to parse char `a`")
    (test "Input is consumed"
      (funcall ($char ?b)) ?b)))

(test-group "String parser"
  (with-temp-buffer
    (save-excursion (insert "abcd"))
    (test "Parse succeed"
      (funcall ($str "ab")) "ab")
    (test-error "Parse fail"
      (funcall ($str "ab"))
      "Parse Error: Failed to parse string `ab`")
    (test "Input is consumed"
      (funcall ($str "cd")) "cd")))

(test-group "Regex parser"
  (with-temp-buffer
    (save-excursion (insert "abcd"))
    (test "Parse succeed"
      (funcall ($regex "ab\\(.\\)" 1)) "c")
    (test-error "Parse fail"
      (funcall ($regex "c"))
      "Parse Error: Failed to match regex `c`")
    (test "Input is consumed"
      (funcall ($regex "d")) "d")))

(test-group "Search Parser"
  (with-temp-buffer
    (save-excursion (insert "abcdefg"))
    (test "Parse succeed"
      (funcall ($search "cd\\(.\\)" 1)) "e")
    (test-error "Parse fail"
      (funcall ($search "ab"))
      "Parse Error: Failed to search regex `ab`")
    (test "Input is consumed"
      (funcall ($search ".*$")) "fg")))

(test-group "Not higher-order parser"
  (with-temp-buffer
    (save-excursion (insert "ab"))
    (test-error "Parse fail"
      (funcall ($not ($char ?a)))
      "Parse Error: Not expected to have `97`")
    (test "Parse succeed"
      (funcall ($not ($char ?b))) nil)
    (test "Input is not consumed"
      (funcall ($char ?a)) ?a)))

(test-group "Seq higher-order parser"
  (with-temp-buffer
    (save-excursion (insert "abcdefg"))
    (test "Parse succeed"
      (funcall ($seq ($search "bc") ($char ?d))) '("bc" ?d))
    (test-error "Parse fail"
      (funcall ($seq ($char ?e) ($char ?g)))
      "Parse Error: Failed to parse char `g`")
    (test "Input is consumed"
      (funcall ($seq ($char ?e) ($char ?f))) '(?e ?f))))

(test-group "Maybe higher-order parser"
  (with-temp-buffer
    (save-excursion (insert "abc"))
    (test "Parse succeed"
      (funcall ($maybe ($char ?a))) ?a)
    (test "Parse succeed with a 0-width match"
      (funcall ($maybe ($char ?a))) nil)
    (test "Input is consumed"
      (funcall ($maybe ($char ?b))) ?b)))

(test-group "Repeat higher-order parser"
  (with-temp-buffer
    (save-excursion (insert "abababcdef"))
    (test "Parse succeed"
      (funcall ($repeat ($str "ab") t)) '("ab" "ab" "ab"))
    (test-error "Parse fail with a 0-width match"
      (funcall ($repeat ($str "ab") t))
      "Parse Error: Failed to parse string `ab`")
    (test "Parse succeed with a 0-width match"
      (funcall ($repeat ($str "ab"))) '())
    (test "Input is consumed"
      (funcall ($repeat ($str "cdef"))) '("cdef"))))

(test-group "Funcall higher-order parser"
  (with-temp-buffer
    (save-excursion (insert "abcdefg"))
    (test "Parse succeed"
      (funcall ($> ($char ?a) (lambda (x) (+ x 1)))) ?b)
    (test-error "Parse fail"
      (funcall ($> ($char ?b) (lambda (x) (error "hoge"))))
      "Parse Error: Consumer function raised `hoge`")
    (test "Input is consumed"
      (funcall ($char ?b)) ?b)))
