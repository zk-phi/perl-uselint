;; ---- enivronment object

(defun uselint-environment-create ()
  "Allocate and return a new environment object, with a root scope pushed."
  (list 'uselint-environment (make-hash-table :test 'equal)))

(defun uselint-environment-push-scope (env)
  "Push another lexical scope to ENV."
  (setcdr env (cons (make-hash-table :test 'equal) (cdr env))))

(defun uselint-environment-add-declaration (env symbol &optional skip-unused-check)
  "Mark SYMBOL as declared in the innermost lexical scope of
ENV. If SKIP-UNUSED-CHECK is non-nil, reference count of the new
symbol will be 1 (otherwise 0). If the symbol is already
declared, raise a \"Duplicate declaration\" error."
  (unless (hash-table-p (cadr env))
    (error "Unexpected Error: Found declaration but all scopes are popped."))
  (when (gethash symbol (cadr env))
    (error "Duplicate declaration: %s" symbol))
  (puthash symbol (if skip-unused-check 1 0) (cadr env)))

(defun uselint-environment-refer-declaration (env symbol)
  "Increment reference count of SYMBOL in ENV. If SYMBOL is not
declared in ENV, raise a \"Declaration missing\" error."
  (unless (hash-table-p (cadr env))
    (error "Unexpected Error: Found declaration but all scopes are popped."))
  (let ((scopes (cdr env)) reference-count)
    (while (null reference-count)
      (unless scopes
        (error "Declaration missing: %s" symbol))
      (unless (setq reference-count (gethash symbol (car scopes)))
        (setq scopes (cdr scopes))))
    (puthash symbol (1+ reference-count) (car scopes))))

(defun uselint-environment-pop-scope (env)
  "Pop the innermost lexical scope of ENV. If some symbols
declared in the scope is unused, raise an \"Unused
declaration(s)\" error."
  (unless (hash-table-p (cadr env))
    (error "Unexpected Error: Scope is popped while no scopes are pushed."))
  (let (unused)
    (maphash (lambda (k v) (when (zerop v) (push k unused))) (cadr env))
    (when unused
      (error "Unused declaration(s): %s" (mapconcat 'identity unused ", "))))
  (setcdr env (cddr env)))

(defun uselint-environment-assert-all-popped (env)
  "Verify that all lexical scopes (including the root one) are
popped from ENV. Call this function at the end of the file."
  (unless (equal env '(uselint-environment))
    (error "Unexpected Error: Parse ended with unpopped scope(s).")))

;; ;; ---- parser combinators
;;
;; ;; A "parser" is a function, which may read characters by moving the
;; ;; cursor forward, or just fail without moving cursor.
;;
;; (defun $char (&optional ch)
;;   "Make a parser which reads a character. If CH is specified, the
;; character after the cursor must be CH (otherwise fails)."
;;   `(lambda ()
;;      (when (or (eobp) (and ,ch (not (= (char-after) ,ch))))
;;        (error "Parse Error: Failed to parse char `%c`" ,ch))
;;      (prog1 (char-after) (forward-char))))
;;
;; (defun $str (str)
;;   "Make a parser which reads an exact char sequence as STR."
;;   `(lambda ()
;;      (unless (looking-at ,(regexp-quote str))
;;        (error "Parse Error: Failed to parse string `%s`" ,str))
;;      (prog1 (match-string 0) (goto-char (match-end 0)))))
;;
;; (defun $regex (regex &optional subexp)
;;   "Make a parser which reads an char sequence matching REGEX. If
;; SUBEXP is specified, returned value will be a substring."
;;   `(lambda ()
;;      (unless (looking-at ,regex)
;;        (error "Parse Error: Failed to match regex `%s`" ,regex))
;;      (prog1 (match-string ,(or subexp 0)) (goto-char (match-end ,(or subexp 0))))))
;;
;; (defun $search (regex &optional subexp)
;;   "Make a parser which skips characters until an char sequence
;; matching REGEX is found. If SUBEXP is specified, returned value
;; will be a substring."
;;   `(lambda ()
;;      (unless (search-forward-regexp ,regex nil t)
;;        (error "Parse Error: Failed to search regex `%s`" ,regex))
;;      (prog1 (match-string ,(or subexp 0)) (goto-char (match-end ,(or subexp 0))))))
;;
;; (defun $not (parser)
;;   "Make a parser which succeeds iff PARSER fails. This parser
;; never consume any characters."
;;   `(lambda ()
;;      (let ((res (save-excursion (ignore-errors (cons (funcall ,parser) nil)))))
;;        (when res
;;          (error "Parse Error: Not expected to have `%s`" (car res))))))
;;
;; (defun $seq (&rest parsers)
;;   "Use PARSERS to parse characters in sequence, and return a list
;; of results. This parseer does not consume any characters if one
;; of the parsers fails."
;;   `(lambda ()
;;      (let ((initial-pos (point)))
;;        (condition-case e
;;            (mapcar 'funcall ',parsers)
;;          (error (progn
;;                   (goto-char initial-pos)
;;                   (error "%s" (error-message-string e))))))))
;;
;; (defun $maybe (parser)
;;   "Use PARSER and return the result if succeeded. Otherwise just
;; return nil."
;;   `(lambda ()
;;      (ignore-errors (funcall ,parser))))
;;
;; (defun $repeat (parser &optional at-least-once)
;;   "Use PARSER as many times as possible and return a list of
;; result. If AT-LEAST-ONCE is non-nil, PARSER must succeed at least
;; once. Otherwise, just return nil on failure."
;;   `(lambda ()
;;      (let (res)
;;        (condition-case e
;;            (while t (push (funcall ,parser) res))
;;          (error ,(if at-least-once
;;                     `(or (nreverse res) (error (error "%s" (error-message-string e))))
;;                   `(nreverse res)))))))
;;
;; (defun $> (parser fn)
;;   "Make a parser which first parses some characters with PARSER,
;; and if succeeded, passes the result to FN to obtain the return
;; value."
;;   `(lambda ()
;;      (let ((initial-pos (point))
;;            (res (funcall ,parser)))
;;        (condition-case e
;;            (funcall ,fn res)
;;          (error (progn
;;                   (goto-char initial-pos)
;;                   (error "Parse Error: Consumer function raised `%s`" (error-message-string e))))))))
;;
;; (defun $or (&rest parsers)
;;   "Try the first parser in PARSERS, and if the parser failed with
;; no characters consumed, try another parser in sequence. If all
;; parsers failed, fail. Otherwise return the result of a successful
;; parser."
;;   `(lambda ()
;;      (let ((initial-pos (point)) (parsers ,parsers) errors res)
;;        (while (and parsers (null res))
;;          (condition-case e
;;              (setq res (list (funcall (car parsers))))
;;            (error (if (not (= (point) initial-pos))
;;                       (error "%s" (error-message-string e))
;;                     (pop parsers)
;;                     (push (error-message-string e) errors)))))
;;        (or (car res)
;;            (error "Parse Error: No branches succeeded (%s)" (mapconcat 'identity errors ","))))))

;; ---- the linter

;; lint の対象にならないもの
;; - シングルクォート文字列、q()
;;   - ※qの後の記号はなんでも良い, 同じ記号または対応する閉じかっこで終了
;;   - ※記号じゃなくてもいいらしい。 `q XhogeX` とか。一つのトークンになっていなければ良い
;;   - ※たぶんかっこの対応を真面目に見る必要はない
;; - qw() リスト (use 文の import を除く)
;; - ダブルクォート文字列のシジルから始まらないトークン、qq()
;; - 数 (0e8 など e を含む場合に気をつけたほうが良さそう)
;; - ヒアドキュメント
;;   - <<hoge;, <<"hoge";, << "hoge"; など。 << hoge はビットシフト
;; - コメント ($# 以外の #)
;; - {<単一のシンボル>}, ->{単一のシンボル}, <単一のシンボル> =>
;;   - bareword なハッシュのキー

;; lint の対象になるもの＝それ以外のすべてのトークン
;; - 裸のトークン → 関数名
;; - A::B::C::D, A::B::C->D の形をした関数呼び出し
;;   - パッケージ内の関数
;;   - 変数になってるものに関しては、もう無理
;; - ->[], ->{}
;;   - arayref, hashref の deref
;; - シジル （または $# ... ） の後ろにシンボルと {} または []
;;   - 変数
;; - シジルの後ろに {}
;;   - 式
;;   - ※波かっこの対応に気をつける @{ map { $_->hoge } @lst } など
;;   - ※ブロックだと思って処理してしまっても問題なさそう
;; - ブロックというかスコープの構造はちゃんと追わなきゃいけないので、
;;   波かっこの対応だけは見ないといけなそう

;; EOF であたかも strict, utf8, warning use が必要かのように振る舞うの
;; が良さそう

;; どこまでがパッケージ名なんだ問題
;;
;; 例) hoge::fuga::piyo->hige
;;
;; 1. hoge がパッケージ名で、 fuga は別のパッケージ名を返すサブルーチ
;;    ン。返ってきたパッケージに piyo サブルーチンがあって、そいつが
;;    hige の生えたオブジェクトを返す
;;
;; 2. hoge::fuga がパッケージ名で、 piyo が hige の生えたオブジェクト
;;    を返すサブルーチン
;;
;; 3. hoge::fuga::piyo がパッケージ名で、 hige がそのクラスメソッド
;;
;; これはダイナミックに決まるっぽい。なんなら同時に hoge::fuga と
;; hoge::fuga::piyo があってもよい。
;;
;; sub AwesomePackage::hoge { return 1; }
;; package hoge::fuga { sub piyo { return "AwesomePackage"; } };
;; package hoge::fuga::piyo { sub Hogehoge { return 1; } };
;; print hoge::fuga::piyo->hoge; # hoge::fuga がパッケージ名、 hoge::fuga::piyo はサブルーチン
;; print hoge::fuga::piyo::Hogehoge; # hoge::fuga::piyo がパッケージ名
;;
;; "大文字始まりはパッケージ名、それ以外は縮めながらバックトラックでそ
;; れぞれ試す" がいいとこかなあ
;;
;; → どうやら、 ::<hoge>:: の形が出てきた場合、 "<hoge>()" にしない限
;; りはパッケージ名として解釈されるっぽい (test.pl)
;;
;; 一般に、 a::b::c::d->e の形が出てきた場合、パッケージ名は
;; 「a::b::c::d」か「a::b::c」のとちらか。 a::b::c::d の場合、パッケー
;; ジ名は 「a::b::c」確定

;; https://en.wikipedia.org/wiki/Perl_language_structure

(defvar uselint-current-environment nil)

(defconst uselint-builtin-symbols
  '("print" "say" "int" "defined" "undef"))

;; 気をつける
;; - immediate string (single quote, q)
;; - interpolating string (double quotes, qq)
;; - 0e8 (an interger may include "e")

(defun uselint--regex
    (concat
     ;; elements to skip EXPLICITLY
     ;; 1. comments (# not followed by $)
     "\\(#$\\|#[^$]\\)"
     ;; 2. singly quoted string
     "\\('\\|\\_<q\\_>\\)"
     ;; 3. doubly quoted string
     "\\(\"\\|\\_<qq\\>\\)"
     ;; 4. heredocs
     "<<"
     ;; 5. special filehandles
     "\\(__END__\\|__DATA__\\)"
     ))

(defun uselint--lint-forward ()
  (let))

(defun uselint--lint-dquot (end)
  )

;; (defun uselint--lint-forward ()
;;   (let ((regex (mapconcat 'identity
;;                           '(
;;                             ;; 1. block start
;;                             "\\({\\)"
;;                             ;; 2. block end
;;                             "\\(}\\)"
;;                             ;; *. use (3. packagename, 4. imports)
;;                             "use[\s\t]+\\([A-z0-9]+\\)\\([\s\t]+qw/\\([^/]+\\)/\\)?[\s\t]*;"
;;                             ;; hash の lookup の中身難しいな $hoge{fn} 式または bareword
;;                             ;; ignored components (string, key =>, comments, PODs, DATAs, heredocs, qw, shebang, q, qq, ...)
;;                             ;; bareword symbols (functions, constants, packages)
;;                             ;; variables ($@%) , pointers (/) , array length ($#) , ...
;;                             ;; keywords (sub, if, while, unless, return, shift, unshift, bless, ...)
;;                             ;; builtin variables (@_, $@, $_, $a, $b, ...)
;;                             )
;;                           "\\|")))
;;     (while (search-forward regex nil t)
;;       )))
