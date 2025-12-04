

;;; ============================================================================
;;; RECOGNIZER IMPLEMENTATION
;;; ============================================================================

;;; Global variables for parser state
(defparameter *tokens* nil)        ; List of all tokens
(defparameter *current-index* 0)   ; Current position in token list
(defparameter *output-stream* nil) ; Output file stream for error messages

;;; Get the current token
(defun current-token ()
  "Return the current token, or NIL if past end."
  (if (< *current-index* (length *tokens*))
      (nth *current-index* *tokens*)
      nil))

;;; Get the token type of current token
(defun current-token-type ()
  "Return the token type of the current token, or NIL."
  (let ((tok (current-token)))
    (if tok (lex-token-type tok) nil)))

;;; Consume (advance to next token)
(defun consume ()
  "Move to the next token."
  (incf *current-index*))

;;; Check if current token matches expected type
(defun match (expected-type)
  "Return T if current token type matches expected type."
  (eq (current-token-type) expected-type))

;;; Error reporting and termination
(defun report-error (message)
  "Write error message to output file and exit."
  (format *output-stream* "~A~%" message)
  (close *output-stream*)
  (exit :code 0))

;;; Expect a specific token type
(defun expect (token-type grammar-rule)
  "Consume current token if it matches expected type, otherwise error."
  (if (match token-type)
      (consume)
      (report-error
       (format nil "Error: In grammar rule ~A, expected token #~A to be ~A but was ~A"
               grammar-rule
               (+ *current-index* 1)
               (token-type-to-string token-type)
               (if (current-token)
                   (token-type-to-string (current-token-type))
                   "EOF")))))

;;; Read tokens from file
(defun read-tokens-from-file (filepath)
  "Read token-lexeme pairs from file and return list of lex structures."
  (with-open-file (stream filepath :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-trim '(#\Space #\Tab) line)
          when (> (length trimmed) 0)
          collect (let* ((space-pos (position #\Space trimmed))
                         (token-str (subseq trimmed 0 space-pos))
                         (lexeme (subseq trimmed (+ space-pos 1))))
                    (make-lex :token-type (string-to-token-type token-str)
                              :lexeme lexeme)))))

;;; Grammar rule: term --> IDENTIFIER | NUMBER
(defun parse-term ()
  "Parse a term non-terminal."
  (cond
    ((match :IDENTIFIER) (consume))
    ((match :NUMBER) (consume))
    (t (report-error
        (format nil "Error: In grammar rule term, expected a valid term non-terminal to be present but was not.")))))

;;; Grammar rule: expression --> term {BINOP term} | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
(defun parse-expression ()
  "Parse an expression non-terminal."
  (cond
    ;; LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
    ((match :LEFT-PARENTHESIS)
     (consume)
     (parse-expression)
     (expect :RIGHT-PARENTHESIS "expression"))
    
    ;; term {BINOP term}
    (t
     (parse-term)
     ;; Handle zero or more {BINOP term}
     (loop while (match :BINOP)
           do (consume)
              (parse-term)))))

;;; Grammar rule: assignment --> IDENTIFIER EQUAL expression EOL
(defun parse-assignment ()
  "Parse an assignment statement."
  (expect :IDENTIFIER "assignment")
  (expect :EQUAL "assignment")
  (parse-expression)
  (expect :EOL "assignment"))

;;; Grammar rule: return --> RETURN_KEYWORD expression EOL
(defun parse-return ()
  "Parse a return statement."
  (expect :RETURN-KEYWORD "return")
  (parse-expression)
  (expect :EOL "return"))

;;; Grammar rule: while-loop --> WHILE_KEYWORD LEFT_PARENTHESIS expression RIGHT_PARENTHESIS body
(defun parse-while-loop ()
  "Parse a while loop statement."
  (expect :WHILE-KEYWORD "while-loop")
  (expect :LEFT-PARENTHESIS "while-loop")
  (parse-expression)
  (expect :RIGHT-PARENTHESIS "while-loop")
  (parse-body))

;;; Grammar rule: statement --> while-loop | return | assignment
(defun parse-statement ()
  "Parse a statement non-terminal."
  (cond
    ((match :WHILE-KEYWORD) (parse-while-loop))
    ((match :RETURN-KEYWORD) (parse-return))
    ((match :IDENTIFIER) (parse-assignment))
    (t (report-error
        (format nil "Error: In grammar rule statement, expected a valid statement non-terminal to be present but was not.")))))

;;; Grammar rule: statement-list --> statement {statement}
(defun parse-statement-list ()
  "Parse a statement-list non-terminal."
  (parse-statement)
  ;; Handle zero or more additional statements
  (loop while (or (match :WHILE-KEYWORD)
                  (match :RETURN-KEYWORD)
                  (match :IDENTIFIER))
        do (parse-statement)))

;;; Grammar rule: body --> LEFT_BRACKET [statement-list] RIGHT_BRACKET
(defun parse-body ()
  "Parse a body non-terminal."
  (expect :LEFT-BRACKET "body")
  ;; Optional statement-list
  (when (or (match :WHILE-KEYWORD)
            (match :RETURN-KEYWORD)
            (match :IDENTIFIER))
    (parse-statement-list))
  (expect :RIGHT-BRACKET "body"))

;;; Grammar rule: arg-decl --> VARTYPE IDENTIFIER {COMMA VARTYPE IDENTIFIER}
(defun parse-arg-decl ()
  "Parse an arg-decl non-terminal."
  (expect :VARTYPE "arg-decl")
  (expect :IDENTIFIER "arg-decl")
  ;; Handle zero or more {COMMA VARTYPE IDENTIFIER}
  (loop while (match :COMMA)
        do (consume)
           (expect :VARTYPE "arg-decl")
           (expect :IDENTIFIER "arg-decl")))

;;; Grammar rule: header --> VARTYPE IDENTIFIER LEFT_PARENTHESIS [arg-decl] RIGHT_PARENTHESIS
(defun parse-header ()
  "Parse a header non-terminal."
  (expect :VARTYPE "header")
  (expect :IDENTIFIER "header")
  (expect :LEFT-PARENTHESIS "header")
  ;; Optional arg-decl
  (when (match :VARTYPE)
    (parse-arg-decl))
  (expect :RIGHT-PARENTHESIS "header"))

;;; Grammar rule: function --> header body
(defun parse-function ()
  "Parse a function (top-level grammar rule)."
  (parse-header)
  (parse-body))

;;; Main recognizer function
(defun recognize (input-file output-file)
  "Main recognizer: read tokens, parse, write result."
  (setf *tokens* (read-tokens-from-file input-file))
  (setf *current-index* 0)
  
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (setf *output-stream* stream)
    
    ;; Parse the top-level rule
    (parse-function)
    
    ;; Check if all tokens were consumed
    (if (< *current-index* (length *tokens*))
        (report-error
         (format nil "Error: Only consumed ~A of the ~A given tokens"
                 *current-index*
                 (length *tokens*)))
        ;; Success!
        (format stream "PARSED!!!~%"))))

