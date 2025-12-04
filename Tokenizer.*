

;;; ============================================================================
;;; TOKENIZER IMPLEMENTATION
;;; ============================================================================

;;; Check if a character is whitespace
(defun whitespace-p (ch)
  "Return T if character is whitespace."
  (member ch '(#\Space #\Tab #\Newline #\Return)))

;;; Check if a character is alphabetic
(defun alpha-p (ch)
  "Return T if character is alphabetic."
  (or (char<= #\a ch #\z)
      (char<= #\A ch #\Z)))

;;; Check if a character is numeric
(defun digit-p (ch)
  "Return T if character is a digit."
  (char<= #\0 ch #\9))

;;; Check if a character is alphanumeric
(defun alnum-p (ch)
  "Return T if character is alphanumeric."
  (or (alpha-p ch) (digit-p ch)))

;;; Check if a string contains only digits
(defun all-digits-p (str)
  "Return T if string contains only numeric characters."
  (and (> (length str) 0)
       (every #'digit-p str)))

;;; Read entire file into a string
(defun read-file-to-string (filepath)
  "Read the entire contents of a file into a string."
  (with-open-file (stream filepath :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;;; Tokenize the input text into lexemes
(defun tokenize-text (text)
  "Convert input text into a list of lexeme strings."
  (let ((lexemes '())
        (current-lexeme "")
        (i 0)
        (len (length text)))
    
    (loop while (< i len) do
      (let ((ch (char text i)))
        (cond
          ;; Whitespace: end current lexeme if any
          ((whitespace-p ch)
           (when (> (length current-lexeme) 0)
             (push current-lexeme lexemes)
             (setf current-lexeme ""))
           (incf i))
          
          ;; Alphanumeric: build alphanumeric lexeme
          ((alnum-p ch)
           ;; If we were building a different type, save it
           (when (and (> (length current-lexeme) 0)
                      (not (alnum-p (char current-lexeme 0))))
             (push current-lexeme lexemes)
             (setf current-lexeme ""))
           (setf current-lexeme (concatenate 'string current-lexeme (string ch)))
           (incf i))
          
          ;; Symbol characters
          (t
           ;; End any alphanumeric lexeme
           (when (> (length current-lexeme) 0)
             (push current-lexeme lexemes)
             (setf current-lexeme ""))
           
           ;; Check for two-character symbols
           (let ((two-char (if (< (+ i 1) len)
                               (concatenate 'string (string ch) (string (char text (+ i 1))))
                               nil)))
             (cond
               ;; Check for != or ==
               ((and two-char (or (string= two-char "!=")
                                  (string= two-char "==")))
                (push two-char lexemes)
                (incf i 2))
               
               ;; Single character symbol
               (t
                (push (string ch) lexemes)
                (incf i))))))))
    
    ;; Don't forget the last lexeme
    (when (> (length current-lexeme) 0)
      (push current-lexeme lexemes))
    
    ;; Reverse because we built the list backwards
    (nreverse lexemes)))

;;; Associate a lexeme with its token type
(defun classify-lexeme (lexeme)
  "Determine the token type for a given lexeme string."
  (cond
    ;; Single-character symbols (check in order)
    ((string= lexeme "(") :LEFT-PARENTHESIS)
    ((string= lexeme ")") :RIGHT-PARENTHESIS)
    ((string= lexeme "{") :LEFT-BRACKET)
    ((string= lexeme "}") :RIGHT-BRACKET)
    
    ;; Keywords
    ((string= lexeme "while") :WHILE-KEYWORD)
    ((string= lexeme "return") :RETURN-KEYWORD)
    
    ;; Other single-character symbols
    ((string= lexeme "=") :EQUAL)
    ((string= lexeme ",") :COMMA)
    ((string= lexeme ";") :EOL)
    
    ;; VARTYPE
    ((or (string= lexeme "int") (string= lexeme "void")) :VARTYPE)
    
    ;; BINOP
    ((or (string= lexeme "+")
         (string= lexeme "*")
         (string= lexeme "!=")
         (string= lexeme "==")
         (string= lexeme "%"))
     :BINOP)
    
    ;; NUMBER (all digits)
    ((all-digits-p lexeme) :NUMBER)
    
    ;; Default to IDENTIFIER (matches [a-zA-Z][a-zA-Z0-9]*)
    (t :IDENTIFIER)))

;;; Convert lexemes to lex structures
(defun lexemes-to-lex-list (lexemes)
  "Convert a list of lexeme strings to a list of lex structures."
  (mapcar (lambda (lexeme)
            (make-lex :token-type (classify-lexeme lexeme)
                      :lexeme lexeme))
          lexemes))

;;; Write lex structures to output file
(defun write-tokens (lex-list filepath)
  "Write token-lexeme pairs to output file."
  (with-open-file (stream filepath :direction :output :if-exists :supersede)
    (dolist (lex lex-list)
      (format stream "~A ~A~%"
              (token-type-to-string (lex-token-type lex))
              (lex-lexeme lex)))))

;;; Main tokenizer function
(defun tokenize (input-file output-file)
  "Main tokenizer: read input file, tokenize, write to output file."
  (let* ((text (read-file-to-string input-file))
         (lexemes (tokenize-text text))
         (lex-list (lexemes-to-lex-list lexemes)))
    (write-tokens lex-list output-file)))

