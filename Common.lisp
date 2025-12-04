;;;; Common.lisp
;;;; Common definitions shared between Tokenizer and Recognizer

;;; Token type enumeration
;;; Defines all possible token classes according to the lexical structure
(defconstant +token-types+
  '(:LEFT-PARENTHESIS
    :RIGHT-PARENTHESIS
    :LEFT-BRACKET
    :RIGHT-BRACKET
    :WHILE-KEYWORD
    :RETURN-KEYWORD
    :EQUAL
    :COMMA
    :EOL
    :VARTYPE
    :IDENTIFIER
    :BINOP
    :NUMBER))

;;; Lex structure - associates a lexeme string with its token class
(defstruct lex
  (token nil :type symbol)    ; Token type (keyword symbol)
  (lexeme "" :type string))   ; The actual lexeme string

;;; Reserved words mapping - tokens defined explicitly in lexical structure
;;; These must be checked before IDENTIFIER regex
(defconstant +reserved-words+
  '(("(" . :LEFT-PARENTHESIS)
    (")" . :RIGHT-PARENTHESIS)
    ("{" . :LEFT-BRACKET)
    ("}" . :RIGHT-BRACKET)
    ("while" . :WHILE-KEYWORD)
    ("return" . :RETURN-KEYWORD)
    ("=" . :EQUAL)
    ("," . :COMMA)
    (";" . :EOL)
    ("int" . :VARTYPE)
    ("void" . :VARTYPE)
    ("+" . :BINOP)
    ("*" . :BINOP)
    ("!=" . :BINOP)
    ("==" . :BINOP)
    ("%" . :BINOP)))

;;; Check if a character is whitespace
(defun whitespace-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return)))

;;; Check if a character is alphanumeric
(defun alphanumeric-p (ch)
  (or (alpha-char-p ch) (digit-char-p ch)))

;;; Check if a string contains only digits (for NUMBER token)
(defun all-digits-p (str)
  (and (> (length str) 0)
       (every #'digit-char-p str)))

;;; Check if a string is a valid identifier (starts with letter, contains letters/digits)
(defun valid-identifier-p (str)
  (and (> (length str) 0)
       (alpha-char-p (char str 0))
       (every #'alphanumeric-p str)))

;;; Convert token keyword to string format matching lexical structure
(defun token-to-string (token)
  (string-upcase (substitute #\_ #\- (symbol-name token))))
