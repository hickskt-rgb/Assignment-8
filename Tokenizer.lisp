;;;; Tokenizer.lisp
;;;; Reads input file, generates lexemes, and outputs tokens with lexemes

(load "Common.lisp")

;;; Main tokenizer function
(defun tokenize-file (input-file output-file)
  "Read input file, tokenize contents, write tokens to output file"
  (let* ((contents (read-file-contents input-file))
         (lexemes (generate-lexemes contents))
         (tokens (associate-tokens lexemes)))
    (write-tokens-to-file tokens output-file)))

;;; Read entire file contents into a string
(defun read-file-contents (filepath)
  "Read all characters from file into a single string"
  (with-open-file (stream filepath :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;;; Generate lexemes from input string
(defun generate-lexemes (input)
  "Convert input string into list of lexeme strings"
  (let ((lexemes '())
        (current-lexeme "")
        (i 0))
    (loop while (< i (length input)) do
      (let ((ch (char input i)))
        (cond
          ;; Whitespace - end current lexeme if any
          ((whitespace-p ch)
           (when (> (length current-lexeme) 0)
             (push current-lexeme lexemes)
             (setf current-lexeme ""))
           (incf i))
          
          ;; Alphanumeric character - add to current lexeme
          ((alphanumeric-p ch)
           (setf current-lexeme (concatenate 'string current-lexeme (string ch)))
           (incf i))
          
          ;; Symbol character - handle multi-character symbols
          (t
           ;; Save current alphanumeric lexeme if any
           (when (> (length current-lexeme) 0)
             (push current-lexeme lexemes)
             (setf current-lexeme ""))
           
           ;; Check for multi-character symbols (!=, ==)
           (let ((two-char (if (< (+ i 1) (length input))
                              (subseq input i (+ i 2))
                              nil)))
             (cond
               ;; Check for != operator
               ((and two-char (string= two-char "!="))
                (push "!=" lexemes)
                (incf i 2))
               
               ;; Check for == operator
               ((and two-char (string= two-char "=="))
                (push "==" lexemes)
                (incf i 2))
               
               ;; Single character symbol
               (t
                (push (string ch) lexemes)
                (incf i))))))))
    
    ;; Don't forget the last lexeme
    (when (> (length current-lexeme) 0)
      (push current-lexeme lexemes))
    
    ;; Reverse to get correct order (we pushed in reverse)
    (nreverse lexemes)))

;;; Associate each lexeme with its token class
(defun associate-tokens (lexemes)
  "Convert list of lexeme strings to list of lex structures"
  (mapcar #'classify-lexeme lexemes))

;;; Classify a single lexeme to determine its token type
(defun classify-lexeme (lexeme)
  "Determine token class for a given lexeme string"
  (let ((token-type nil))
    ;; Check reserved words first (in order defined in lexical structure)
    (dolist (pair +reserved-words+)
      (when (string= lexeme (car pair))
        (setf token-type (cdr pair))
        (return)))
    
    ;; If not a reserved word, check if it's a NUMBER
    (when (null token-type)
      (if (all-digits-p lexeme)
          (setf token-type :NUMBER)
          ;; Otherwise it must be an IDENTIFIER (assuming valid input)
          (setf token-type :IDENTIFIER)))
    
    ;; Create and return lex structure
    (make-lex :token token-type :lexeme lexeme)))

;;; Write tokens to output file
(defun write-tokens-to-file (tokens output-file)
  "Write each token and lexeme pair to output file"
  (with-open-file (stream output-file 
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (lex tokens)
      (format stream "~A ~A~%" 
              (token-to-string (lex-token lex))
              (lex-lexeme lex)))))

;;; Entry point - parse command line arguments and run tokenizer
(defun main ()
  "Main entry point for tokenizer program"
  (let ((args sb-ext:*posix-argv*))  ; For SBCL
    (when (< (length args) 3)
      (format *error-output* "Usage: ~A <input-file> <output-file>~%" (first args))
      (sb-ext:exit :code 1))
    
    (let ((input-file (second args))
          (output-file (third args)))
      (tokenize-file input-file output-file))))

;;; Run main function
(main)
