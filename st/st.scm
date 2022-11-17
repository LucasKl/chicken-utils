(import (chicken port)
	(chicken file)
	(chicken file posix)
	(chicken format)
        (chicken process-context)
	(srfi 69))

(define (read-line p w)
  "Reads and returns one line from port p"
  (define (read-line-intern p acc w-count)
    (cond [(eof-object? (peek-char p)) (if (string=? "" acc) #f acc)]
	  [(char=? (peek-char p) #\newline) (read-char p) acc]
	  [(and (not (= w -1)) (> w-count w)) (read-char p) (read-line-intern p acc (+ w-count 1))]
	  [(char=? (peek-char p) #\tab) (read-char p)
	   (read-line-intern p (string-append acc (make-string 4 #\ )) (+ w-count 1))]
	  [else (read-line-intern p (string-append acc (string (read-char p))) (+ w-count 1))]))

  (read-line-intern p "" 0))

(define (get-lines src-a src-b acc n w)
  "Returns a list of tuples containing the lines of src-a and src-b. From EOF contains #f."
  (let ((line-a (read-line src-a w))
	(line-b (read-line src-b w)))
    (if (and (not (= n 0)) (or line-a line-b))
	(get-lines src-a src-b (cons (cons (if line-a line-a #f) (if line-b line-b #f)) acc) (- n 1) w)
	acc)))

(define (longest-line-a lines)
  "Returns the length of the longest line in src-a"
  (define (longest-line-a-intern lines max-length)
    (if (not (eq? lines '()))
	(let* ((line (car lines))
	       (a-length (string-length (if (car line) (car line) ""))))
	  (longest-line-a-intern (cdr lines) (max a-length max-length)))
	max-length))  
  (+ (longest-line-a-intern lines 0) 2))

(define (longest-line lines)
  "Returns the length of the overall longest line"
  (define (longest-line-intern lines max-length)
    (if (not (eq? lines '()))
	(let* ((line (car lines))
	       (a-length (string-length (if (car line) (car line) "")))
	       (b-length (string-length (if (cdr line) (cdr line) ""))))
	  (longest-line-intern (cdr lines) (max (+ a-length b-length) max-length)))
	max-length))  
  (+ (longest-line-intern lines 0) 4))

(define (print-line line width)
  "Prints a padded line"
  (cond [line (printf "~A~A" line (make-string (- width (string-length line)) #\ ))]
	[else (display (make-string width #\ ))]))

;;(define
 (define-syntax number-or-else
   (syntax-rules ()
     ((_ table key else)
      (let ((v (hash-table-ref/default table key else)))
	(if (symbol? v) v (string->number v))))))

(define (print-files src-a src-b options)
  (define line-count (string->number (hash-table-ref/default options "-n" "-1")))
  (define column-count (string->number (hash-table-ref/default options "-w" "-1")))
  (define show-diff (or (hash-table-exists? options "-d")
			(hash-table-exists? options "--diff")))
  (define show-header (not (or (hash-table-exists? options "-p")
			       (hash-table-exists? options "--plain"))))
  
  (define lines (reverse (get-lines src-a src-b '() line-count column-count)))
  (define width (longest-line-a (cons (cons file-a-path file-b-path) lines)))

  (when (and (not (eq? lines '())) show-header)
    (print-line "a" width)
    (print-line " |  b" width)
    (printf "~%~A~%" (make-string (+ 3 (longest-line lines)) #\-)))
  (for-each (lambda (line)
	      (print-line (car line) width)
	      (if show-diff
		  (cond [(not (and (car line) (cdr line))) (display " # ")]
			[(string=? (car line) (cdr line)) (display " | ")]
			[else (display " # ")])
		  (display " | "))
	      (when (cdr line) (display (cdr line)))
	      (display #\newline))
	    lines))

(define (parse-args args)
  "Parse the command line arguments. Everything after a flag starting with
is set as the value of abc, except when it is also a flag."
  (define (next-arg args options arguments)
    (cond [(eq? args '()) (cons (alist->hash-table options) (reverse arguments))]
	  [else (let ((arg (car args))
		      (next (if (not (eq? (cdr args) '())) (cadr args) '())))
		  (if (string=? "-" (substring arg 0 1))
		      (if (and (not (eq? next '()))
			       (not (string=? "-" (substring next 0 1))))
			  (next-arg (cddr args) (cons (cons arg next) options) arguments)
			  (next-arg (cdr args) (cons (cons arg #t) options) arguments))
		      (next-arg (cdr args) options (cons arg arguments))))]))

  (next-arg args '() '()))

;; ------------------------------ Main Script ------------------------------
;; -------------------------------------------------------------------------

(define parsed-args (parse-args (cdr (argv))))
(define options (car parsed-args))
(define arguments (cdr parsed-args))
(define nargs (length arguments))

(when (or (hash-table-ref/default options "-h" #f)
	  (hash-table-ref/default options "--help" #f)
	  (< nargs 1))
  (printf "Usage: st [OPTIONS] [FILEA] [FILEB]~%")
  (printf "st: Show Two files next to each other~%~%")
  (printf "Options: ~%")
  (printf "  -h, --help        print this help text~%")
  (printf "  -n,               print only n lines~%")
  (printf "  -w,               print only w columns~%")
  (printf "  -d, --diff        highlight differences~%")
  (printf "  -p, --plain       dont print header~%")
  (exit 1))

(define file-a-path (car arguments))
(define file-b-path (if (> (length arguments) 1) (cadr arguments) #f))

;; Open first file. At least one file has to be specified as an argument
(define src-a (cond [(file-exists? file-a-path) (open-input-file file-a-path)]
		    [else (printf "Error: File ~S does could not be opened~%" file-a-path)
			  (exit 1)]))

;; Open second file. If no second argument was passed read from stdin
(define src-b (cond [(not file-b-path) (open-input-file* fileno/stdin)]
		    [(file-exists? file-b-path) (open-input-file file-b-path)]
		    [else (printf "Error: File ~S does could not be opened~%" file-b-path)
			  (exit 1)]))

(print-files src-a src-b options)
(close-input-port src-a)
(close-input-port src-b)
