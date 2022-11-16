(import (chicken port)
	(chicken file)
	(chicken format)
        (chicken process-context))

(define (read-line p)
  "Reads and returns one line from port p"
  (define (read-line-intern p acc)
    (cond ((eof-object? (peek-char p))
	   (if (string=? "" acc) #f acc))
	  ((char=? (peek-char p) #\newline) (read-char p) acc)
	  ((char=? (peek-char p) #\tab) (read-char p)
	    (read-line-intern p (string-append acc (make-string 4 #\ ))))
	  (else (read-line-intern p (string-append acc (string (read-char p)))))))

  (read-line-intern p ""))

(define (get-lines src-a src-b acc)
  "Returns a list of tuples containing the lines of src-a and src-b. From EOF contains #f."
  (let ((line-a (read-line src-a))
	(line-b (read-line src-b)))
    (cond [(or line-a line-b)
	   (get-lines src-a src-b (cons (cons (if line-a line-a #f) (if line-b line-b #f)) acc))]
	  [else acc])))

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

(define (print-files path-a path-b)
  (define src-a (open-input-file file-a-path))
  (define src-b (open-input-file file-b-path))
  (define lines (reverse (get-lines src-a src-b '())))
  (define width (longest-line-a lines))

  (print-line "a" width)
  (print-line " |  b" width)
  (printf "~%~A~%" (make-string (+ 3 (longest-line lines)) #\-))
  (for-each (lambda (line)
	      (print-line (car line) width)
	      (cond [(not (and (car line) (cdr line))) (display " # ")]
		    [(string=? (car line) (cdr line)) (display " | ")]
		    [else (display " # ")])
	      (when (cdr line) (display (cdr line)))
	      (display #\newline))
	    lines)

  (close-input-port src-a)
  (close-input-port src-b))

;; ------------------------------ Main Script ------------------------------
;; -------------------------------------------------------------------------

(when (< (length (command-line-arguments)) 2)
  (printf "st: Show Two files next to each other~%~%Usage: st [FILEA] [FILEB]~%")
  (exit 1))

(define file-a-path (car (command-line-arguments)))
(define file-b-path (cadr (command-line-arguments)))

(unless (file-exists? file-a-path)
  (printf "Error: File ~S does could not be opened~%" file-a-path)
  (exit 1))

(unless (file-exists? file-b-path)
  (printf "Error: File ~S does could not be opened~%" file-b-path)
  (exit 1))

(print-files file-a-path file-b-path)
