;Make X!Y
(define (make-variable x y)
	(if (equal? x y) x
		(string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
	)
)

(define (make-if x y)
	(cons 'if (cons '% (cons x (cons y '()))))
)

;Replace x by its X!Y form
(define (make-pair-x x a b)
	(if (memq x a) (let ((z (list-ref b (index-of a x)))) (make-variable x z))
		x	
	)
)

;Same for y
(define (make-pair-y y a b)
    (if (memq y b) (let ((z (list-ref a (index-of b y)))) (make-variable z y))
        y
    )
)

;Compare two constant literals, or make X!Y
(define (const-cmp x y a b)
	(if (and (equal? x #t) (equal? y #f)) '% 
		(if (and (equal? x #f) (equal? y #t)) '(not %)
			(let ((c (make-pair-x x a b)) (d (make-pair-y y a b))) 
				(if (equal? c d) c
					(make-if c d)
				)
			)
		)
	)
)			

;Compare two lists (same length) where each element should be some expression
(define (list-cmp x y a b)
	(if (or (equal? x '()) (equal? y '())) '()
		(cons (expr-cmp (car x) (car y) a b) (list-cmp (cdr x) (cdr y) a b))
	)
)

;Check if x or y is a special form (if, let, lambda, quote)
(define (check-special x y)
	(if (or (or (or (equal? x 'if) (equal? y 'if))
		(or (equal? x 'let) (equal? y 'let)))
		(or (or (equal? x 'lambda) (equal? y 'lambda))
		(or (equal? x 'quote) (equal? y 'quote)))) #t
		#f
	)
)

;Check the bindings for lambda and build the binding lists
(define (build-let-bindings x y)
	(if (and (equal? x '()) (equal? y '())) (cons '() '())	
		(cons 
			(cons (car (car x)) (car (build-let-bindings (cdr x) (cdr y)))) 
			(cons (car (car y)) (cdr (build-let-bindings (cdr x) (cdr y))))
		)
	)
)

;In the bindings of lets, replace variables
(define (replace-in-bindings-x x a b)
	(if (equal? x '()) '()
		(cons (list (car (car x)) (make-pair-x (last (car x)) a b)) (replace-in-bindings-x (cdr x) a b))
	)
)

(define (replace-in-bindings-y y a b)
	(if (equal? y '()) '()
		(cons (list (car (car y)) (make-pair-y (last (car y)) a b)) (replace-in-bindings-y (cdr y) a b))
	)
)

;Compare two lets, also build for bindings
(define (let-cmp x y a b)
	(if (equal? (length (car (cdr x))) (length (car (cdr y))))
		(let ((z (build-let-bindings (car (cdr x)) (car (cdr y)))))
			(cons 'let (append (list (list-cmp (replace-in-bindings-x (car (cdr x)) a b) 
			(replace-in-bindings-y (car (cdr y)) a b) (car z) (cdr z))) 
			(list-cmp (cdr (cdr x)) (cdr (cdr y)) (append (car z) a) (append (cdr z) b))))
		)
		(const-cmp x y '() '())
	)
)

;Check the bindings for lambda and build the binding lists
(define (build-lambda-bindings x y)
	(if (and (equal? x '()) (equal? y '())) (cons '() '())
		(let ((z (build-lambda-bindings (cdr x) (cdr y)))) 
			(cons (cons (car x) (car z)) (cons (car y) (cdr z)))
		)
	)	
)

;Compare two lambdas, also build for bindings
(define (lambda-cmp x y a b)
	(if (equal? (length (car (cdr x))) (length (car (cdr x))))
		(let ((z (build-lambda-bindings (car (cdr x)) (car (cdr y)))))
			(list-cmp x y (append (car z) a) (append (cdr z) b))
		)
		(const-cmp x y '() '())	
	)
)

;The actual main function. Treats x and y as lists and compares element-wise, 
;according to the types. a, b are the lists of binding aliases in x and y
(define (expr-cmp x y a b)
	(if (and (and (list? x) (list? y)) (equal? (length x) (length y))) 
		(if (equal? (car x) (car y))
			(case (car x)
				('quote (const-cmp x y a b))
				('let (let-cmp x y a b))
				('lambda (lambda-cmp x y a b))
				('_ (const-cmp x y a b))
				(else (list-cmp x y a b))
			)
			(if (check-special (car x) (car y)) (const-cmp x y a b)
				(list-cmp x y a b)
			)
		)
		(const-cmp x y a b)
	)
)

;The wrapper for the main function as required
(define (expr-compare x y)
	(expr-cmp x y '() '())
)

(define (test-expr-compare x y)
	(and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
		(equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))
	)
)

(define (test-expr-x)
	'(
		;Simple let
		(let ((a 3) (b 4) (c 5)) (cons (cons a b) c))
		;Let with different number of bindings
		(let ((a 3) (b 4)) (a + b))
		;Quotes
		(quote 12)
		;Ifs
		(if (#t) 3 4)
		;Lambdas
		((lambda (a) (+ 12 a)) 10)
	)
)

(define (test-expr-y)
	'(
		(let ((a 4) (b 5) (c 6)) (cons (cons a c) b))
		(let ((a 3)) a)
		(quote 11)
		(if (#f) 2 5)
		((lambda (a) (+ 15 a)) 11)
	)
)
