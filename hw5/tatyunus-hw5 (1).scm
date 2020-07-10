(define symbol-length 
	(lambda (inSym)
		(if (symbol? inSym)
			(string-length (symbol->string inSym))
			0	
		)
	)
)





(define sequence? 
	(lambda (inSeq)
		(if (list? inSeq)
			(cond 
				((null? inSeq) 'true)
				((symbol? (car inSeq))
					(if (eq? (symbol-length (car inSeq)) 1)
						(sequence? (cdr inSeq))
						false
					))
				(else false)
			)
			false
		)
	)
)




(define same-sequence?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
				(cond
					((null? inSeq1)
						(if (null? inSeq2)
							true
							false
						))
					(else 
						(if (null? inSeq2)
							false
							(if (eq? (car inSeq1) (car inSeq2))
								(same-sequence? (cdr inSeq1) (cdr inSeq2))
								false
							)
						)
					)
				)
			(error "error")	
		)
	)
)

(define reverse-sequence
	(lambda (inSeq)
		(if(sequence? inSeq)
				(if (null? inSeq)
					'()
					(append (reverse-sequence (cdr inSeq)) (list (car inSeq)))
				)
		    (error "error")
		)
	)	
)

(define palindrome?
	(lambda (inSeq)
		(if (sequence? inSeq)
			(cond
				((same-sequence? (reverse-sequence inSeq) inSeq) true)
				(else false)
			)
			(error "error")
		)
	)
)

(define member?
	(lambda (inSym inSeq)
		(if (and (symbol? inSym) (sequence? inSeq))
			(if(null? inSeq) 
         false
			   (if (eq? (car inSeq) inSym)
						true
			      (member? inSym (cdr inSeq))
					)
				)
			(error "error")
		)
	)
)

(define remove-member
	(lambda (inSym inSeq)
		(if (symbol? inSym)
			(if (sequence? inSeq) 
				(if (member? inSym inSeq)
					(cond
						((eq? inSym (car inSeq)) (cdr inSeq))
						(else (append (list (car inSeq)) (remove-member inSym (cdr inSeq))))
					)
					(error "error")
				)
				(error "error")
			)
			(error "error")
		)
	)
)

(define helper
	(lambda (inSeq1 inSeq2)
		(cond
			((and (null? inSeq1) (null? inSeq2)) true)
			((null? inSeq2) false)
			((null? inSeq1) false)
			(else
				(if (member? (car inSeq1) inSeq2)
					(begin 
						(let 
							((inSeq2 (remove-member (car inSeq1) inSeq2))
							(inSeq1 (remove-member (car inSeq1) inSeq1)))
							(helper inSeq1 inSeq2)
						)
					)
					false
				)
			)
		)
	)
)
							
					
(define anagram?
	(lambda (inSeq1 inSeq2)
		(if (sequence? inSeq1) 
			(cond
				((sequence? inSeq2) (helper inSeq1 inSeq2))
				(else (error "error"))
			)										
			(error "error")
		)
	)
)

(define anapoli?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(if (and (palindrome? inSeq2) (anagram? inSeq1 inSeq2))
				true
				false				
			)
			(error "error")
		)
	)
)


