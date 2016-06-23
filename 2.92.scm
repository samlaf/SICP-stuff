;; ------------------------- COMMENTS ----------------------------- ;;
;; Exercise 2.92 of SICP                                            ;;
;; Be able to add/multiply polynomials that are indexed by          ;;
;; different variables                                              ;;
;; 1. implement general polynomial datatype                         ;; DONE!!!
;; 2. implement poly->gpoly method (expand polynomial)              ;; DONE!!!
;; 3. implement gpoly->poly method (rearrange terms to be indexed)  ;;
;; ------------------------- COMMENTS ----------------------------- ;;

;; UTILITIES

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=zero? n)
  (and (number? n) (= 0 n)))
(define (adjoin-ord-set e set comparison)
  (cond ((null? set) (list e))
        ((comparison e (car set)) (cons e set))
        (else (cons (car set) (adjoin-ord-set e (cdr set) comparison)))))
(define (flatten proc . seq)
  (reduce-left append () (apply map proc seq)))
(define (compose f g)
  (lambda (x) (f (g x))))

;; TERMS / TERM-LISTS

;; Term-list constructor/selectors
(define (make-termlist terms) terms)
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))

;; Term constructrs/selectors
(define (make-term order coeff) (cons order coeff))
(define (order term) (car term))
(define (coeff term) (cdr term))

;; Term/Term-list Methods
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (empty-termlist? term-list) (null? term-list))
(define (the-empty-termlist) '())

;; We use this as a simplification
(define add +)
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define mul *)
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))





;; INDEXED POLYNOMIALS (indexed by a variable)

;; polynomials Constructor/Selectors
(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable poly) (car poly))
(define (term-list poly) (cdr poly))
(define (poly? p)
  (and (pair? p) (symbol? (variable p))))

;; polynomials methods
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))




;; GENERAL POLYNOMIALS (not indexed in a variable)
;; We need a general version of polynomials

;; First we have varpower datatype (eg. x^2, y^3, abc^100)
(define (make-varpower var power) (cons var power))
(define (var-varpower varpower) (car varpower))
(define (power-varpower varpower) (cdr varpower))

;; Then we make a general polynomial term datatype (2x^3y^4, z^100abc^2)
(define (make-gterm coeff varpower-list) (cons coeff varpower-list))
(define (coeff-gterm gterm) (car gterm))
(define (varpower-list gterm) (cdr gterm))

;; Method for adjoining a varpower to a gterm (3x^3 * y^2 = 3x^3y^2)
(define (adjoin-varpower vpower gterm)
  (make-gterm (coeff-gterm gterm)
              (adjoin-ord-set vpower
                              (varpower-list gterm)
                              (lambda (a b) (var<? (car a) (car b))))))

;; We need a var comparison operator to order our gterms according to variables
;; eg. (always x^2y^2z^2 not y^2x^2z^2 or some other combination)
(define (var<? v1 v2)
  (let ((s1 (symbol->string v1))
        (s2 (symbol->string v2)))
    (string<? s1 s2)))

;; VARPOWER TESTS
(define sample-vp (make-varpower 'x 2))
(define sample-gterm (make-gterm 3 '((y . 2) (z . 3))))
(adjoin-varpower sample-vp sample-gterm)

;; And finally, we have the general polynomial datatype (x + xy +
;; x^2y...)
(define (make-gpoly gterm-list) gterm-list)
(define (first-term-gpoly gpoly) (car gpoly))
(define (rest-terms-gpoly gpoly) (cdr gpoly))


;; Methods for converting
(define (poly->gpoly poly)
  (define (transform vpower gpoly)
    ;; vpower is of the form (x . 4)
    ;; introduces vpower into every gterm of gpoly
    (if (= 0 (power-varpower vpower))
        gpoly
        (map (lambda (gterm) (adjoin-varpower vpower gterm)) gpoly)))
  (if (not (poly? poly))
      ;;Then poly is an int, so we make it a gpoly
      ;;for eg (x (4 . 1)) will call rec (poly->gpoly 1)
      (make-gpoly (list (list poly)))
      ;; Else we have eg (x (4 . [poly in y]) (2 . [poly in z]))
      (let ((var (variable poly))
            (t-list (term-list poly)))
        (flatten transform
                 (map (lambda (x) (make-varpower var (order x))) t-list)
                 (map (compose poly->gpoly coeff) t-list)))))
      
      

                   
;; TESTS
(define sample-poly (make-poly 'x (list (make-term 3 2) (make-term 2 1))))
(define sample-poly-y (make-poly 'y '((2 . 1) (0 . 6))))
(define sp (make-poly 'x (list (make-term 0 1))))
(define cp (make-poly 'x (list (make-term 2 sample-poly-y)
                               (make-term 1 sample-poly-y))))
(poly->gpoly sp)
(poly->gpoly sample-poly)
(poly->gpoly sample-poly-y)
(poly->gpoly cp)




(add-poly sample-poly sample-poly)
(mul-poly sample-poly-y sample-poly-y)

