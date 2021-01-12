;; Programming Language: Scheme Assignment
;; Chi-Yu (Evan) Lin
;; N16594213
;;-------------------------------------
;; Q1
;; (fromTo k n) returns the integers from k to n
;; Base Case: if k is larger than n, then return an empty list
;; Hypothesis: assume that (fromTo (+ k 1) n) works, and it returns the list of integers from k+1 to n
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: (from k n) = (cons k (fromTo (+ k 1) n)
(define (fromTo k n)
  (cond ((> k n) '())
        (else (cons k (fromTo (+ k 1) n)))))
;; Test Case:
;; (fromTo 3 8)
;; (fromTo 8 7)
;;-------------------------------------
;; Q2
;; (removeMults m L) returns a list containing all the elements of L that are not multiples of m
;; Base Case: if L is empty, then return an empty list.
;; Hypothesis: assume that (removeMults m (cdr L)) works, and it returns a list containing all the elements of (cdr L) except multiples of m.
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: if (car L) is a multiple of m, we simply return (removeMults m (cdr L)).
;;                 if (car L) is not a multiple of m, we return concatenation of (car L) and (removeMults m (cdr L)).
(define (removeMults m L)
  (cond ((null? L) '())
        ((= 0 (modulo (car L) m)) (removeMults m (cdr L)))
        (else (cons (car L) (removeMults m (cdr L))))))

;; Test Case:
;; (removeMults 3 '(2 3 4 5 6 7 8 9 10))
;; (removeMults 4 (fromTo 20 40))
;;-------------------------------------
;; Q3
;; (removeAllMults L) returns a list containing the increasing integers while those elements are not multiples of each other
;; Base Case: if L is empty, then return an empty list.
;; Hypothesis: if removeAllMults works for (cdr L) by removing all the multiples of (car L), that is (removeMults (car L) (cdr L)).
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: we concatenate (car L) with (removeAllMults (removeMults (car L) (cdr L)))
(define (removeAllMults L)
  (cond ((null? L) '())
        (else (cons (car L)(removeAllMults (removeMults (car L) (cdr L)))))))
;; Test Case:
;; (removeAllMults '(3 4 6 7 8 10 12 15 20 22 23))
;;-------------------------------------
;; Q4
;; (primes n) returns a list of all primes less than or equal to n.
;; Explanation: We simply call a (fromTo 2 n) first to constuct a list of integer from 2 to n.
;;              Then we use removeAllMults to remove all the multiples in the list (fromTo 2 n).
(define (primes n)
  (removeAllMults (fromTo 2 n)))
;; Test Case:
;; (primes 30)
;;-------------------------------------
;; Q5
;; (maxDepth L) returns the maximum nesting depth of any element within L
;; Base Case: if L is not a list(is an integer), then return -1
;;            if L is an empty list, then return 0
;; Hypothesis: assume that (maxDepth (car L)) and (maxDepth (cdr L)) works, then we can use the two result to do a comparison later
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: let a=(maxDepth (car L))+1, b=(maxDepth (cdr L)).
;;                 The reason why there is a "plus one" in a is because that it is in a deeper layer. Therefore, we add one to its value.
;;                 if a>b, then we return a.
;;                 else, we return b.
(define (maxDepth L)
  (cond ((not (list? L)) -1)
        ((null? L) 0)
        (else (let ((a (+ 1 (maxDepth (car L)))) (b (maxDepth (cdr L))))
                (if (> a b) a b)))))
;; Test Case:
;; (maxDepth '(1 2 3))
;; (maxDepth '((0 1) (2 (3 (4 5 (6 (7 8) 9) 10) 11 12) 13) (14 15)))
;; (maxDepth '())
;;-------------------------------------
;; Q6
;; (prefix exp) returns an atom or a list of three elements in prefix expression
;; Base Case: if exp is not a list, then return exp
;;            if exp is a list with one element, then return (car exp)
;; Hypothesis: assume that prefix works for the first element:(car exp), the second element:(cadr exp), the rest of the elements:(cddr exp).
;;             also, we name them as n1, optr, and n2, respectively.
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: We then return a list with the order: optr n1 n2, which is (list (prefix potr) (prefix n1) (prefix n2))
(define (prefix exp)
  (cond ((not (list? exp)) exp)
        ((= 1 (length exp)) (car exp))
        (else (let ((n1 (car exp)) (optr (cadr exp))(n2 (cddr exp)))
                (list (prefix optr) (prefix n1) (prefix n2))))))
;; Test Case:
;; (prefix 3)
;; (prefix '(3 + 4))
;; (prefix '((3 + 4) * 5))
;; (prefix '(3 + 4 * 5 - 6))
;; (prefix '((3 * 4) + (5 - 6) * 7))
;;-------------------------------------
;; Q7
;; (composition fns) returns a function that is the composition of the functions in fns.
;; My Parameters: Let f1=(car fns), fb=(cdr fns).
;; Base Case: if fb is an empty list(length of fns is one), then return the only function in fbs(which is f1).
;; Hypothesis: assume that composition works for fb (size of this problem is one less than the size of the original problem)
;; Recursive Step: we return the function of (f1 ((composition fb) x)).
(define (composition fns)
  (let ((f1 (car fns)) (fb (cdr fns)))
    (if (null? fb) (lambda (x) (f1 x))
        (lambda (x) (f1 ((composition fb) x))))))
  
;  (cond ((null? fns) )
;        ((= 1 (length fns)) (car fns))
;        (else (lambda (x) (((car fns) ((cdr fns) x))))
;        (else ((car fns) (composition (cdr fns))))))
;; Test Case:
;; (define f (composition (list (lambda (x) (+ x 1)) (lambda (x) (* x 2)))))
;; (f 3)
;;-------------------------------------
;; Q8
;; (bubble-to-nth L N) returns a list containing all the elements of L,
;;                     except that the largest element among the first N elements of L is now the Nth element of the list.
;; Base Case: if N=1, then we can simply return L since we don't need to do anything is N is one.
;;            if length of L is smaller than 2 (e.g. 0 or 1), then we can also simply return L since no comparison is needed.
;; My Parameters: I have name the first element in L as a1, the second element in L as a2.
;; Hypothesis: assume that bubble-to-nth works for (bubble-to-nth (cdr L) (- N 1)) and (bubble-to-nth (cons a1 (cddr L)) (- N 1))
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: We compare a1 and a2: if a1<a2, then we return concatenation of a1 and (bubble-to-nth (cdr L) (- N 1)).
;;                                       else, we return concatenation of a2 and (bubble-to-nth (cons a1 (cddr L)) (- N 1))
(define (bubble-to-nth L N)
  (cond ((= N 1) L)
        ((< (length L) 2) L)
        (else (let ((a1 (car L)) (a2 (cadr L)))
          (if (< a1 a2) (cons a1 (bubble-to-nth (cdr L) (- N 1)))
                        (cons a2 (bubble-to-nth (cons a1 (cddr L)) (- N 1))))))))
;; Test Case:
;; (bubble-to-nth '(1 6 2 3 5 4 8 0) 3)
;; (bubble-to-nth '(1 6 2 3 5 4 8 0) 4)
;; (bubble-to-nth '(1 6 2 3 5 4 8 0) 5)
;; (bubble-to-nth '(1 6 2 3 5 4 8 0) 6)
;; (bubble-to-nth '(1 6 2 3 5 4 8 0) 7)
;; (bubble-to-nth '(1 6 2 3 5 4 8 0) 8)
;;-------------------------------------
;; Q9
;; (b-s L N) returns a list containing the elements of L in their original order except that the first N elements are in sorted order.
;; Base Case: if N=1, we can return L directly since we do not need to do anything.
;; Hypothesis: assume that (b-s (bubble-to-nth L N) (- N 1)) works,
;;             and it returns a list where the largest element among the first N elements of L is now the Nth element of the list.
;;             (size of this problem is one less than the size of the original problem)
;; Recursive Step: we recursively call (b-s (bubble-to-nth L N) (- N 1)).
(define (b-s L N)
  (cond ((= N 1) L)
        (else (b-s (bubble-to-nth L N) (- N 1)))))
;; Test Case:
;; (b-s '(1 6 2 3 5 4 8 0) 1)
;; (b-s '(1 6 2 3 5 4 8 0) 2)
;; (b-s '(1 6 2 3 5 4 8 0) 3)
;; (b-s '(1 6 2 3 5 4 8 0) 4)
;; (b-s '(1 6 2 3 5 4 8 0) 5)
;; (b-s '(1 6 2 3 5 4 8 0) 6)
;; (b-s '(1 6 2 3 5 4 8 0) 7)
;; (b-s '(1 6 2 3 5 4 8 0) 8)
;; (b-s '() 8)
;;-------------------------------------
;; Q10
;; (bubble-sort L) returns a list of L in sorted order by calling b-s
;; Explanation: If L is an empty list, return empty list.
;;              Else, we simply call (b-s L (length of L))
(define (bubble-sort L)
  (if (null? L) '() (b-s L (length L))))
;; Test Case:
;; (bubble-sort '(1 6 2 3 5 4 8 0))
;; (bubble-sort '(9 38 2 5 1 0 -2 4 38 99))
;; (bubble-sort '(1))
;; (bubble-sort '())
;;-------------------------------------

(define (reduce f L)
  (cond ((= 1 (length L)) (car L))
        (else (f (car L) (reduce f (cdr L))))))

(reduce + '(1 2 3 4))


(reduce (lambda (x y)
          (if (> x y) x y))
        '(1 2 3 4))

;;(define (sum L)
;;  (if (null? (cdr L)) (car L) (+ (car L) (sum (cdr L)))))
;;(sum '(1 3 4 2))


(define (average L)
  (letrec ((sum (lambda (x)
                 (if (null? x) 0 (+ (car x) (sum (cdr x)))))))
    (/ (sum L) (length L))
    ))
(average '(1 3 28))

(define (bar x)
  (lambda (L) (map (lambda (y) (+ x y)) L)))
((bar 3) '(1 2 4 52 2))


(define (find-nth n L)
  (if (= 1 n) (car L) (find-nth (- n 1) (cdr L))))
(find-nth 3 '(3 4 2 5))


(define (in-relation op L)
  (cond ((null? L) #t)
        ((= 1 (length L)) #t)
        ((not(op (car L) (cadr L))) #f)
        (else (in-relation op (cdr L)))))
(in-relation < '())
(in-relation < '(1 2 3))
(in-relation < '(2 8 1))
(in-relation eq? '(1 1 1))
(in-relation eq? '(1 2 1))
(in-relation (lambda (x y) (eq? y (+ x 1))) '(1 2 3))
(in-relation (lambda (x y) (eq? y (+ x 1))) '(1 2 4))


(define (helper k n L)
  (cond ((null? L) '())
        ((= k 1) (helper n n (cdr L)))
        (else (cons (car L) (helper (- k 1) n (cdr L))))))
(helper 3 3 '(1 2 4 6 5 9 7 8))     

(define (drop n L) (helper n n L))
(drop 2 '(1 2 3 4 5 6 7 8 9))



((lambda (x y) (+ x y)) 5 6)
(map (lambda (a) (* 2 a)) '(8 7 9))
(Let ((x 5)(y 8)) (+ x y))
(Let ((F (lambda (x y) (+ x y))) (n 5) (m 7))
     (F n m))

(define (combine fns)
  (Let ((f1 (car fns))(fn (cdr fns)))
       (cond ((null? fn) (lambda (x) (f1 x)))
             (else (lambda (x) (f1 ((combine fn) x)))))))
(define f (combine (list (lambda (x) (+ x 1)) (lambda (x) (* x 2)))))
(f 3)


(define (TFns L x)
  (cond ((null? L) '())
        ((eq? ((car L) x) #t) (cons (car L) (TFns (cdr L) x)))
        (else (TFns (cdr L) x))))


(define (is3 y) (= y 3))
(define (is4 y) (= y 4))
(define (is4orMore y) (>= y 4))
(TFns (list is3 is4 is4orMore) 4)


(define (count x L)
  (cond ((null? L) 0)
        ((list? (car L)) (+ (count x (car L)) (count x (cdr L))))
        ((= x (car L)) (+ 1 (count x (cdr L))))
        (else (count x (cdr L)))))
(count 3 '((1 2 (3 4)) 3 ((((2 1) 3) 4) 5) 6 3))


(define (increasing? L)
  (cond ((= 1 (length L)) #t)
        ((> (car L) (cadr L)) #f)
        (else (increasing? (cdr L)))))
(increasing? '(1 3 5 6 10))
(increasing? '(1 3 5 4 10))


(define (decPrefix L)
  (cond ((null? L) )