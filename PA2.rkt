;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname PA2) (read-case-sensitive #t) (teachpacks ((lib "master.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "master.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
;
; Traveling Salesman solution--brute force
; PA 2 - Scheme
; Joung Kim
;

(require "distances.rkt")  ; pull in the distances matrix as a list of lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of the first pos elements of the list lst
(define (take lst pos)
  (if (= pos 0)
      null
      (cons (car lst) (take (cdr lst) (- pos 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns a list of all elements after the first pos elements of the list lst
(define (drop lst pos)
  (if (= pos 0)
      lst
      (drop (cdr lst) (- pos 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inserts the element at position pos in the list lst
(define (insert-at elem pos lst)
  (append (take lst pos) (list elem) (drop lst pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inserts the element elem to positions pos, pos+1, ..etc in the list lst, and returns the resulting list of lists
(define (insert-everywhere elem pos lst)
  (if (> pos (length lst))
      null
      (cons (insert-at elem pos lst) (insert-everywhere elem (+ pos 1) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inserts the element elem in every possible position of every sublist of list lst
(define (insert-everywhere/in-all-lists elem lst)
  (if (null? lst)
      null
      (append (insert-everywhere elem 0 (car lst)) (insert-everywhere/in-all-lists elem (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Given a list, find all possible permutations, result is unsorted
(define (arrangements lst)
  (if (= 1 (length lst))
      (list lst)
      (insert-everywhere/in-all-lists (car lst)
                                      (arrangements (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lexicographically compare two lists of symbols
(define (list<? lst1 lst2)
  (cond ((and (null? lst1) (null? lst2)) false)
        ((and (null? lst1) (not (null? lst2))) true)
        ((and (not (null? lst1)) (null? lst2)) false)
        ((string<? (symbol->string (car lst1)) (symbol->string (car lst2))) true)
        ((string>? (symbol->string (car lst1)) (symbol->string (car lst2))) false)
        ((string=? (symbol->string (car lst1)) (symbol->string (car lst2)))
         (list<? (cdr lst1) (cdr lst2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return a list containing all permutations of an input list a
(define (permute a)
  (sort (arrangements a) list<?))

(check-expect (permute '(a b c)) '((a b c)(a c b)(b a c)(b c a)(c a b)(c b a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return a list of permutations of list a formed by taking each element x
; from the list items as the first symbol in the permutation, and forming all
; permutation of the symbols of list a with x removed.
(define (permute-each items a)
  (if (null? items)
      null
      (append (map (lambda (lst) (cons (car items) lst)) (permute a))
              (permute-each (cdr items) a))))

(check-expect (permute-each '(a b) '(c d)) '((a c d)(a d c)(b c d)(b d c)))
(check-expect (permute-each null '(a b c)) null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Cons the value on to each list in the list aa
(define (cons-all value aa)
  (map (lambda (sublist) (cons value sublist)) aa))

(check-expect (cons-all 'a '((1 2)(3 4)(5 6))) '((a 1 2)(a 3 4)(a 5 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Define some constants to be used in testing the remaining functions
(define test-cities (list 'Washington 'Detroit 'Miami 'Denver 'Houston ))
(define test-distance-matrix (list
    (list 'Miami 'Houston 1908)
    (list 'Miami 'Washington 1699)
    (list 'Miami 'Denver 3323)
    (list 'Miami 'Detroit 2226)
    (list 'Houston 'Miami 1908)
    (list 'Houston 'Washington 2266)
    (list 'Houston 'Denver 1658)
    (list 'Houston 'Detroit 2119)
    (list 'Washington 'Miami 1697)
    (list 'Washington 'Houston 2270)
    (list 'Washington 'Denver 2667)
    (list 'Washington 'Detroit 843)
    (list 'Denver 'Miami 3323)
    (list 'Denver 'Houston 1663)
    (list 'Denver 'Washington 2720)
    (list 'Denver 'Detroit 2045)
    (list 'Detroit 'Miami 2227)
    (list 'Detroit 'Houston 2111)
    (list 'Detroit 'Washington 845)
    (list 'Detroit 'Denver 2041)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fetch the distance bewteen city1 and city2 from the distance matrix dmatrix
; If a city is not present, then return a big number (999999999)
(define (distance city1 city2 dmatrix)
  (cond ((null? dmatrix) 999999999)
        ((and (equal? city1 (first (car dmatrix))) (equal? city2 (second (car dmatrix)))) (third (car dmatrix)))
        (else (distance city1 city2 (cdr dmatrix)))))

(check-expect (distance 'Detroit 'Houston test-distance-matrix) 2111)
(check-expect (distance 'Detroit 'Montreal test-distance-matrix) 999999999)
(check-expect (distance 'Houston 'Miami test-distance-matrix) 1908)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return the length of a path, where a path is a list of cities, and its
; length is the sum of the distances from each city in the list to the next,
; as indicated by the distance matrix dmatrix.
(define (path-length cities dmatrix)
  (cond ((null? cities) 0)
        ((= 1 (length cities)) 0)
        (else (+ (distance (first cities) (second cities) dmatrix)
                 (path-length (cdr cities) dmatrix)))))

(check-expect (path-length test-cities test-distance-matrix) 8056)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return the length of a circuit, where a circuit is a list of cities, and
; its length is the sum of the distances from each city in the list to the
; next, plus the distance from the last city in the list to the first, as
; indicated in the distance matrix dmatrix.
;
; Note that a path from a to c through b is represented as (a b c), and a
; circuit from a through b and c back to a is also represented (a b c)--we
; just intepret the lists differently for paths and circuits.
(define (circuit-length cities dmatrix)
  (+ (path-length cities dmatrix)
     (distance (list-ref cities (- (length cities) 1)) (list-ref cities 0) dmatrix)))

(check-expect (circuit-length test-cities test-distance-matrix) 10322)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return a list of all circuits from the first city in a list of cities
; through all other cities in the list--that is, a list of all permuations of
; a list of cities that start with the first city in the list.
(define (make-circuits cities)
  (map (lambda (cities-lst) (cons (car cities) cities-lst)) (permute (cdr cities))))

(check-expect (make-circuits '(a b c d))
              '((a b c d)(a b d c)(a c b d)(a c d b)(a d b c)(a d c b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return a list of lists of the form (length circuit) where circuit is an
; element of the circuits parameter, and length is the length of the circuit
; according to the data in the distance matrix dmatrix.
; We will call the list of circuits and lengths returned by this function a
; lc-list (a length/circuit list), and an element of this list an lc
; (length/circuit) in later functions.
(define (add-circuit-lengths circuits dmatrix)
  (if (null? circuits)
      null
      (cons (list (circuit-length (car circuits) dmatrix) (car circuits))
            (add-circuit-lengths (cdr circuits) dmatrix))))

(check-expect (add-circuit-lengths (make-circuits '(Washington Denver Detroit))
                                   test-distance-matrix)
              '((5557 (Washington Denver Detroit))
                (5604 (Washington Detroit Denver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compare two length/circuits and return the one with the smaller length; if
; they are the same, return the second
(define (min-lc lc1 lc2)
  (if (< (first lc1) (first lc2))
      lc1
      lc2))

(check-expect (min-lc '(34 'Abel 'Baker) '(22 'Charlie 'Dog)) '(22 'Charlie 'Dog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return the minimim lc in an lc-list, that is, the circuit with the shortest
; length.
(define (min-lc-list lc-list)
  (if (= 1 (length lc-list))
      (first lc-list)
      (let ((min-lc (min-lc-list (cdr lc-list))))
        (if (<= (first (car lc-list)) (first min-lc))
            (car lc-list)
            min-lc))))
      

(check-expect (min-lc-list '((34 "Abel" "Baker") (22 "Charlie" "Dog") (24 "Easy" "Fox")))
              '(22 "Charlie" "Dog"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Return the lc representing the shortest circuit in a list of cities given
; distances between them in distance matrix dmatrix; in other words solve
; the travelling salesman problem.
(define (shortest-circuit cities dmatrix)
  (min-lc-list (add-circuit-lengths (make-circuits cities) dmatrix)))

(check-expect (shortest-circuit test-cities test-distance-matrix)
              '(8153 (Washington Miami Houston Denver Detroit)))


