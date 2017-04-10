;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS2500 Problem Set 14 (EXTRA CREDIT) - Seth Carroll & Kevin Khomiakov|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set 14 -- Seth Carroll & Kevin Khomiakov
;; carroll.se@husky.neu.edu & khomiakov.k@husky.neu.edu

(define-struct graph (nodes neighbors node=?))

; A [Graph X] is a structure:
; (make-graph [Listof X] [X -> [Listof X]] [X X -> Boolean])

(define G1
  (make-graph '(A B C D E F G)
              (lambda (n)
                (cond [(symbol=? n 'A) '(B E)]
                      [(symbol=? n 'B) '(E F)]
                      [(symbol=? n 'C) '(D)]
                      [(symbol=? n 'D) '()]
                      [(symbol=? n 'E) '(C F A)]
                      [(symbol=? n 'F) '(D G)]
                      [(symbol=? n 'G) '()]))
              symbol=?))

(define REVERSEG1
  (make-graph '(A B C D E F G)
              (lambda (n)
                (cond [(symbol=? n 'A) '(E)]
                      [(symbol=? n 'B) '(A)]
                      [(symbol=? n 'C) '(E)]
                      [(symbol=? n 'D) '(C F)]
                      [(symbol=? n 'E) '(A B)]
                      [(symbol=? n 'F) '(B E)]
                      [(symbol=? n 'G) '(F)]))
              symbol=?))

(define UNDIRECTED (make-graph '(A B C D E F G)
              (lambda (n)
                (cond [(symbol=? n 'A) '(B E)]
                      [(symbol=? n 'B) '(A E F)]
                      [(symbol=? n 'C) '(D E)]
                      [(symbol=? n 'D) '(C F)]
                      [(symbol=? n 'E) '(A B C F)]
                      [(symbol=? n 'F) '(B D E G)]
                      [(symbol=? n 'G) '(F)]))
              symbol=?))


; find-paths : [Graph X] X X -> [Listof [Listof X]]
; Find all of the paths in the graph from src to dest

(check-expect (find-paths G1 'C 'C) (list (list 'C))) ; src = dest
(check-expect (find-paths G1 'C 'G) empty) ; no paths from 'C to 'G
(check-expect (find-paths G1 'A 'B)
              (list (list 'A 'B)))
(check-expect (find-paths G1 'E 'G)
              (list (list 'E 'F 'G)
                    (list 'E 'A 'B 'F 'G)))
(check-expect (find-paths G1 'B 'G)
              (list (list 'B 'E 'F 'G)
                    (list 'B 'F 'G)))
(check-expect (find-paths G1 'A 'G)
              (list (list 'A 'B 'E 'F 'G)
                    (list 'A 'B 'F 'G)
                    (list 'A 'E 'F 'G)))

(define (find-paths g src dest)
  (find-paths/list g src dest empty))

; [Graph X] X X [List-of X] -> [List-of [List-of X]]
; Find all of the paths in the graph from src to dest (list version)

(check-expect (find-paths/list
               (make-graph empty (graph-neighbors G1) symbol=?) 'A 'F empty) empty)
(check-expect (find-paths/list G1 'C 'C empty) (list (list 'C))) ; src = dest
(check-expect (find-paths/list G1 'C 'G empty) empty) ; no paths from 'C to 'G
(check-expect (find-paths/list G1 'A 'B empty)
              (list (list 'A 'B)))
(check-expect (find-paths/list G1 'E 'G empty)
              (list (list 'E 'F 'G)
                    (list 'E 'A 'B 'F 'G)))
(check-expect (find-paths/list G1 'B 'G empty)
              (list (list 'B 'E 'F 'G)
                    (list 'B 'F 'G)))
(check-expect (find-paths/list G1 'A 'G empty)
              (list (list 'A 'B 'E 'F 'G)
                    (list 'A 'B 'F 'G)
                    (list 'A 'E 'F 'G)))

(define (find-paths/list g src dest seen)
  (cond [(empty? (graph-nodes g)) empty]
        [((graph-node=? g) src dest) (list (list src))]
        [else (foldr (λ (x y) (append
                               (make-path src (find-paths/list
                                               g x dest (cons src seen))) y))
                     empty
                     (not-seen ((graph-neighbors g) src) seen))]))

; [List-of X] [List-of X] -> [List-of X]
; Have we seen these neighbors?

(check-expect (not-seen (list 'A 'F) empty) (list 'A 'F))
(check-expect (not-seen (list 'A) empty) (list 'A))
(check-expect (not-seen (list 'A 'B 'C 'D 'E 'F) (list 'E)) (list 'A 'B 'C 'D 'F))
(check-expect (not-seen (list 'A 'B 'C 'D 'E 'F) (list 'A 'B 'C)) (list 'D 'E 'F))

(define (not-seen neighbors seen)
  (filter (λ (x) (not (member? x seen))) neighbors))

; X [List-of X] -> [List-of [List-of X]]
; Takes source and adds it to each path we have found

(check-expect (make-path 'A (list (list 'B 'F) (list 'B 'C 'F)))
              (list (list 'A 'B 'F) (list 'A 'B 'C 'F)))

(check-expect (make-path 'A (list '()))
              (list (list 'A)))
                         
             
(define (make-path scr paths)
  (map (λ (path) (cons scr path)) paths))

;; Problem 2

;; list-pass : [List-of X] [List-of X] [X->Boolean]  -> Boolean
;; Do two lists match the given operator's criteria

(check-expect (list-pass (list 1 2) (list 1 2) equal?) true)
(check-expect (list-pass (list 1 2) (list 1 3) equal?) false)
(check-expect (list-pass empty (list 1 2) equal?) false)
(check-expect (list-pass (list 1 2) '() equal?) false)
(check-expect (list-pass '() '() equal?) true)

(define (list-pass l1 l2 equals?)
  (or (and (empty? l1) (empty? l2))
      (and (cons? l1) (cons? l2)
           (local ((define l (acc-remover (first l1) l2 equals?)))
             (and (not (false? l)) (list-pass (rest l1) l equals?))))))

;; acc-remover : X [List X] [X->Boolean] -> [Maybe [List X]]
;; Removes an element from the list, otherwise false

(check-expect (acc-remover 2 (list 2 4) equal?) (list 4))
(check-expect (acc-remover 3 (list 2 4) equal?) false)
(check-expect (acc-remover 2 '() equal?) false)
(check-expect (acc-remover "foo" (list "foo" "oof") equal?) (list "oof"))
(check-expect (acc-remover "foo" (list "foo" "foo") equal?) '("foo"))

(define (acc-remover x alox equals?)
  (cond [(empty? alox) false] 
        [else (if (equals? x (first alox)) (rest alox)
            (if (false? (acc-remover x (rest alox) equals?)) (acc-remover x (rest alox) equals?)
                (cons (first alox) (acc-remover x (rest alox) equals?))))]))

;; same-graph? : Graph Graph -> Boolean
;; Are two graphs the same?

(check-expect (same-graph? (make-graph '() (lambda (x) '()) symbol=?)
             (make-graph '() (lambda (x) '()) symbol=?)) true)

(check-expect (same-graph? (make-graph '(a) (lambda (x) '()) symbol=?)
             (make-graph '() (lambda (x) '()) symbol=?)) false)

(check-expect (same-graph? (make-graph '(a) (lambda (x) '()) symbol=?)
             (make-graph '(a) (lambda (x) '()) symbol=?)) true)

(check-expect (same-graph? (make-graph '(b) (lambda (x) '()) symbol=?)
             (make-graph '(a) (lambda (x) '()) symbol=?)) false)

(check-expect (same-graph? (make-graph '(a b) (lambda (x) '()) symbol=?)
             (make-graph '(b a) (lambda (x) '()) symbol=?)) true)

(check-expect (same-graph? (make-graph '(a b)
                         (lambda (x)
                           (cond
                             [(symbol=? x 'a) '(b)]
                             [(symbol=? x 'b) '()]))
                         symbol=?)
             (make-graph '(a b)
                         (lambda (x)
                           (cond
                             [(symbol=? x 'b) '(a)]
                             [(symbol=? x 'a) '()]))
                        symbol=?))
              false)

(check-expect (same-graph? (make-graph '(a b)
                         (lambda (x)
                           (cond
                             [(symbol=? x 'a) '(a b)]
                             [(symbol=? x 'b) '()]))
                         symbol=?)
             (make-graph '(a b)
                         (lambda (x)
                           (cond
                             [(symbol=? x 'a) '(b a)]
                             [(symbol=? x 'b) '()]))
                         symbol=?)) true)

(define (same-graph? g1 g2)
  (local ((define (graph-list-pass x y)
            (list-pass x y (graph-node=? g1))))
    (and (graph-list-pass (graph-nodes g1) (graph-nodes g2))
         (andmap (lambda (x) (graph-list-pass ((graph-neighbors g1) x) 
                                              ((graph-neighbors g2) x))) 
                 (graph-nodes g1)))))

;; Problem 3

; reverse-edge-graph : [Graph X] -> [Graph X]
; Build a graph with the same nodes as g, but with reversed edges.
; That is, if g has an edge from a to b then the result graph will
; have an edge from b to a.

(check-expect (same-graph? REVERSEG1 (reverse-edge-graph G1)) true)
               
(define (reverse-edge-graph g)
  (make-graph (graph-nodes g)
              (lambda (y) (filter (lambda (x) (member? y ((graph-neighbors g) x)))
                                  (graph-nodes g)))
              (graph-node=? g)))

;; Problem 4

; undirected? : [Graph X] -> Boolean
; Determine if each edge in g has a matching edge going the opposite direction

(check-expect (undirected? G1) false)
(check-expect (undirected? REVERSEG1) false)
(check-expect (undirected? UNDIRECTED) true)

(define (undirected? g)
  (same-graph? g (reverse-edge-graph g))) 

