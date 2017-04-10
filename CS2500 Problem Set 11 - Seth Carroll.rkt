;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Problem Set 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PROBLEM SET 11 (By Kevin Khomiakov(khomiakov.k@husky.neu.edu)
;                   and Seth Carroll (carroll.se@husky.neu.edu))

;; A BTN is one of
;; - Number
;; - (make-node BTN BTN)
(define-struct node (left right))

; Problem 1

; Design a function, btn-height, that takes in a binary tree of numbers and computes
; the maximum distance from the root to any leaf. Here "distance" is measured by adding 1
; for each internal node on the way from the root to the leaf.

; btn-height: BTN->Number
; computes the maximum distance from the root to any leaf

(check-expect (btn-height 42) 0)
(check-expect (btn-height (make-node 2 (make-node 4 9)))2)

(define (btn-height btn)
  (local (; Number Number-> Number
          ; just compares two numbers and returns the largest
          ; made not to do some conds in the btn-height function body
          (define (btn-height-helper x y)
           (cond
             [(> x y) x]
             [else y])))
  (cond
    [(number? btn) 0]
    [else (btn-height-helper (+ 1 (btn-height (node-left btn)))
                             (+ 1 (btn-height (node-right btn))))])))


; Problem 2

; Design a function, btn-sum, that takes in a binary tree of numbers
; and computes the sum of all leaves.
; btn-sum: BTN -> Number
; takes in a binary tree of numbers and computes the sum of all leaves

(check-expect (btn-sum 42) 42)
(check-expect (btn-sum (make-node 2 (make-node 4 9))) 15)

(define (btn-sum btn)
  (cond [(number? btn) btn]
        [(node? btn) (+ (btn-sum (node-left btn))
                       (btn-sum (node-right btn)))]))

; Problem 3

; A LBT (Leafy Binary Tree) is one of 
; - 'leaf
; - (make-node LBT LBT)


; all-bts: Number -> [List-of BTs]
; consumes a natural number n 
; and creates (a list of) all leafy binary trees of height n
(define (all-bts n)
  (local (; combine-two-lists: [List-of BTs] [List-of BTs] -> [List-of BTs]
          ; takes two lists of binary trees
          ; and outputs a list of BTs which combines the given two lists of BTs together
          (define (combine-two-lists l1 l2)
            (cond [(empty? l1) empty]
                  [else (append (LoLBT+BT (first l1) l2)
                                (combine-two-lists (rest l1) l2))]))
          ; LoLBT+BT: BT [List-of BTs] -> [List-of BTs]
          ; takes a BT and a list of BTs
          ; and outputs a list of BTs that contains given BT in every element of LoBT
          (define (LoLBT+BT bt lobt)
            (cond [(empty? lobt) empty]
                  [else (cons (make-node bt (first lobt))
                              (LoLBT+BT bt (rest lobt)))]))
          ; bts-less Number-> [List-of BTs]
          ; takes a number (n) and return all BTs of height less than n
          (define (bts-less n)
            (cond [(zero? n) empty]
                  [else (append (all-bts (- n 1)) 
                                (bts-less (- n 1)))])))
    (cond [(zero? n) (list 'leaf)]
          [else (append (combine-two-lists (all-bts (- n 1)) 
                                (all-bts (- n 1)))
                        (combine-two-lists (all-bts (- n 1)) 
                                (bts-less (- n 1)))
                        (combine-two-lists (bts-less (- n 1)) 
                                (all-bts (- n 1))))])))

(check-expect (all-bts 0) (list 'leaf))
(check-expect (all-bts 1) (list (make-node 'leaf 'leaf)))
(check-expect (all-bts 2) (list(make-node (make-node 'leaf 'leaf) 
                                         (make-node 'leaf 'leaf))
                              (make-node (make-node 'leaf 'leaf) 'leaf)
                              (make-node 'leaf 
                                         (make-node 'leaf 'leaf))))
  
(check-expect (all-bts 3) (list
 (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))
 (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) (make-node (make-node 'leaf 'leaf) 'leaf))
 (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) (make-node 'leaf (make-node 'leaf 'leaf)))
 (make-node (make-node (make-node 'leaf 'leaf) 'leaf) (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))
 (make-node (make-node (make-node 'leaf 'leaf) 'leaf) (make-node (make-node 'leaf 'leaf) 'leaf))
 (make-node (make-node (make-node 'leaf 'leaf) 'leaf) (make-node 'leaf (make-node 'leaf 'leaf)))
 (make-node (make-node 'leaf (make-node 'leaf 'leaf)) (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))
 (make-node (make-node 'leaf (make-node 'leaf 'leaf)) (make-node (make-node 'leaf 'leaf) 'leaf))
 (make-node (make-node 'leaf (make-node 'leaf 'leaf)) (make-node 'leaf (make-node 'leaf 'leaf)))
 (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) (make-node 'leaf 'leaf))
 (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) 'leaf)
 (make-node (make-node (make-node 'leaf 'leaf) 'leaf) (make-node 'leaf 'leaf))
 (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf)
 (make-node (make-node 'leaf (make-node 'leaf 'leaf)) (make-node 'leaf 'leaf))
 (make-node (make-node 'leaf (make-node 'leaf 'leaf)) 'leaf)
 (make-node (make-node 'leaf 'leaf) (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))
 (make-node (make-node 'leaf 'leaf) (make-node (make-node 'leaf 'leaf) 'leaf))
 (make-node (make-node 'leaf 'leaf) (make-node 'leaf (make-node 'leaf 'leaf)))
 (make-node 'leaf (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))
 (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf))
 (make-node 'leaf (make-node 'leaf (make-node 'leaf 'leaf)))))