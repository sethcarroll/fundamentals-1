;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |CS2500 Problem Set 6 - Seth Carroll & Evan Sevieri|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 6
;; Evan Sevieri and Seth Carroll
(require 2htdp/image)
(require 2htdp/universe)


;; A Letter is a (make-letter String Number)
;; A Number is a weight value less than 3.5 ounces
(define-struct letter [address weight])

#;(define (letter-temp aletter)
  (... (letter-address aletter) ...
       (letter-weight aletter) ...))

;;A Box is a (make-box Number Number Number Number)
;; A Box's width, height, and length must be numbers 
;;   with a sum less than 62 inches and a volume no more than 7938 inches
;; A Box's weight is a number less than 800 ounces
(define-struct box [width height length weight])

#;(define (box-temp abox)
  (... (box-width abox) ...
       (box-height abox) ...
       (box-length abox) ...
       (box-weight abox) ...))

;; An Item is one of:
;;  -Letter
;;  -Box

#;(define (item-temp aitem)
  (cond
    [(letter? aitem) (letter-temp aitem)]
    [(box? aitem) (box-temp aitem)]))


;; Item -> Boolean
;; Checks if the item satisfies the rules for mail

(define MAIL1 (make-letter "1200 Boylston Ave." 12))
(define MAIL2 (make-letter "3456 Massachusetts Ave." 2))
(define MAIL3 (make-box 11 20 20 575))
(define MAIL4 (make-box 11 32 41 500))
(define MAIL5 (make-box 11 32 24 760))
(define MAIL6 (make-box 10 20 22 806))

(check-expect (item-ok? MAIL1) #f)
(check-expect (item-ok? MAIL2) #t)
(check-expect (item-ok? MAIL3) #t)
(check-expect (item-ok? MAIL4) #f)
(check-expect (item-ok? MAIL5) #f)
(check-expect (item-ok? MAIL6) #f)

(define (item-ok? aitem)
  (cond
    [(letter? aitem) (letter-ok? aitem)]
    [(box? aitem) (box-ok? aitem)]))

;; Letter -> Boolean
;; Checks if a letter satifies the rules of a letter

(check-expect (letter-ok? MAIL1) #f)
(check-expect (letter-ok? MAIL2) #t)

(define (letter-ok? aletter)
  (< (letter-weight aletter) 3.5))

;; Box -> Boolean
;; Checks if a box satisfies the rules of a box

(check-expect (box-ok? MAIL3) #t)
(check-expect (box-ok? MAIL4) #f)
(check-expect (box-ok? MAIL5) #f)
(check-expect (box-ok? MAIL6) #f)

(define (box-ok? abox)
  (and (< (+ (box-width abox) 
             (box-height abox) 
             (box-length abox))
          62)
       (< (* (box-width abox) 
             (box-height abox) 
             (box-length abox))
          7938)
       (< (box-weight abox)
          800)))

;; A LOI, List of Items, is one of:
;; - empty
;; - (cons Item LOI)

#;(define (loi-temp aloi)
    (cond
      [(empty? aloi) ...]
      [else ... (item-temp (first loi)) ...
            ... (loi-temp (rest aloi)) ...]))

;; LOI -> LOI
;; Consumes a LOI and produces a LOI of items that
;; don't satisfy the rules

(define LOI1 (list MAIL1 MAIL2 MAIL5))
(define LOI2 (list MAIL2 MAIL3 MAIL4))
(define LOI3 (list MAIL1 MAIL2 MAIL3
                   MAIL4 MAIL5 MAIL6))

(check-expect (bad-items LOI1) (cons MAIL1 (cons MAIL5 empty)))
(check-expect (bad-items LOI2) (cons MAIL4 empty))
(check-expect (bad-items LOI3) (cons MAIL1
                                     (cons MAIL4
                                           (cons MAIL5
                                                 (cons MAIL6 empty)))))
(define (bad-items aloi)
  (cond
      [(empty? aloi) empty]
      [else  (if (item-ok? (first aloi)) 
                 (bad-items (rest aloi))
                 (cons (first aloi)
                       (bad-items (rest aloi))))]))


;; LOI -> Number
;; Calculates the total postage of a List of Items

(check-expect (total-postage LOI1) .5)
(check-expect (total-postage LOI2) 86.75)
(check-expect (total-postage LOI3) 86.75)

(define (total-postage aloi)
  (cond
      [(empty? aloi) 0]
      [else (if (item-ok? (first aloi))
                (+ (item-calc (first aloi)) 
               (total-postage (rest aloi)))
                 (total-postage (rest aloi)))]))


;; Item -> Number
;; Calculates the postage of an item

(check-expect (item-calc MAIL3) 86.25)
(check-expect (item-calc MAIL2) 0.50)

(define (item-calc aitem)
  (cond
    [(letter? aitem) .50]
    [(box? aitem) (* .15 (box-weight aitem))]))
  
                                          
              
