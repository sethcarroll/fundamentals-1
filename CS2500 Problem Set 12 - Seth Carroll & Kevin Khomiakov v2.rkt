;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS2500 Problem Set 12 - Seth Carroll & Kevin Khomiakov FIXv2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Problem Set 12 -- Seth Carroll & Kevin Khomiakov
;     carroll.se@husky.neu.edu & khomiakov.k@husky.neu.edu

(require 2htdp/image)


; Problem 1

;; An Atom is one of:
;; - Number
;; - Symbol
;; - String

;; An SExp is one of:
;; - Atom
;; - [List-of SExp]


;; sexp->string : S-expr -> String
;; converts any s-expression to a string representation

(check-expect (sexp->string 3) "3")
(check-expect (sexp->string 'foo) "foo")
(check-expect (sexp->string "hello") "\"hello\"")
(check-expect (sexp->string '(37 "foo")) "(37 \"foo\" )")
(check-expect (sexp->string '(a (37 "foo") c)) "(a (37 \"foo\" ) c )")

(define (sexp->string sexp)
  (local (;; sexp-string-list : [List-of SExp] -> String
          ;; converts a list of s-expressions to a string representation
          (define (sexp-string-list sl)
            (cond [(empty? sl) ")"]
                  [else (string-append (sexp->string (first sl)) " "
                                       (sexp-string-list (rest sl)))])))
    (cond [(atom? sexp) (atom->string sexp)]
          [else (string-append "("  (sexp-string-list sexp))])))

;; check-expects modified to accomodate acceptable white space ending lists.
;; Approved by TA.

;; atom? : X -> Boolean
;; checks if an item x is an atom

(check-expect (atom? "foo") true)
(check-expect (atom? 'blue) true)
(check-expect (atom? 37) true)
(check-expect (atom? '(37 "foo")) false)

(define (atom? atom)
  (or (number? atom) (string? atom) (symbol? atom)))

;; atom->string : Atom -> String
;; converts an atom to a string representation

(check-expect (atom->string "foo") "\"foo\"")
(check-expect (atom->string 'blue) "blue")
(check-expect (atom->string 37) "37")
(check-expect (atom->string "") "\"\"")
               
(define (atom->string anatom)
  (cond [(number? anatom) (number->string anatom)]
        [(symbol? anatom) (symbol->string anatom)]
        [(string? anatom) (string-append "\"" anatom "\"")]))


; Legos! (Problem 2-6)

(define-struct lego (label color width))
;; A Lego is a structure:
;;    (make-lego Number Symbol Number)
;; interpretation: (make-lego l c w) is the lego brick
;; with label l, color c, and width w (in pixels).

(define-struct bigger (lego left right))
;; A LegoBldg (lego building) is one of:
;; - Lego
;; - (make-bigger Lego LegoBldg LegoBldg)
;; interpretation: (make-bigger l lft rgt) makes a bigger
;; lego building by putting a lego brick l on top of two lego
;; buildings lft (left) and rgt (right).

(define SAMPLELB1 (make-lego 1 'blue 30))
(define SAMPLELB2 (make-bigger (make-lego 1 'blue 30)
                               (make-lego 2 'red 30)
                               (make-lego 3 'red 30)))
(define SAMPLELB3 (make-bigger (make-lego 1 'blue 30)
                               (make-bigger (make-lego 2 'red 30)
                                            (make-lego 4 'green 15)
                                            (make-lego 5 'green 15))
                               (make-bigger (make-lego 3 'red 30)
                                            (make-lego 6 'green 15)
                                            (make-lego 7 'green 15))))
(define SAMPLELB4 (make-bigger (make-lego 1 'blue 30)
                               (make-bigger (make-lego 2 'red 30)
                                            (make-lego 4 'green 15)
                                            (make-lego 5 'green 15))
                               (make-lego 7 'purple 30)))
(define SAMPLELB5 (make-bigger (make-lego 1 'blue 30)
                               (make-lego 7 'purple 30)
                               (make-bigger (make-lego 2 'red 30)
                                            (make-lego 4 'green 15)
                                            (make-lego 5 'green 15))))
; Problem 2

;; count-bricks : LegoBldg -> Number
;; counts the total number of legos in the building

(check-expect (count-bricks SAMPLELB1) 1)
(check-expect (count-bricks SAMPLELB2) 3)
(check-expect (count-bricks SAMPLELB3) 7)
(check-expect (count-bricks SAMPLELB4) 5)
(check-expect (count-bricks SAMPLELB5) 5)

(define (count-bricks albldg)
  (cond [(lego? albldg) 1]
        [(bigger? albldg) (+ 1 (count-bricks (bigger-left albldg))
                 (count-bricks (bigger-right albldg)))]))

; Problem 3

;; how-high : LegoBldg -> Number
;; identifies the height of the lego building

(check-expect (how-high SAMPLELB1) 10)
(check-expect (how-high SAMPLELB2) 20)
(check-expect (how-high SAMPLELB3) 30)
(check-expect (how-high SAMPLELB4) 30)
(check-expect (how-high SAMPLELB5) 30)

(define (how-high albldg)
  (cond [(lego? albldg) 10]
        [(bigger? albldg) (max (+ 10 (how-high (bigger-left albldg)))
                                  (+ 10 (how-high (bigger-right albldg))))]))

; Problem 4

;; contains-colored-brick? : LegoBldg -> Boolean
;; takes a lego building and a color and
;; determines whether the building contains a lego brick of the given color.

(check-expect (contains-colored-brick? SAMPLELB1 'blue) true)
(check-expect (contains-colored-brick? SAMPLELB1 'red) false)
(check-expect (contains-colored-brick? SAMPLELB2 'red) true)
(check-expect (contains-colored-brick? SAMPLELB2 'green) false)
(check-expect (contains-colored-brick? SAMPLELB3 'green) true)
(check-expect (contains-colored-brick? SAMPLELB3 'yellow) false)
(check-expect (contains-colored-brick? SAMPLELB4 'green) true)
(check-expect (contains-colored-brick? SAMPLELB4 'yellow) false)
(check-expect (contains-colored-brick? SAMPLELB5 'green) true)
(check-expect (contains-colored-brick? SAMPLELB5 'yellow) false)

(define (contains-colored-brick? albldg acolor)
  (cond [(lego? albldg) 
         (symbol=? (lego-color albldg) acolor)]
        [(bigger? albldg) 
         (or (contains-colored-brick? (bigger-right albldg) acolor)
             (contains-colored-brick? (bigger-left albldg) acolor))]))


; Problem 5

;; A MaybeLego is one of:
;; - false
;; - Lego

;; find-colored-brick? : LegoBldg Color -> MaybeLego
;; finds a lego in the lego building matching the given color
;; returns false if the lego building doesn't contain the color

(check-expect (find-colored-brick? SAMPLELB1 'blue) (make-lego 1 'blue 30))
(check-expect (find-colored-brick? SAMPLELB1 'red) false)
(check-expect (find-colored-brick? SAMPLELB2 'red) (make-lego 2 'red 30))
(check-expect (find-colored-brick? SAMPLELB2 'green) false)
(check-expect (find-colored-brick? SAMPLELB3 'blue) (make-lego 1 'blue 30))
(check-expect (find-colored-brick? SAMPLELB3 'yellow) false)
(check-expect (find-colored-brick? SAMPLELB4 'purple) (make-lego 7 'purple 30))
(check-expect (find-colored-brick? SAMPLELB4 'yellow) false)
(check-expect (find-colored-brick? SAMPLELB5 'purple) (make-lego 7 'purple 30))
(check-expect (find-colored-brick? SAMPLELB5 'yellow) false)

(define (find-colored-brick? albldg acolor)
  (cond [(lego? albldg) 
         (if (symbol=? (lego-color albldg) acolor)
             albldg 
             false)]
        [(bigger? albldg) 
         (if (symbol=? (lego-color (bigger-lego albldg)) acolor)
             (bigger-lego albldg)
             (look-both albldg acolor))]))

;; LegoBldg Symbol -> MaybeLego
;; checks both branches for the "colored brick"
;;   (explained in the previous signature)

(check-expect (look-both SAMPLELB2 'blue) false)
(check-expect (look-both SAMPLELB2 'purple) false)
(check-expect (look-both SAMPLELB3 'blue) false)
(check-expect (look-both SAMPLELB3 'purple) false)
(check-expect (look-both SAMPLELB4 'blue) false)
(check-expect (look-both SAMPLELB4 'purple) (make-lego 7 'purple 30))
(check-expect (look-both SAMPLELB5 'blue) false)
(check-expect (look-both SAMPLELB5 'purple) (make-lego 7 'purple 30))

(define (look-both albldg acolor)
  (local ((define result (find-colored-brick? (bigger-left albldg) acolor)))
    (if (lego? result) result
        (find-colored-brick? (bigger-right albldg) acolor))))
          
; Problem 6

;; lb->image : LegoBldg -> Image
;; takes a lego building and produces an image of the building

(check-expect (lb->image SAMPLELB1) (rectangle 30 10 'solid 'blue))
(check-expect (lb->image SAMPLELB2) (above (rectangle 30 10 'solid 'blue)
                                           (beside/align
                                            "top"
                                            (rectangle 30 10 'solid 'red)
                                            (rectangle 30 10 'solid 'red))))
(check-expect (lb->image SAMPLELB3) (above
                                     (rectangle 30 10 'solid 'blue)
                                     (beside/align
                                      "top"
                                      (above
                                       (rectangle 30 10 'solid 'red)
                                       (beside/align
                                        "top"
                                        (rectangle 15 10 'solid 'green)
                                        (rectangle 15 10 'solid 'green)))
                                      (above
                                       (rectangle 30 10 'solid 'red)
                                       (beside/align
                                        "top"
                                        (rectangle 15 10 'solid 'green)
                                        (rectangle 15 10 'solid 'green))))))
(check-expect (lb->image SAMPLELB4) (above
                                     (rectangle 30 10 'solid 'blue)
                                     (beside/align
                                      "top"
                                      (above (rectangle 30 10 'solid 'red)
                                             (beside/align
                                              "top"
                                              (rectangle 15 10 'solid 'green)
                                              (rectangle 15 10 'solid 'green)))
                                      (rectangle 30 10 'solid 'purple))))


(check-expect (lb->image SAMPLELB5) (above
                                     (rectangle 30 10 'solid 'blue)
                                     (beside/align
                                      "top"
                                      (rectangle 30 10 'solid 'purple)
                                      (above
                                       (rectangle 30 10 'solid 'red)
                                       (beside/align
                                        "top"
                                        (rectangle 15 10 'solid 'green)
                                        (rectangle 15 10 'solid 'green))))))

(define (lb->image albldg)
  (local (;; lego->image : Lego -> Image
          ;; takes a lego and produces an image of the lego
          (define (lego->image l)
            (rectangle (lego-width l) 10 'solid (lego-color l))))
    (cond [(lego? albldg) (lego->image albldg)]
          [(bigger? albldg) 
           (above (lego->image (bigger-lego albldg))
                  (beside/align "top"
                                (lb->image (bigger-left albldg))
                                (lb->image (bigger-right albldg))))])))