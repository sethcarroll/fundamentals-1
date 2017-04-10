;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |CS 2500 Problem Set 3 - Seth Carroll & Mary Olvera|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; CS 2500 Problem Set 3 - Seth Carroll & Mary Olvera

;; Problem 1

;; A Ninja Turtle is one of:
;; Lenonardo
;; Michaelangelo
;; Raphael
;; Donatello

;(define NINJA-TURTLE ...)

;; A percentage is a real number between 0 and 100
;; Example: 72





;; Problem 2


; Price -> Number
; computes the amount of tax charged for price p

(define LOW 1000)
(define LUXURY 10000)


(check-expect (sales-tax 500) 0)
(check-expect (sales-tax 2000) (* 0.05 2000))
(check-expect (sales-tax 12000) (* 0.08 12000))

(define (sales-tax p)
  (cond
    [(< p LOW) 0]
    [(and (<= LOW p) (< p LUXURY)) (* 0.05 p)]
    [(> p LUXURY) (* 0.08 p)]))





;; Problem 3

;; A posn is a coordinate point (x y) on a Cartesian plane
;; (make-posn 3 4) is a coordinate point with x-value 3 and y-value 4
(define x (make-posn 4 3))

;; posn -> Number
;; determines distance from the origin (0, 0) based on values from posn
;; Given: (make-posn 3 4) Expect: 5

(check-expect (manhattan-distance x) 5)
  
(define (manhattan-distance a-posn)
(sqrt
    (+ (sqr (posn-x a-posn))
       (sqr (posn-y a-posn)))))

(manhattan-distance x)




;; Problem 4


(define C-RADIUS 5)
(define BG (empty-scene 200 200))

;; 2 Posns -> Image
;; Takes 2 positions and creates an image of a red and blue dot at given
;; positions, a green dot 10% of the distance between the red and blue dot
;; away from the blue dot, and a line connecting the red and blue dots

#;(define (color-dots red-posn blue-posn)
  (place-images (...
                red-posn blue-posn...)))

(check-expect (color-dots (make-posn 10 10) (make-posn 180 180))
               (add-line (place-images
                (list (circle C-RADIUS 'solid 'red)
                      (circle C-RADIUS 'solid 'blue)
                      (circle C-RADIUS 'solid 'green))
                (list (make-posn 10 10)
                      (make-posn 180 180)
                      (make-posn
                       (- 180 (* .1 170))
                       (- 180 (* .1 170))))
                BG) 10 10 180 180 'black))

(define (color-dots red-posn blue-posn)
    (add-line
     (place-images
   (list (circle C-RADIUS 'solid 'red)
         (circle C-RADIUS 'solid 'blue)
         (circle C-RADIUS 'solid 'green))
   (list (make-posn (posn-x red-posn) (posn-y red-posn))
         (make-posn (posn-x blue-posn) (posn-y blue-posn))
         (make-posn
          (- (posn-x blue-posn) (* .1 (- (posn-x blue-posn) (posn-x red-posn))))
          (- (posn-y blue-posn) (* .1 (- (posn-y blue-posn) (posn-y red-posn))))))
   BG)
     (posn-x red-posn) (posn-y red-posn)
     (posn-x blue-posn) (posn-y blue-posn)
     'black))