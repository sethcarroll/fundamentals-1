;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Seth Carroll - CS 2500 Problem Set 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CS 2500 Problem Set 1 - Seth T. Carroll



;Problem 1

;;constants

(define tuition 44620)
(define room-and-board 14472)
(define semester-number 2)
(define class-number 4)
(define lecture-number (* 3 13))

"Problem 1"
;; expressions

(/ (+ tuition room-and-board) semester-number)

(/ tuition (* semester-number class-number lecture-number))



;Problem 2

;; 100 | 110 | 120 | 130 | 140 | 150 | 160 | 170 | 180 | 190 | 200 |
;;  0  |  1  |  1  |  2  |  3  |  3  |  4  |  5  |  5  |  6  |  7  |

;; y = (x - 95) / 15
;; x = blood sugar. y = # of insulin units. Always round down to nearest whole number.
;; y = (290 - 95) / 15 = (195) / 15 = 13


"Problem 2"
;; expressions

(define (blood-sugar x) (/ (- x 95) 15))

(floor (blood-sugar 100))
(floor (blood-sugar 110))
(floor (blood-sugar 120))
(floor (blood-sugar 130))
(floor (blood-sugar 140))
(floor (blood-sugar 150))
(floor (blood-sugar 160))
(floor (blood-sugar 170))
(floor (blood-sugar 180))
(floor (blood-sugar 190))
(floor (blood-sugar 200))
(floor (blood-sugar 290))



;Problem 3

;; d = distance traveled, t = time
;; (+ (* a t t) b) -> d(t) = a t^2 + b
;; (13) = a (0)^2 + b -> b = 13
;; 96 = a (2)^2 + 13 -> a = (83/4)
;; d(t) = (83/4) t^2 + 13 -> (+ (* (/ 83 4) t t) 13)

"Problem 3"
;; expressions
(define (distance t) (+ (* (/ 83 4) t t) 13))

(distance 0)
(distance 1)
(distance 2)
(distance 3)
(distance 4)
(distance 6)



;Problem 4

;; m = money, b = number of bins
;; m = 4 b + 10 -> (+ (* 4 b) 10)

"Problem 4"
;; expressions
(define (bins b) (+ (* 4 b) 10))

(bins 0)
(bins 5)



;Problem 5

"Problem 5"
;;expressions
(define (salutations name) (string-append "Dear " name ":"))

(salutations "Seth")



;Problem 6

(require 2htdp/image)

"Problem 6"
(define (my-star color) (star 12 "solid" color))

(my-star "sky blue")
