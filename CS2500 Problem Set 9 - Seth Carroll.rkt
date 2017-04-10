;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS2500 Problem Set 9 - Seth Carroll|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set 9 - Seth T. Carroll (assigned partner dropped, no alternative partner assigned)


;;Problem 1

;; add1* : Lon -> Lon
;; add 1 to each number on l
(check-expect (add1* '()) '())
(check-expect (add1* (list 5 6)) (list 6 7))

(define (add1* l)
  (adder l 1))


;; plus5 : Lon -> Lon
;; adds 5 to each number on l
(check-expect (plus5 '()) '())
(check-expect (plus5 (list 5 10)) (list 10 15))

(define (plus5 l)
  (adder l 5))


;; adder : Lon Number -> Lon
;; adds Number to each number on l
(check-expect (adder '() 5) '())
(check-expect (adder (list 5 6) 1) (list 6 7))
(check-expect (adder (list 5 10) 5) (list 10 15))

(define (adder l n)
  (cond
    [(empty? l) '()]
    [else
     (cons (+ (first l) n)
           (adder (rest l) n))]))

;; sub2 : Lon -> Lon
;; subtracts 2 from each number on l
(check-expect (sub2 '()) '())
(check-expect (sub2 (list 8 10)) (list 6 8))

(define (sub2 l)
  (adder l -2))


;; Problem 2

;; A [Maybe X] is one of: 
;; – #false 
;; – X

;; A [Maybe String] is one of: 
;; – #false 
;; – String
;; Interpretation : a [Maybe String] is an ITEM that is either a String
;;  or returns false

; A [Maybe [List-of String]] is one of: 
; – #false 
; – [List-of String]
;; Interpretation : a [Maybe [List-of String]] is an ITEM
;;  that is either a [List-of String] or returns false

;; A [List-of [Maybe String]] is one of: 
;; – '() 
;; – (cons [Maybe String] [List-of [Maybe String]]


;; String [List-of String] -> [Maybe [List-of String]]
;; returns the remainder of the list los if it contains s 
;; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d")) (list "d"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)

(define (occurs s los)
  (cond [(empty? los) false]
        [(string=? s (first los)) (rest los)]
        [else (occurs s (rest los))]))