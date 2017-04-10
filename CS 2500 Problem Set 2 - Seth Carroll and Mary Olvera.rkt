;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Seth Carroll and Mary Olvera - CS 2500 Problem Set 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Problem 1

; Number String Number -> Image
; overlays square whose side length is sq-len
; with star whose side length is side-len-p and color is COLOR
; given:
; 20 for side-len-p
; "blue" for COLOR"
; 50 for sq-len
; expected:
; (overlay (star-polygon 20 5 2 "solid" "blue")
;           (square 50 "outline" "black"))

(define (favorite-star side-len-p COLOR sq-len)
  (overlay (star-polygon side-len-p 5 2 "solid" COLOR)
           (square sq-len "outline" "black")))

(check-expect (favorite-star 20 "blue" 50)
              (overlay (star-polygon 20 5 2 "solid" "blue")
                       (square 50 "outline" "black")))

;; Problem 2

(define WIDTH-OF-WORLD 400)
(define HEIGHT-OF-WORLD 300)
(define WHEEL-RADIUS 10)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WHEEL (circle WHEEL-RADIUS "solid" "dark gray"))
(define SPACE (rectangle (* 4 WHEEL-RADIUS)
                         (* .001 WHEEL-RADIUS) "outline" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define CAR
  (overlay/offset
   (overlay/offset
   BOTH-WHEELS
   0 (- (* 1.2 WHEEL-RADIUS))
   (rectangle (* 10 WHEEL-RADIUS) (* 2 WHEEL-RADIUS) "solid" "sky blue"))
   0 (- (* 2 WHEEL-RADIUS))
   (rectangle (* 7 WHEEL-RADIUS) (* 1.5 WHEEL-RADIUS) "solid" "sky blue")))

(define TREE
  (underlay/xy (rectangle WHEEL-RADIUS (* 8 WHEEL-RADIUS) "solid" "brown")
               (- (* 1.5 WHEEL-RADIUS)) (- WHEEL-RADIUS)
               (circle (* 2 WHEEL-RADIUS) "solid" "green")))
             

(define BG (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))
(define Y-CAR (- HEIGHT-OF-WORLD (/ (image-height CAR) 2) 1))
(define Y-TREE (- HEIGHT-OF-WORLD (/ (image-height TREE) 2) 1))
(define END-BG (- WIDTH-OF-WORLD (/ (image-width CAR) 2)))


; Number -> Image
; draws CAR onto the background BG
; example:
; given: 20 for WS
; expect (place-images(list CAR TREE)
;  (list (make-posn 20 Y-CAR)
;  (make-posn (* .75 WIDTH-OF-WORLD) Y-TREE))
;        BG))

(define (draw-car WS)
  (place-images
   (list CAR
         TREE)
   (list (make-posn WS Y-CAR)
         (make-posn (* .75 WIDTH-OF-WORLD) Y-TREE))
         BG))
               
(check-expect (draw-car 20)
              (place-images
               (list CAR
                     TREE)
                (list (make-posn 20 Y-CAR)
                (make-posn (* .75 WIDTH-OF-WORLD) Y-TREE))
                BG))


; Number -> Number
; increases WS position of car at each tick
; example:
; given: 20, expect 23

(define (next-car WS)
  (if (>= WS END-BG)
      WS
      (+ WS 3)))

(check-expect (next-car 20) 23)

(define (main WS)
  (big-bang WS
            [to-draw draw-car]
            [on-tick next-car]))


; Problem 3

(define SINEWIDTH-OF-WORLD 400)
(define SINEHEIGHT-OF-WORLD 300)
(define SINEWHEEL-RADIUS 10)
(define SINEWHEEL-DISTANCE (* SINEWHEEL-RADIUS 5))

(define SINEWHEEL (circle SINEWHEEL-RADIUS "solid" "dark gray"))
(define SINESPACE (rectangle (* 4 SINEWHEEL-RADIUS)
                         (* .001 SINEWHEEL-RADIUS) "outline" "white"))
(define SINEBOTH-WHEELS (beside SINEWHEEL SINESPACE SINEWHEEL))
(define SINECAR
  (overlay/offset
   (overlay/offset
   SINEBOTH-WHEELS
   0 (- (* 1.2 SINEWHEEL-RADIUS))
   (rectangle (* 10 SINEWHEEL-RADIUS) (* 2 SINEWHEEL-RADIUS) "solid" "sky blue"))
   0 (- (* 2 SINEWHEEL-RADIUS))
   (rectangle (* 7 SINEWHEEL-RADIUS) (* 1.5 SINEWHEEL-RADIUS) "solid" "sky blue")))
             
(define SINEBG (empty-scene SINEWIDTH-OF-WORLD SINEHEIGHT-OF-WORLD))
(define SINEY-CAR (* 2/3 SINEHEIGHT-OF-WORLD))
(define SINEEND-BG (- SINEWIDTH-OF-WORLD (/ (image-width SINECAR) 2)))


; Number -> Image
; draws SINECAR onto the background BG
; example:
; given: 20 for SINEWS
; expect (place-images(list SINECAR)
;  (list (make-posn SINEWS (+ (* .5 SINEHEIGHT-OF-WORLD) (* 20 (sin SINEWS)))))
;        SINEBG)

(define (draw-sinecar SINEWS)
  (place-images
   (list SINECAR)
   (list (make-posn SINEWS (+ (* .5 SINEHEIGHT-OF-WORLD) (* 20 (sin SINEWS)))))
         SINEBG))
               
(check-expect (draw-sinecar 20)
              (place-images
               (list SINECAR)
                (list (make-posn 20 (+ (* .5 SINEHEIGHT-OF-WORLD) (* 20 (sin 20)))))
                SINEBG))


; Number -> Number
; increases SINEWS position of car at each tick
; example:
; given: 20, expect 23

(define (next-sinecar SINEWS)
  (if (>= SINEWS SINEEND-BG)
      SINEWS
      (+ SINEWS .3)))

(check-expect (next-sinecar 20) 20.3)

(define (main2 SINEWS)
  (big-bang SINEWS
            [to-draw draw-sinecar]
            [on-tick next-sinecar]))

; Problem 4

; Number -> String
; Input number value for percent humidity, outputs interval of comfort
; example:
; given: 70, expect "humid"
; given: 15, expect "dry"
; given: 40, expect "comfortable"
(define (comfortable c)
  (cond [(> c 65) "humid"]
        [(< c 20) "dry"]
        [(and (>= c 20) (<= c 65)) "comfortable"]))

(check-expect (comfortable 40) "comfortable")