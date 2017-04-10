;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |CS 2500 Problem Set 4 - Seth Carroll & Mary Olvera|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Problem Set 4 Seth Carroll and Mary Olvera

(require 2htdp/image)
(require 2htdp/universe)

; Number 1
; 1.1
(define-struct lecture-hall (number capacity))
(define-struct automobile (year make model))
(define-struct football-player (name position number))
(define-struct shirt (material size color))

#|
(make-lecture-hall) (lecture-hall?) (lecture-hall-number)
(lecture-hall-capacity)
(make-automobile) (automobile?) (automobile-year) (automobile-make)
(automobile-model)
(make-football-player) (football-player?) (football-player-name)
(football-player-position) (football-player-number)
(make-shirt) (shirt?) (shirt-material) (shirt-size) (shirt-color)


 lecture-hall:
 +-------+---------+
 | number|capacity |
 |-------|-------- |
 |   5   |   72    |
 +-------+---------+

 automobile:
 +-------+---------+-------+
 | year  |  make   | model |
 |-------|---------|-------|
 | 2015  |  Audi   |  C500 |
 +-------+---------+-------+

 football-player:
 +-------+-----------+-------+
 |  name | position  | number|
 |-------|-----------|-------|
 | Brady |quarterback|  12   |       
 +-------+-----------+-------+

 shirt:
 +--------+---------+-------+
 |material|  size   | color |
 |--------|---------|-------|
 | cotton |   XL    |  blue |
 +--------+---------+-------+

|#

; 1.2

; (define-struct lecture-hall (number capacity))
; A lecture-hall is a structure:
; (make-lecture-hall Number Number)
; A number is a classroom label
; A capacity is the number of people a lecture-hall can hold

; (define-struct automobile (year make model))
; A automobile is a (make-automobile Number String String)

; (define-struct football-player (name position number))
; A football-player is a (make-football-player String Number Number)
; A name is a football player's last name
; A position is a player's position on the team
; A number is a player's jersey number

; (define-struct shirt (material size color))
; A shirt is a (make-shirt String Number String)

; 1.3

(define (lhtemp alh)
  ( ...(lecture-hall-number alh) ...(lecture-hall-capacity alh) ... ))
(define (atemp aa)
  ( ...(automobile-year aa) ...(automobile-make aa) ...
       (automobile-model aa) ...))
(define (fptemp afp)
  ( ...(football-player-name afp) ...(football-player-position afp)
       ...(football-player-number afp)))
(define (stemp as)
  (... (shirt-material as) ...(shirt-size as) ...(shirt-color as) ...))

; Problem 2

(define-struct time (hours minutes))
; A Time is a structure:
;    (make-time Number Number)
; interpretation: (make-time h m) is the time  
; expressed in hours, and minutes
; Constraints:
; – hours is always between 0 and 11
; – minutes is always between 0 and 59


; 2.1

;; Time -> Time
;; Adds one minute to the given time
(check-expect (tock (make-time 10 48)) (make-time 10 49))
(check-expect (tock (make-time 10 59)) (make-time 11 00))
(check-expect (tock  (make-time 11 59)) (make-time 0 00))

#;(define (tock-temp atime)
  (cond [(... (time-minutes atime)
          ... (time-hours atime)
          ...)]
        [(...)]))

(define (tock atime)
  (cond [(equal? atime (make-time 11 59)) (make-time 0 00)]
        [(equal? (time-minutes atime) 59)
         (make-time (+ 1 (time-hours atime)) 00)]
        [else (make-time (time-hours atime) (+ 1 (time-minutes atime)))]))



; Time -> image
; converts atime into an image with the strings hours and minutes separated by
; a colon
(check-expect (time->text (make-time 0 8)) (text "00:08" 24 'black))
(check-expect (time->text (make-time 10 5)) (text "10:05" 24 'black))
(check-expect (time->text (make-time 1 20)) (text "01:20" 24 'black))
(check-expect (time->text (make-time 10 10)) (text "10:10" 24 'black))
              
(define (time->text-temp atime)
  (... (time-hours atime)... (time-minutes atime) ...))

(define (time->text atime)
  (text (cond [(and (<= (time-minutes atime) 9) (<= (time-hours atime) 9))
         (string-append "0" (number->string (time-hours atime)) ":0"
                       (number->string (time-minutes atime)))]
        [(<= (time-minutes atime) 9)
         (string-append (number->string (time-hours atime)) ":0"
                       (number->string (time-minutes atime)))]
        [(<= (time-hours atime) 9)
         (string-append "0" (number->string (time-hours atime)) ":"
                       (number->string (time-minutes atime)))]
        [else
          (string-append (number->string (time-hours atime)) ":"
                       (number->string (time-minutes atime)))]) 24 'black))


(define (main-time atime)
  (big-bang atime
            [to-draw time->text]
            [on-tick tock 60]))
        



;; Problem 3

; 3.1

; a ball position is a (Number Number)
; interpretation: first Number is a positive integer representing the
; x-coordinate second Number is a positive integer representing the y-coordinate
; example: (150 150)



; 3.2

; SPEED is a Number
; interpretation: Number refers to a movement of a number of pixels per tick
(define SPEED 10)

; a direction is one of:
; "left"
; "right"
; "up"
; "down"



;3.3

; ; A ball is a structure:
; (make-ball ballx bally direction)
; interpretation: a ballx is a ball position x-coordinate
;  a bally is a ball position y-coordinate
(define-struct ball (ballx bally direction))



; 3.4

; Ball -> Ball
; creates a ball and shifts it 10 pixels in the given direction
(check-expect (ball-next (make-ball 150 150 "left"))
              (make-ball 140 150 "left"))
(check-expect (ball-next (make-ball 150 150 "right"))
              (make-ball 160 150 "right"))
(check-expect (ball-next (make-ball 150 150 "up"))
              (make-ball 150 140 "up"))
(check-expect (ball-next (make-ball 150 150 "down"))
              (make-ball 150 160 "down"))

#;(define (ball-temp aball)
  (... (ball-ballx aball)
       ... (ball-bally aball)
       ... (ball-direction aball)))

(define (ball-next aball)
 (cond [(equal? (ball-direction aball) "left")
               (make-ball
                (- (ball-ballx aball) SPEED)
                (ball-bally aball)
                (ball-direction aball))]
       [(equal? (ball-direction aball) "right")
               (make-ball
                (+ (ball-ballx aball) SPEED)
                (ball-bally aball)
                (ball-direction aball))]
       [(equal? (ball-direction aball) "up")
               (make-ball
                (ball-ballx aball)
                (- (ball-bally aball) SPEED)
                (ball-direction aball))]
       [(equal? (ball-direction aball) "down")
               (make-ball
                (ball-ballx aball)
                (+ (ball-bally aball) SPEED)
                (ball-direction aball))]))
    
    


; 3.5

; ball -> Image
; Consumes a ball and creates a solid red circle with radius RADIUS
; on a background BG at a position of (ballx, bally)
(check-expect (ball-image (make-ball 150 150 "left"))
              (place-image (circle RADIUS 'solid 'red)
                           150 150
                          BG))
                          

(define BG (empty-scene 300 300))
(define RADIUS 10)

(define (ball-image aball)
  (place-image (circle RADIUS 'solid 'red) (ball-ballx aball) (ball-bally aball)
                          BG))
                   


; 3.6

; ball keyevent -> ball
; consumes a ball and a keyevent and produces a ball with ballx and bally of the
; initial ball, and direction based on keyevent

(check-expect (ball-change (make-ball 150 150 "left") "right")
              (make-ball 150 150 "right"))
(check-expect (ball-change (make-ball 150 150 "right") "left")
              (make-ball 150 150 "left"))
(check-expect (ball-change (make-ball 150 150 "left") "up")
              (make-ball 150 150 "up"))
(check-expect (ball-change (make-ball 150 150 "left") "down")
              (make-ball 150 150 "down"))

#;(define (ball-change-temp aball akeyevent)
  (... (ball-ballx aball)... (ball-bally aball)... (ball-direction aball)
  ... akeyevent...))

(define (ball-change aball akeyevent)
  (make-ball (ball-ballx aball) (ball-bally aball)
             (cond [(key=? akeyevent "left") "left"]
                   [(key=? akeyevent "right") "right"]
                   [(key=? akeyevent "up") "up"]
                   [(key=? akeyevent "down") "down"])))

; 3.7

;ball -> ball
(define (main-ball aball)
  (big-bang aball
            [to-draw ball-image]
            [on-key ball-change]
            [on-tick ball-next]))
