;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |CS2500 Problem Set 5 - Seth Carroll & Evan Sevieri|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;CS 2500 Problem Set 5 -- Seth Carroll & Evan Sevieri

(require 2htdp/image)
(require 2htdp/universe)





;;Problem 1

; A movie is one of:
;(make-regularmovie String Number Number)
;(make-classicmovie String Number)
(define-struct regularmovie [productid baseprice years])
(define-struct classicmovie [productid price])
; baseprice is the starting price of the movie at release
; years is the number of years the movie has been in the stores collection

; Movie -> Number
; Consumes a movie and outputs a current price from its baseprice and years

(check-expect (current-price (make-regularmovie "123-45-ABC" 5 3)) 4.475)
(check-expect (current-price (make-regularmovie "123-45-ABC" 5 18)) 2)
(check-expect (current-price (make-classicmovie "123-45-ABC" 10)) 10)

#;(define (movie-temp amovie)
  (cond [(classicmovie? amovie) ...(classicmovie-productid amovie)
                                ...(classicmovie-price amovie)]
        [(regularmovie? amovie) ...(regularmovie-productid amovie)
                                ...(regularmovie-baseprice amovie)
                                ...(regularmovie-years amovie)]))

(define (current-price amovie)
  (cond [(classicmovie? amovie) (classicmovie-price amovie)]
        [(regularmovie? amovie) (if (<= (markdown amovie) 2) 2
                                    (markdown amovie))]))

(check-expect (markdown (make-regularmovie "123-45-ABC" 5 3)) 4.475)
(define (markdown amovie)
  (- (regularmovie-baseprice amovie)
     (* (* .035 (regularmovie-baseprice amovie))
        (regularmovie-years amovie))))





;Problem 2

;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle
 
(define-struct circl [x y r outline c])
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;;   r the radius, outline whether it's outlined or solid,
;;   and c its color
 
(define-struct squar [x y size outline c])
;; A Square is a (make-squar Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of a square,
;;   size is the length of a side, outline whether it's outlined or solid,
;;   and c its color
 
(define-struct recta [x y width height outline c])
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the recatngle, width the
;;    length of the width, height the length of the height, outline whether
;;    it's outlined or solid, and c it's color

 

#;(define (shape-tmpl s)
  (cond
    [(circl? s) ... (circl-x s)
                ... (circl-y s)
                ... (circl-r s)
                ... (circl-outline s)
                ... (circl-c s)]
    [(squar? s) ... (squar-x s)
                ... (squar-y s)
                ... (squar-size s)
                ... (squar-outline s)
                ... (squar-c s)]
    [(recta? s) ... (recta-x s)
                ... (recta-y s)
                ... (recta-width s)
                ... (recta-height s)
                ... (recta-outline s)
                ... (recta-c s)]))

(check-expect (shape-shift-x (make-circl 4 5 3 #f 'red) 3)
              (make-circl 7 5 3 #f 'red))
(check-expect (shape-shift-x (make-squar 4 5 3 #f 'red) 3)
              (make-squar 7 5 3 #f 'red))
(check-expect (shape-shift-x (make-recta 4 5 3 4 #f 'red) 3)
              (make-recta 7 5 3 4 #f 'red)) 

;Shape Number -> Shape
;Shifts shape delta units along x axis
(define (shape-shift-x s delta)
  (cond
    [(circl? s) (make-circl (+ delta (circl-x s))
                            (circl-y s)
                            (circl-r s)
                            (circl-outline s)
                            (circl-c s))]
    [(squar? s) (make-squar (+ delta (squar-x s))
                            (squar-y s)
                            (squar-size s)
                            (squar-outline s)
                            (squar-c s))]
    [(recta? s) (make-recta (+ delta (recta-x s))
                            (recta-y s)
                            (recta-width s)
                            (recta-height s)
                            (recta-outline s)
                            (recta-c s))]))

(check-expect (shape-in? (make-circl 4 5 3 #f 'red) (make-posn 4 5))
              "Inside the Shape!")
(check-expect (shape-in? (make-circl 4 5 3 #f 'red) (make-posn 0 5))
              "Outside the Shape!")
(check-expect (shape-in? (make-squar 4 5 3 #f 'red) (make-posn 4 5))
              "Inside the Shape!")
(check-expect (shape-in? (make-squar 4 5 3 #f 'red) (make-posn 0 5))
              "Outside the Shape!")
(check-expect (shape-in? (make-recta 4 5 3 4 #f 'red) (make-posn 4 5))
              "Inside the Shape!")
(check-expect (shape-in? (make-recta 4 5 3 4 #f 'red) (make-posn 0 5))
              "Outside the Shape!")

; Shape Posn -> String
;Checks whether the posn is in the shape or on its border
(define (shape-in? sh pt)
  (cond
    [(circl? sh) (if (<= (sqrt (+ (sqr (- (posn-x pt) (circl-x sh)))
                                  (sqr (- (posn-y pt) (circl-y sh)))))
                         (circl-r sh))
                    "Inside the Shape!"
                    "Outside the Shape!")]
    [(squar? sh) (if (and (<= (abs (- (posn-x pt) (squar-x sh)))
                                  (* 0.5 (squar-size sh)))
                         (<= (abs (- (posn-y pt) (squar-y sh)))
                                  (* 0.5 (squar-size sh))))
                    "Inside the Shape!"
                    "Outside the Shape!")]

    [(recta? sh) (if (and (<= (abs (- (posn-x pt) (recta-x sh)))
                                  (* 0.5 (recta-width sh)))
                         (<= (abs (- (posn-y pt) (recta-y sh)))
                                  (* 0.5 (recta-height sh))))
                    "Inside the Shape!"
                    "Outside the Shape!")]))


; Shape Image -> Image
; Consumes and draws a shape on a consumed image/scene

(check-expect (shape-draw (make-circl 130 130 5 true 'red)
            (empty-scene 300 300))
            (place-image (circle 5 'outline 'red)
                         130
                         130
                         (empty-scene 300 300)))
(check-expect (shape-draw (make-squar 130 130 5 #f 'red)
            (empty-scene 300 300))
            (place-image (square 5 'solid 'red)
                         130
                         130
                         (empty-scene 300 300)))
(check-expect (shape-draw (make-recta 130 130 5 4 true 'red)
            (empty-scene 300 300))
            (place-image (rectangle 5 4 'outline 'red)
                         130
                         130
                         (empty-scene 300 300)))

(define (shape-draw sh sc)
  (cond [(circl? sh)
         (place-image (circle (circl-r sh)
                              (if (circl-outline sh)
                                  'outline
                                  'solid) (circl-c sh))
                                   (circl-x sh)
                                   (circl-y sh)
                                   sc)]
    [(squar? sh) (place-image (square (squar-size sh)
                              (if (squar-outline sh)
                                  'outline
                                  'solid) (squar-c sh))
                                   (squar-x sh)
                                   (squar-y sh)
                                   sc)]
    [(recta? sh) (place-image (rectangle (recta-width sh)
                                         (recta-height sh)
                                         (if (recta-outline sh)
                                         'outline
                                         'solid) (recta-c sh))
                                                 (recta-x sh)
                                                 (recta-y sh)
                                                 sc)]))





;Problem 3

;; A lop is a List of passwords
;; A password is a String with a length of 6-10 (at least 6, less than 11)
;;     characters long

;; lop -> boolean
;; Consumes a list of passwords and checks if all passwords are 6-10
;;     characters long

(check-expect (passwords-6-11?
               (list "password"
                    "123456"
                    "87654321"))
              true)
(check-expect (passwords-6-11?
               (list "lol"
                    "12345678910"))
              false)

#;(define (password-temp alop)
  (cond [(empty? alop) ...]
        [else
         (... (first alop) ...
              ... (password-temp (rest alop)))]))
                    
(define (passwords-6-11? alop)
  (cond [(empty? alop) true]
        [else (and (and (>= (string-length (first alop)) 6)
               (< (string-length (first alop)) 11))
          (passwords-6-11?(rest alop)))]))

;; lop Number Number -> boolean
;; Consumes a list of passwords and checks if all passwords are between the
;;     min and max lengths


(check-expect (passwords-ok?
               (list "password"
                    "123456"
                    "87654321") 6 10)
              true)
(check-expect (passwords-ok?
               (list "lol"
                    "12345678910") 6 10)
              false)

(define (passwords-ok? alop min max)
  (cond [(empty? alop) true]
        [ (and (>= (string-length (first alop)) min)
               (<= (string-length (first alop)) max))
          (passwords-ok? (rest alop) min max)]
        [else false]))

(define testlop (list "password"
                    "123456"
                    "87654321"))



;Problem 4

(define-struct ball [x y color])
;; Ball = (make-ball Number Number Color)
;; Color is one of 'red, 'yellow, 'blue, etc.


; An ListofBalls, lob is is one of:
; - empty
;(cons Ball lob)

#;(define (lob-temp alob)
  (cond
    [(empty? alob) ...]
    [else ... (first alob)
          (lob-temp (rest alob)) ...]))

(define lob1 (list (make-ball 100 100 'red)
                   (make-ball 200 200 'blue)
                   (make-ball 250 250 'pink)))

(define lob2 (list (make-ball 150 100 'blue)
                   (make-ball 150 200 'blue)))
(define lob3 '())



;ListofBalls -> Number
;Consumes a List of Balls and outputs the number of balls in the list
(check-expect (lob-length lob1) 3)
(check-expect (lob-length lob2) 2)
(check-expect (lob-length lob3) 0)

(define (lob-length alob)
  (cond
    [(empty? alob) 0]
    [else (add1 (lob-length (rest alob)))]))



;ListofBalls -> List of (ball-x balls)
;Consumes a List of Balls and produces a List of the Balls x coordinates
(check-expect (lob-x lob1) (list 100 200 250))
(check-expect (lob-x lob2) (list 150 150))
(check-expect (lob-x lob3) '())

(define (lob-x alob)
  (cond
    [(empty? alob) '()]
    [else (cons (ball-x (first alob))
                  (lob-x (rest alob)))]))


(define (ball-x-list aball)
  (list (ball-x aball)))


;; ListofBalls -> Image
;; Consumes a list of balls and creates an image representing all balls
;; as circles (radius=3) at their ball-x and ball-y coordinates
(define BG (empty-scene 300 300))
(define RADIUS 3)

(check-expect (lob-draw lob1)
              (place-image (circle RADIUS 'solid 'red)
                           100 100
                           (place-image (circle RADIUS 'solid 'blue)
                                        200 200
                                        (place-image
                                         (circle RADIUS 'solid 'pink)
                                         250 250
                                         BG))))

(define (lob-draw alob)
  (cond
    [(empty? alob) BG]
    [else (place-image (circle RADIUS 'solid (ball-color (first alob)))
                       (ball-x (first alob))
                       (ball-y (first alob))
                       (lob-draw (rest alob)))]))


;; ListofBalls -> ListofBalls
;; Consumes a ListofBalls and creates a list of all balls with
;; ball-x and ball-y cooridnates less than 300

(define lob4 (list (make-ball 100 100 'red)
                   (make-ball 400 400 'pink)
                   (make-ball 200 200 'blue)))

(check-expect (lob-filter lob4)
              (list (make-ball 100 100 'red)
                   (make-ball 200 200 'blue)))

(define (lob-filter alob)
  (cond
    [(empty? alob) '()]
    [(and (< (ball-x (first alob)) 300)
          (< (ball-y (first alob)) 300))
          (cons (first alob)
                  (lob-filter (rest alob)))]
    [else (lob-filter (rest alob))]))

;; ListofBalls Ball -> Boolean
;; Consumes a List of balls and a ball and checks whether the ball is contained
;; in the list

(check-expect (lob-member? lob4 (make-ball 100 100 'red)) true)
(check-expect (lob-member? lob4 (make-ball 100 100 'pink)) false)

(define (lob-member? alob b)
  (cond
    [(empty? alob) false]
    [else (or (equal? b (first alob))
              (lob-member? (rest alob) b))]))







;; Problem 5


(define-struct txt [content x y])
;; Txt = (make-txt String Number Number)
;; Represents the occurrence of the given text at the given location,
;; in computer-graphics coordinates.

(define TXT1 (make-txt "On your mark." 200 100))
(define TXT2 (make-txt "Get set." 200 200))
(define TXT3 (make-txt "Go!" 200 300))

#;(define (txt-temp atext)
  (... (txt-content atext) ...
       (txt-x atext)       ...
       (txt-y atext)       ...))


;; LoTxt is one of:
;; -- empty
;; -- (cons Txt LoTxt)

(define LOTXT1 (list TXT1
                     TXT2
                     TXT3))

#;(define (lotxt-temp alotxt)
  (cond
    [(empty? alotxt) ...]
    [else ... (first alotxt)
          ... (lotxt-temp (rest alotxt))]))
 
(define-struct world [image hidden])
;; World = (make-world Image LoTxt)
;; intepretation:
;;  The world's image represents the image that the audience can see.
;;  The world's list of Txt represents the yet-to-be-revealed elements.



;; World -> Image
;; Cosumes a world and creates the current image based on the world
(define SLIDE (empty-scene 400 400))
(define TEXTSIZE 15)
(define TEXTCOLOR 'red)
(define WORLD0 (make-world SLIDE LOTXT1))

#;(define (world-temp aworld)
  (... (world-image aworld)  ...
       (lotxt-temp (world-hidden aworld)) ...))

(check-expect (display WORLD0) SLIDE)
(check-expect (display (make-world (place-image
                            (text (txt-content TXT1)
                                  TEXTSIZE TEXTCOLOR)
                            (txt-x TXT1)
                            (txt-y TXT1)
                            SLIDE) LOTXT1))
              (place-image
               (text (txt-content TXT1)
                     TEXTSIZE TEXTCOLOR)
               (txt-x TXT1)
               (txt-y TXT1)
               SLIDE))

(define (display aworld)
  (world-image aworld))




;; World -> World
;; Consumes a world and creates a new world with the next hidden txt added to
;; the world-image

(check-expect (next WORLD0)
              (make-world (place-image
                            (text (txt-content (first (world-hidden WORLD0)))
                                  TEXTSIZE TEXTCOLOR)
                            (txt-x (first (world-hidden WORLD0)))
                            (txt-y (first (world-hidden WORLD0)))
                            (world-image WORLD0))
                           (rest (world-hidden WORLD0))))

(define (next aworld)
  (cond [(empty? (world-hidden aworld)) aworld]
         [else (make-world (place-image
                            (text (txt-content (first (world-hidden aworld)))
                                  TEXTSIZE TEXTCOLOR)
                            (txt-x (first (world-hidden aworld)))
                            (txt-y (first (world-hidden aworld)))
                            (world-image aworld))
                           (rest (world-hidden aworld)))]))


(define (main w0)
  (big-bang w0
            [on-tick next 1]
            [to-draw display]))

