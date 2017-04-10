;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |CS2500 Problem Set 7 - Seth Carroll & Evan Sevieri|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; PROBLEM SET 7
;; Evan Sevieri and Seth Carroll

(require 2htdp/image)
(require 2htdp/universe)


;; Data Definitions

;;A Centipede is a (make-centipede LoS)
(define-struct centipede [segs])

;;A LoC, List of Centipedes is one of
;; -empty
;; -(cons Centipede LoC)

;; A Dir is one of:
;;- "up", "down", "left", "right"

;;A LoS, List of Segs, is one of:
;; - empty
;; -(cons Seg LoS)

;; A LoX, List of Xs, is one of:
;; - empty
;; - (cons x LoX)
;; interp. an x is an x-position taken from a seg-x

;; A LoY, List of Ys, is one of:
;; - empty
;; - (cons y LoY)
;; interp. a y is a y-position taken from a seg-y

;;A Seg is a (make-seg Number Number Dir)
(define-struct seg [x y dir])
;; x and y in grid coords

;;A Player is a (make-player Number Number)
(define-struct player [x y])

;; A Bullet is a (make-bullet Number Number Boolean)
;; bullet-fired indicates whether a bullet has been fired
(define-struct bullet [x y fired])

;; A Gamestate is one of:
;; - "win"
;; - "lose"
;; - "play"

;; A World is a (make-world Player LoC Bullet Gamestate)
(define-struct world [player loc bullet gamestate])


;; Constants

(define GRID-WIDTH 25)
(define GRID-HEIGHT 40)
(define CELL-SIZE 15)
(define BG-WIDTH (* CELL-SIZE GRID-WIDTH))
(define BG-HEIGHT (* CELL-SIZE GRID-HEIGHT))

(define BG (empty-scene BG-WIDTH BG-HEIGHT))
 
(define PLAYER (square CELL-SIZE 'solid 'black))
(define PLAYER-Y (- GRID-HEIGHT .5))
(define BULLET (rectangle 3 8 'solid 'orange))
(define CENTIPEDE-CELL (square CELL-SIZE 'solid 'green))
(define TONGUE (triangle 5 'solid 'red))
(define LEFT-HEAD (overlay/align "left"
                                 "middle"
                                 (rotate 90 TONGUE)
                                 CENTIPEDE-CELL))
(define RIGHT-HEAD (overlay/align "right"
                                  "middle"
                                  (rotate 30 TONGUE)
                                  CENTIPEDE-CELL))
 
(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C 'LightSalmon)
(define MUSHROOM-2-C 'Salmon)
(define MUSHROOM-3-C 'OrangeRed)
(define MUSHROOM-4-C 'DarkRed)
 
(define WINNER (text "WINNER" 72 'black))
(define LOSER (text "LOSER" 72 'black))


;; Sample Worlds

(define WORLD0 (make-world
                (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 1 .5 "right")
                                            (make-seg 2 .5 "right")
                                            (make-seg 3 .5 "right")
                                            (make-seg 4 .5 "right")
                                            (make-seg 5 .5 "right")
                                            (make-seg 6 .5 "right")
                                            (make-seg 7 .5 "right")
                                            (make-seg 8 .5 "right")
                                            (make-seg 9 .5 "right")
                                            (make-seg 10 .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                             PLAYER-Y
                                false)
                "play"))


(define WORLD-TURN (make-world
                (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 25 1.5 "left")
                                            (make-seg 25 .5 "right")
                                            (make-seg 24 .5 "right")
                                            (make-seg 23 .5 "right")
                                            (make-seg 22 .5 "right")
                                            (make-seg 21 .5 "right")
                                            (make-seg 20 .5 "right")
                                            (make-seg 19 .5 "right")
                                            (make-seg 18 .5 "right")
                                            (make-seg 17 .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                             PLAYER-Y
                             false)
                "play"))

(define WORLD-SHOT (make-world
                (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 1 .5 "right")
                                            (make-seg 2 .5 "right")
                                            (make-seg 3 .5 "right")
                                            (make-seg 4 .5 "right")
                                            (make-seg 5 .5 "right")
                                            (make-seg 6 .5 "right")
                                            (make-seg 7 .5 "right")
                                            (make-seg 8 .5 "right")
                                            (make-seg 9 .5 "right")
                                            (make-seg 10 .5 "right"))))
                (make-bullet 8 .5 true)
                "play"))

(define WORLD-FIRED (make-world
                     (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 25 1.5 "left")
                                            (make-seg 25 .5 "right")
                                            (make-seg 24 .5 "right")
                                            (make-seg 23 .5 "right")
                                            (make-seg 22 .5 "right")
                                            (make-seg 21 .5 "right")
                                            (make-seg 20 .5 "right")
                                            (make-seg 19 .5 "right")
                                            (make-seg 18 .5 "right")
                                            (make-seg 17 .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                             PLAYER-Y
                             true)
                "play"))


;; ____________________________________________________________________________
;; TO-DRAW RENDER FUNCTION
;; ____________________________________________________________________________

;; render : World -> Image
;; Draws the world.

(check-expect (render (make-world
                       (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                       (list (make-centipede
                              '())
                             (make-centipede
                              '()))
                       (make-bullet (* .5 GRID-WIDTH)
                                    PLAYER-Y
                                    false)
                       "win"))
              (overlay WINNER BG))

(check-expect (render (make-world
                       (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                       (list (make-centipede 
                              (list (make-seg
                                     (* .5 GRID-WIDTH)
                                     PLAYER-Y
                                     "right"))))
                       (make-bullet (* .5 GRID-WIDTH)
                                    PLAYER-Y
                                    false)
                       "lose"))
                      (overlay LOSER BG))
              
(check-expect (render WORLD0)
              (place-image/grid
               CENTIPEDE-CELL
               1 .5
               (place-image/grid
                CENTIPEDE-CELL
                2 .5
                (place-image/grid
                CENTIPEDE-CELL
                3 .5
                (place-image/grid
                CENTIPEDE-CELL
                4 .5
                (place-image/grid
                CENTIPEDE-CELL
                5 .5
                (place-image/grid
                CENTIPEDE-CELL
                6 .5
                (place-image/grid
                CENTIPEDE-CELL
                7 .5
                (place-image/grid
                CENTIPEDE-CELL
                8 .5
                (place-image/grid
                CENTIPEDE-CELL
                9 .5
                (place-image/grid
                CENTIPEDE-CELL
                10 .5
                (place-image/grid
                 PLAYER
                 (* .5 GRID-WIDTH) PLAYER-Y
                 BG))))))))))) )

(check-expect (render (update WORLD-SHOT))
               (place-image/grid
                CENTIPEDE-CELL
                2 .5
                (place-image/grid
                CENTIPEDE-CELL
                3 .5
                (place-image/grid
                CENTIPEDE-CELL
                4 .5
                (place-image/grid
                CENTIPEDE-CELL
                5 .5
                (place-image/grid
                CENTIPEDE-CELL
                6 .5
                (place-image/grid
                CENTIPEDE-CELL
                7 .5
                (place-image/grid
                CENTIPEDE-CELL
                8 .5
                (place-image/grid
                CENTIPEDE-CELL
                9 .5
                (place-image/grid
                CENTIPEDE-CELL
                10 .5
                (place-image/grid
                 PLAYER
                 (* .5 GRID-WIDTH) PLAYER-Y
                 BG)))))))))) )

(define (render aworld)
     (cond [(string=? "win" (world-gamestate aworld))
            (overlay WINNER BG)]
           [(string=? "lose" (world-gamestate aworld))
            (overlay LOSER BG)]
           [(string=? "play" (world-gamestate aworld))
            (draw-centipede (first (world-loc aworld))
             (draw-player (world-player aworld)
                          (draw-bullet (world-bullet aworld) BG)))]))

;; draw-centipede : Centipede Image -> Image
;; adds the centipede to the scene

(check-expect (draw-centipede (make-centipede
                                      (list (make-seg 1 .5 "right")
                                            (make-seg 2 .5 "right"))) BG)
              (place-image/grid CENTIPEDE-CELL 1 .5
                                (place-image/grid CENTIPEDE-CELL 2 .5 BG)))

(define (draw-centipede acentipede ascene)
  (draw-segs (centipede-segs acentipede) ascene))

;; draw-player : Player Image -> Image
;; adds the player to the scene

(check-expect (draw-player (make-player (* .5 GRID-WIDTH  ) PLAYER-Y) BG)
              (place-image/grid PLAYER (* .5 GRID-WIDTH) PLAYER-Y BG))
                               
 
(define (draw-player aplayer ascene)
  (place-image/grid PLAYER
                    (player-x aplayer)
                    (player-y aplayer)
                    ascene))
  
;; draw-bullet : Bullet Image -> Image
;; adds the bullet to the scene if bullet-fired = true

(check-expect (draw-bullet (make-bullet (* .5 GRID-WIDTH)
                                        PLAYER-Y
                                        true) BG)
              (place-image/grid BULLET
                                (* .5 GRID-WIDTH)
                                PLAYER-Y
                                BG))
(check-expect (draw-bullet (make-bullet (* .5 GRID-WIDTH)
                                        PLAYER-Y
                                        false) BG)
                          BG)
                               
 
(define (draw-bullet abullet ascene)
  (if (bullet-fired abullet)
      (place-image/grid BULLET
                    (bullet-x abullet)
                    (bullet-y abullet)
                    ascene)
      ascene))

;; draw-segs : Seg Image -> Image
;; Draws segments on the scene

(check-expect (draw-segs (list (make-seg 1 .5 "right")
                               (make-seg 2 .5 "right")) BG)
              (place-image/grid CENTIPEDE-CELL 1 .5
                                (place-image/grid CENTIPEDE-CELL 2 .5 BG)))
              

(define (draw-segs alos ascene)
  (cond [(empty? alos) ascene]
        [else (place-image/grid CENTIPEDE-CELL
                                (seg-x (first alos))
                                (seg-y (first alos))
                                (draw-segs (rest alos) ascene))] ))
  

   
;; place-image/grid : Image Number Number Image -> Image
;; Places an image on a background in terms of grid coordinates

(check-expect (place-image/grid
               PLAYER
               (player-x (world-player WORLD0))
               (player-y (world-player WORLD0))
               BG)
              (place-image PLAYER
                           (* .5 BG-WIDTH)
                           (- BG-HEIGHT (* .5 CELL-SIZE))
                           BG))                          

(define (place-image/grid grid-image x y bg)
  (place-image grid-image
               (* x CELL-SIZE)
               (* y CELL-SIZE)
               bg))

;; ____________________________________________________________________________
;; ON-TICK UPDATE FUNCTION
;; ____________________________________________________________________________

;; update : World -> World
;; updates the world state

(check-expect (update (make-world
                       (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                       (list (make-centipede
                              '())
                             (make-centipede
                              '()))
                      (make-bullet (* .5 GRID-WIDTH)
                                   PLAYER-Y
                                   false)
                      "play"))
              (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
               (list (make-centipede
                      '())
                     (make-centipede
                      '()))
               (make-bullet (* .5 GRID-WIDTH)
                            PLAYER-Y
                            false)
               "win"))

(check-expect (update (make-world
                       (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                       (list (make-centipede 
                              (list (make-seg
                                     (* .5 GRID-WIDTH)
                                     PLAYER-Y
                                     "right"))))
                       (make-bullet (* .5 GRID-WIDTH)
                                    PLAYER-Y
                                    false)
                       "play"))
              (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
               (list (make-centipede 
                      (list (make-seg
                             (* .5 GRID-WIDTH)
                             PLAYER-Y
                             "right"))))
               (make-bullet (* .5 GRID-WIDTH)
                            PLAYER-Y
                            false)
               "lose"))

(check-expect (update (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                       (list (make-seg 24 1.5 "left")
                             (make-seg 25 1.5 "left")
                             (make-seg 25 2.5 "right")
                             (make-seg 24 2.5 "right")
                             (make-seg 23 2.5 "right")
                             (make-seg 22 2.5 "right")
                             (make-seg 21 2.5 "right")
                             (make-seg 20 2.5 "right")
                             (make-seg 19 2.5 "right")
                             (make-seg 18 2.5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                            -.5
                             true)
                "play"))
                      (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                       (list (make-seg (- 24 (/ 1 3)) 1.5 "left")
                             (make-seg (- 25 (/ 1 3)) 1.5 "left")
                             (make-seg 25 3.5 "left")
                             (make-seg (+ 24 (/ 1 3)) 2.5 "right")
                             (make-seg (+ 23 (/ 1 3)) 2.5 "right")
                             (make-seg (+ 22 (/ 1 3)) 2.5 "right")
                             (make-seg (+ 21 (/ 1 3)) 2.5 "right")
                             (make-seg (+ 20 (/ 1 3)) 2.5 "right")
                             (make-seg (+ 19 (/ 1 3)) 2.5 "right")
                             (make-seg (+ 18 (/ 1 3)) 2.5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                            -.5
                             false)
                "play"))

(check-expect (update WORLD-FIRED)
              (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                       (list (make-seg (- 25 (/ 1 3)) 1.5 "left")
                             (make-seg 25 1.5 "left")
                             (make-seg (+ 24 (/ 1 3)) .5 "right")
                             (make-seg (+ 23 (/ 1 3)) .5 "right")
                             (make-seg (+ 22 (/ 1 3)) .5 "right")
                             (make-seg (+ 21 (/ 1 3)) .5 "right")
                             (make-seg (+ 20 (/ 1 3)) .5 "right")
                             (make-seg (+ 19 (/ 1 3)) .5 "right")
                             (make-seg (+ 18 (/ 1 3)) .5 "right")
                             (make-seg (+ 17 (/ 1 3)) .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                            (sub1 PLAYER-Y)
                             true)
                "play"))

(check-expect (update WORLD-SHOT)
              (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
               (list (make-centipede 
                      (list (make-seg 2 .5 "right")
                            (make-seg 3 .5 "right")
                            (make-seg 4 .5 "right")
                            (make-seg 5 .5 "right")
                            (make-seg 6 .5 "right")
                            (make-seg 7 .5 "right")
                            (make-seg 8 .5 "right")
                            (make-seg 9 .5 "right")
                            (make-seg 10 .5 "right"))))
               (make-bullet (* .5 GRID-WIDTH)
                            PLAYER-Y
                            false)
               "play"))

(check-expect (update WORLD0)
              (make-world
               (make-player (* .5 GRID-WIDTH) PLAYER-Y)
               (list (make-centipede (list (make-seg (+ 1 (/ 1 3)) .5 "right")
                                           (make-seg (+ 2 (/ 1 3)) .5 "right")
                                           (make-seg (+ 3 (/ 1 3)) .5 "right")
                                           (make-seg (+ 4 (/ 1 3)) .5 "right")
                                           (make-seg (+ 5 (/ 1 3)) .5 "right")
                                           (make-seg (+ 6 (/ 1 3)) .5 "right")
                                           (make-seg (+ 7 (/ 1 3)) .5 "right")
                                           (make-seg (+ 8 (/ 1 3)) .5 "right")
                                           (make-seg (+ 9 (/ 1 3)) .5 "right")
                                           (make-seg (+ 10 (/ 1 3)) .5 "right"))))
               (make-bullet (* .5 GRID-WIDTH)
                            PLAYER-Y
                            false)
               "play"))



(define (update aworld)
  (cond [(dead-centipedes (world-loc aworld))
         (make-world
          (world-player aworld)
          (world-loc aworld)
          (world-bullet aworld)
          "win")]
        [(dead-player aworld)
         (make-world
          (world-player aworld)
          (world-loc aworld)
          (world-bullet aworld)
          "lose")]
        [(= -.5 (bullet-y (world-bullet aworld)))
         (make-world
          (world-player aworld)
          (move-centipedes (world-loc aworld))
          (make-bullet (bullet-x (world-bullet aworld))
                       (bullet-y (world-bullet aworld))
                       false)
          (world-gamestate aworld))]
        [(and (bullet-fired (world-bullet aworld))
              (not (world-extractor aworld)))
         (make-world
          (world-player aworld)
          (move-centipedes (world-loc aworld))
          (make-bullet (bullet-x (world-bullet aworld))
                       (sub1 (bullet-y (world-bullet aworld)))
                       true)
          (world-gamestate aworld))]
        [(and (bullet-fired (world-bullet aworld))
              (world-extractor aworld))
         (make-world
          (world-player aworld)
          (centipedes-killer (world-loc aworld))
          (make-bullet (player-x (world-player aworld))
                       PLAYER-Y
                       false)
          (world-gamestate aworld))]  
        [(not (bullet-fired (world-bullet aworld)))
         (make-world
          (world-player aworld)
          (move-centipedes (world-loc aworld))
          (world-bullet aworld)
          (world-gamestate aworld))]))

;; move-centipedes : LoC -> LoC
;; moves the position of the centipedes one grid position

(check-expect (move-centipedes (list (make-centipede 
                                      (list (make-seg 24 1.5 "left")
                                            (make-seg 25 1.5 "left")
                                            (make-seg 25 .5 "right")
                                            (make-seg 24 .5 "right")))))
              (list (make-centipede 
                     (list (make-seg (- 24 (/ 1 3)) 1.5 "left")
                           (make-seg (- 25 (/ 1 3)) 1.5 "left")
                           (make-seg 25 1.5 "left")
                           (make-seg (+ 24 (/ 1 3)) .5 "right")))))
(check-expect (move-centipedes (list (make-centipede 
                                      (list (make-seg 1 2.5 "right")
                                            (make-seg 0 2.5 "right")
                                            (make-seg 0 1.5 "left")
                                            (make-seg 1 1.5 "left")))))
              (list (make-centipede 
                     (list (make-seg (+ 1 (/ 1 3)) 2.5 "right")
                           (make-seg (+ 0 (/ 1 3)) 2.5 "right")
                           (make-seg 0 2.5 "right")
                           (make-seg (- 1 (/ 1 3)) 1.5 "left")))))

(define (move-centipedes aloc)
  (cond [(empty? aloc) empty]
        [else
         (cons (move-centipede (first aloc)) (move-centipedes (rest aloc)))]))

;; move-centipede : Centipede -> Centipede
;; Move one centipede one grid position

(check-expect (move-centipede (make-centipede 
                               (list (make-seg 24 1.5 "left")
                                     (make-seg 25 1.5 "left")
                                     (make-seg 25 .5 "right")
                                     (make-seg 24 .5 "right"))))
              (make-centipede 
               (list (make-seg (- 24 (/ 1 3)) 1.5 "left")
                     (make-seg (- 25 (/ 1 3)) 1.5 "left")
                     (make-seg 25 1.5 "left")
                     (make-seg (+ 24 (/ 1 3)) .5 "right"))))
(check-expect (move-centipede (make-centipede 
                                (list (make-seg 1 2.5 "right")
                                      (make-seg 0 2.5 "right")
                                      (make-seg 0 1.5 "left")
                                      (make-seg 1 1.5 "left"))))
              (make-centipede 
               (list (make-seg (+ 1 (/ 1 3)) 2.5 "right")
                     (make-seg (+ 0 (/ 1 3)) 2.5 "right")
                     (make-seg 0 2.5 "right")
                     (make-seg (- 1 (/ 1 3)) 1.5 "left"))))

(define (move-centipede acentipede)
  (make-centipede (move-segs (centipede-segs acentipede))))

;; move-segs : LoS -> LoS
;; moves the segments one grid position

(check-expect (move-segs (list (make-seg 24 1.5 "left")
                               (make-seg 25 1.5 "left")
                               (make-seg 25 .5 "right")
                               (make-seg 24 .5 "right")))
              (list (make-seg (- 24 (/ 1 3)) 1.5 "left")
                    (make-seg (- 25 (/ 1 3)) 1.5 "left")
                    (make-seg 25 1.5 "left")
                    (make-seg (+ 24 (/ 1 3)) .5 "right")))
(check-expect (move-segs (list (make-seg 1 2.5 "right")
                               (make-seg 0 2.5 "right")
                               (make-seg 0 1.5 "left")
                               (make-seg 1 1.5 "left")))
              (list (make-seg (+ (/ 1 3) 1) 2.5 "right")
                    (make-seg (+ (/ 1 3) 0) 2.5 "right")
                    (make-seg 0 2.5 "right")
                    (make-seg (- 1 (/ 1 3)) 1.5 "left")))

(define (move-segs alos)
  (cond [(empty? alos) empty]
        [else (cons (move-seg (first alos)) (move-segs (rest alos)))]))


;; move-seg : Seg -> Seg
;; Moves one segment one grid position

(check-expect (move-seg (make-seg 5 .5 "right")) (make-seg (+ (/ 1 3) 5) .5 "right"))
(check-expect (move-seg (make-seg 25 .5 "right")) (make-seg 25 1.5 "left"))
(check-expect (move-seg (make-seg 5 1.5 "left")) (make-seg (- 5 (/ 1 3)) 1.5 "left"))
(check-expect (move-seg (make-seg 0 1.5 "left")) (make-seg 0 2.5 "right"))

(define (move-seg aseg)
  (cond [(and (string=? (seg-dir aseg) "right")
              (< (seg-x aseg) 25))
         (make-seg (+ (/ 1 3) (seg-x aseg)) (seg-y aseg) (seg-dir aseg))]
        [(and (string=? (seg-dir aseg) "right")
              (= (seg-x aseg) 25))
         (make-seg (seg-x aseg) (+ 1 (seg-y aseg)) "left")]
        [(and (string=? (seg-dir aseg) "left")
              (> (seg-x aseg) 0))
         (make-seg (- (seg-x aseg) (/ 1 3)) (seg-y aseg) (seg-dir aseg))]
        [(and (string=? (seg-dir aseg) "left")
              (= (seg-x aseg) 0))
         (make-seg (seg-x aseg) (+ 1 (seg-y aseg)) "right")]))





        
                                      

;; centipedes-killer : LoC -> LoC
;; Consumes an LoC and returns an LoC with any hit centipede

(check-expect (centipedes-killer (list (make-centipede 
                                        (list (make-seg 1 .5 "right")
                                              (make-seg 2 .5 "right")
                                              (make-seg 3 .5 "right")
                                              (make-seg 4 .5 "right")
                                              (make-seg 5 .5 "right")
                                              (make-seg 6 .5 "right")
                                              (make-seg 7 .5 "right")
                                              (make-seg 8 .5 "right")))
                                       (make-centipede 
                                        (list (make-seg 1 .5 "right")
                                              (make-seg 2 .5 "right")
                                              (make-seg 3 .5 "right")
                                              (make-seg 4 .5 "right")
                                              (make-seg 5 .5 "right")
                                              (make-seg 6 .5 "right")
                                              (make-seg 7 .5 "right")
                                              (make-seg 8 .5 "right")))))
              (list (make-centipede 
                     (list (make-seg 2 .5 "right")
                           (make-seg 3 .5 "right")
                           (make-seg 4 .5 "right")
                           (make-seg 5 .5 "right")
                           (make-seg 6 .5 "right")
                           (make-seg 7 .5 "right")
                           (make-seg 8 .5 "right")))
                    (make-centipede 
                     (list   (make-seg 2 .5 "right")
                             (make-seg 3 .5 "right")
                             (make-seg 4 .5 "right")
                             (make-seg 5 .5 "right")
                             (make-seg 6 .5 "right")
                             (make-seg 7 .5 "right")
                             (make-seg 8 .5 "right")))))

(define (centipedes-killer aloc)
  (cond
    [(empty? aloc) '()]
    [else (cons (centipede-killer (first aloc))
                (centipedes-killer (rest aloc)))]))

;; centipede-killer : Centipede -> Centipede
;; Consumes a centipede and returns a centipede without the last segment

(check-expect (centipede-killer (make-centipede 
                                        (list (make-seg 1 .5 "right")
                                              (make-seg 2 .5 "right")
                                              (make-seg 3 .5 "right")
                                              (make-seg 4 .5 "right")
                                              (make-seg 5 .5 "right")
                                              (make-seg 6 .5 "right")
                                              (make-seg 7 .5 "right")
                                              (make-seg 8 .5 "right")
                                              (make-seg 9 .5 "right"))))
              (make-centipede 
                                        (list (make-seg 2 .5 "right")
                                              (make-seg 3 .5 "right")
                                              (make-seg 4 .5 "right")
                                              (make-seg 5 .5 "right")
                                              (make-seg 6 .5 "right")
                                              (make-seg 7 .5 "right")
                                              (make-seg 8 .5 "right")
                                              (make-seg 9 .5 "right"))))

(define (centipede-killer acentipede)
  (make-centipede (rest (centipede-segs acentipede))))

;; bullet-contact: LoC Bullet -> Boolean
;; Consumes a LoC and determines if a bullet is in contact with a
;;  centipede segment

(check-expect (bullet-contact (world-loc WORLD0) (world-bullet WORLD0))
              false)
(check-expect (bullet-contact (world-loc WORLD-SHOT) (world-bullet WORLD-SHOT))
              true)

(define (bullet-contact aloc abullet)
  (cond [(empty? aloc) false]
        [(or (or (x-member-checker abullet
                             (xextremelister
                              (x-lister (segs-extractor (first aloc)))))
             (y-member-checker abullet
                             (yextremelister
                              (y-lister (segs-extractor (first aloc))))))
             (bullet-contact (rest aloc) abullet)) true]
        [else false]))


;; world-extractor : World -> Boolean
;; extracts the LoC and Bullet from a world for the bullet-contact function
(check-expect (world-extractor WORLD0) false)
(check-expect (world-extractor WORLD-SHOT) true)

(define (world-extractor aworld)
  (bullet-contact (loc-extractor aworld) (world-bullet aworld)))


               
;; loc-extractor : World -> LoC
;; extracts a LoC from a World

(check-expect (loc-extractor WORLD0)
              (list (make-centipede 
                     (list (make-seg 1 .5 "right")
                           (make-seg 2 .5 "right")
                           (make-seg 3 .5 "right")
                           (make-seg 4 .5 "right")
                           (make-seg 5 .5 "right")
                           (make-seg 6 .5 "right")
                           (make-seg 7 .5 "right")
                           (make-seg 8 .5 "right")
                           (make-seg 9 .5 "right")
                           (make-seg 10 .5 "right")))))
(define (loc-extractor aworld)
  (world-loc aworld))
              

;; segs-extractor : Centipede -> LoS
;; Extracts an LoS from a Centipede

(check-expect (segs-extractor (first (world-loc WORLD0)))
              (list (make-seg 1 .5 "right")
                    (make-seg 2 .5 "right")
                    (make-seg 3 .5 "right")
                    (make-seg 4 .5 "right")
                    (make-seg 5 .5 "right")
                    (make-seg 6 .5 "right")
                    (make-seg 7 .5 "right")
                    (make-seg 8 .5 "right")
                    (make-seg 9 .5 "right")
                    (make-seg 10 .5 "right")))

(define (segs-extractor acentipede)
  (centipede-segs acentipede))

;; xextremelister : LoX -> LoXE
;; consumes a List of Xs and creates
;;  a list of the floors and ceilings of their values

(check-expect (xextremelister (list .5 1.33))
              (list 0 1 1 2))
(define (xextremelister alox)
  (cond [(empty? alox) empty]
        [else (cons (floor (first alox))
                    (cons (ceiling (first alox))
                          (xextremelister (rest alox))))]))

;; xlister : LoS -> LoX
;; consumes a List of Segments and creates a list of their x-values

(check-expect (x-lister (list
                             (make-seg 1 .5 "right")
                             (make-seg 2 .5 "right")
                             (make-seg 3 .5 "right")
                             (make-seg 4 .5 "right")
                             (make-seg 5 .5 "right")
                             (make-seg 6 .5 "right")
                             (make-seg 7 .5 "right")
                             (make-seg 8 .5 "right")
                             (make-seg 9 .5 "right")
                             (make-seg 10 .5 "right")))
              (list 1 2 3 4 5 6 7 8 9 10))

(define (x-lister alos)
  (cond [(empty? alos) '()]
        [else (cons (seg-x (first alos))
                    (x-lister (rest alos)))]))

;; yextremelister : LoY -> LoYE
;; consumes a List of Ys and creates
;;  a list of the floors and ceilings of their values

(check-expect (yextremelister (list .5 1.33))
              (list 0 1 1 2))
(define (yextremelister aloy)
  (cond [(empty? aloy) empty]
        [else (cons (floor (first aloy))
                    (cons (ceiling (first aloy))
                          (yextremelister (rest aloy))))]))
;; ylister : LoS -> LoY
;; consumes a List of Segments and creates a list of their y-values

(check-expect (y-lister (list
                             (make-seg 1 .5 "right")
                             (make-seg 2 .5 "right")
                             (make-seg 3 .5 "right")
                             (make-seg 4 .5 "right")
                             (make-seg 5 .5 "right")
                             (make-seg 6 .5 "right")
                             (make-seg 7 .5 "right")
                             (make-seg 8 .5 "right")
                             (make-seg 9 .5 "right")
                             (make-seg 10 .5 "right")))
              (list .5 .5 .5 .5 .5 .5 .5 .5 .5 .5))

(define (y-lister alos)
  (cond [(empty? alos) '()]
        [else (cons (seg-y (first alos))
                    (y-lister (rest alos)))]))


;; x-member-checker : Bullet LoXE -> Boolean
;; Checks if a Bullet's X is a member of a LoXE

(check-expect (x-member-checker (world-bullet WORLD-SHOT) (list 7 8))
              true)
(check-expect (x-member-checker (world-bullet WORLD0) (list 11 12))
              false)

(define (x-member-checker abullet aloe)
  (member (bullet-x abullet) aloe))

;; y-member-checker : Bullet LoYe -> Boolean
;; Checks if a Bullet's Y is a member of a LoYE

(check-expect (y-member-checker (world-bullet WORLD-SHOT) (list .5 .5))
              true)
(check-expect (y-member-checker (world-bullet WORLD0) (list .5 .5))
              false)

(define (y-member-checker abullet aloe)
  (member (bullet-y abullet) aloe))


;; dead-centipedes : LoC -> Boolean
;; Reports whether all centipedes are dead (are empty/has no segments)

(check-expect (dead-centipedes (list (make-centipede
                                      (make-seg 1 .5 "right"))
                                     (make-centipede
                                      (make-seg 2 .5 "right"))))
              false)

(check-expect (dead-centipedes (list (make-centipede
                                      '())
                                     (make-centipede
                                      '())))
              true)

(define (dead-centipedes aloc)
  (cond [(empty? aloc) true]
        [else (and (dead-centipede (first aloc))
                   (dead-centipedes (rest aloc)))]))

;; dead-centipede : Centipede -> Boolean
;; Reports whether a centipede is dead (is empty/has no segments)

(check-expect (dead-centipede (make-centipede
                                      (make-seg 1 .5 "right")))
              false)

(check-expect (dead-centipede (make-centipede
                                      '()))
              true)

(define (dead-centipede acentipede)
  (cond
    [(empty? (segs-extractor acentipede)) true]
    [else false]))
  

;; dead-player : World -> Boolean
;; reports whether the player is dead (player pos = centipede head pos)

(check-expect (dead-player WORLD0) false)
(check-expect (dead-player (make-world
                            (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                            (list (make-centipede 
                                   (list (make-seg
                                          (* .5 GRID-WIDTH)
                                          PLAYER-Y
                                          "right"))))
                            (make-bullet (* .5 GRID-WIDTH)
                                         PLAYER-Y
                                         false)
                            "play"))
              true)

(define (dead-player aworld)
  (cond
    [(and (= (player-x (world-player aworld))
             (first (x-lister (segs-extractor (first (world-loc aworld))))))
          (= (player-y (world-player aworld))
             (first (y-lister (segs-extractor (first (world-loc aworld)))))))
         true]
    [else false]))
          
;; ____________________________________________________________________________
;; ON-KEY PLAYER-INPUT FUNCTION
;; ____________________________________________________________________________

;;player-input : World KeyEvent -> World
;;Updates the world on a key input

;;MOVE PLAYER LEFT
(check-expect (player-input WORLD0 "left")
              (make-world
                (make-player (- (* .5 GRID-WIDTH) 1) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 1 .5 "right")
                                            (make-seg 2 .5 "right")
                                            (make-seg 3 .5 "right")
                                            (make-seg 4 .5 "right")
                                            (make-seg 5 .5 "right")
                                            (make-seg 6 .5 "right")
                                            (make-seg 7 .5 "right")
                                            (make-seg 8 .5 "right")
                                            (make-seg 9 .5 "right")
                                            (make-seg 10 .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                             PLAYER-Y
                                false)
                "play"))
;; MOVE PLAYER RIGHT
(check-expect (player-input WORLD0 "right")
              (make-world
                (make-player (+ (* .5 GRID-WIDTH) 1) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 1 .5 "right")
                                            (make-seg 2 .5 "right")
                                            (make-seg 3 .5 "right")
                                            (make-seg 4 .5 "right")
                                            (make-seg 5 .5 "right")
                                            (make-seg 6 .5 "right")
                                            (make-seg 7 .5 "right")
                                            (make-seg 8 .5 "right")
                                            (make-seg 9 .5 "right")
                                            (make-seg 10 .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                             PLAYER-Y
                                false)
                "play"))
;;FIRE BULLET
(check-expect (player-input WORLD0 " ")
              (make-world
                (make-player (* .5 GRID-WIDTH) PLAYER-Y)
                (list (make-centipede 
                                      (list (make-seg 1 .5 "right")
                                            (make-seg 2 .5 "right")
                                            (make-seg 3 .5 "right")
                                            (make-seg 4 .5 "right")
                                            (make-seg 5 .5 "right")
                                            (make-seg 6 .5 "right")
                                            (make-seg 7 .5 "right")
                                            (make-seg 8 .5 "right")
                                            (make-seg 9 .5 "right")
                                            (make-seg 10 .5 "right"))))
                (make-bullet (* .5 GRID-WIDTH)
                             PLAYER-Y
                                true)
                "play"))


(define (player-input aworld ke)
  (cond
    [(and (key=? ke "left") (> (player-x (world-player aworld)) 1))
     (make-world (make-player (sub1 (player-x (world-player aworld)))
                              (player-y (world-player aworld)))
                 (world-loc aworld)
                 (world-bullet aworld)
                 (world-gamestate aworld))]
    [(and (key=? ke "right") (> (sub1 GRID-WIDTH) (player-x (world-player aworld))))
     (make-world (make-player (add1 (player-x (world-player aworld)))
                              (player-y (world-player aworld)))
                 (world-loc aworld)
                 (world-bullet aworld)
                 (world-gamestate aworld))]
    [(and (key=? ke " ") (not (bullet-fired (world-bullet aworld))))
     (make-world (world-player aworld)
                 (world-loc aworld)
                 (make-bullet (player-x (world-player aworld))
                              PLAYER-Y
                              true)
                 (world-gamestate aworld))]
    [else aworld]))
    
                              
;; ____________________________________________________________________________
;; BIG-BANG MAIN FUNCTION
;; ____________________________________________________________________________

(define (main w0)
  (big-bang w0
            [to-draw render]
            [on-tick update]
            [on-key player-input]))

(main WORLD0)











































