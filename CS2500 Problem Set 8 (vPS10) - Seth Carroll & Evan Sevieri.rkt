;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |CS2500 Problem Set 8 (vPS10) - Seth Carroll & Evan Sevieri|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; PROBLEM SET 8 (v Pre-Problem Set 10)
;; Evan Sevieri and Seth Carroll

(require 2htdp/image)
(require 2htdp/universe)


;; Data Definitions

;; A Dir is one of:
;;- "up", "down", "left", "right"

;; An HDir is one of:
;; -"left"
;; -"right"

;; A VDir is one of:
;; -"up"
;; -"down"

;; A Mushroom is a (make-mush Posn Number)
(define-struct mush [posn state])

;;List of Mushroom, LoM, is one of
;; - empty
;; - (cons Mushroom LoM)

(define (dir-temp hdir vdir)
  (cond
    [(string=? hdir "left") ...]
    [(string=? hdir "right") ...]
    [(string=? vdir "up") ...]
    [(string=? vdir "down") ...]))

;;A LoPosn, List of Posns, is one of:
;; - empty
;; -(cons Seg LoPosn)

#;(define (loposn-temp aloposn)
  (cond
    [(empty? aloposn) ...]
    [(cons? aloposn) ... (first aloposn) ...
                     ... (loposn-temp (rest aloposn)) ...]))

;;A Centipede is a (make-centipede Dir Dir LoPosn)
(define-struct centipede [hdir vdir segs])
;  - Centipede head is first segment in list.
;  - Grid coordinates, +y is down.



#;(define (centipede-temp acentipede)
  (... (dir-temp (centipede-dir acentipede)) ...
       (centipede-segs acentipede) ...))

;;A LoC, List of Centipedes is one of
;; -empty
;; -(cons Centipede LoC)

#;(define (loc-temp aloc)
  (cond
    [(empty? aloc) ...]
    [(cons? aloc) ... (centipede-temp (first aloc)) ...
                  ... (loc-temp (rest aloc)) ...]))

;;A Player is a (make-player posn)
(define-struct player [posn])

#;(define (player-temp aplayer)
  (... (posn-x (player-posn aplayer)) ...
       (posn-y (player-posn aplayer)) ...))
       

;; A Bullet is a (make-bullet Posn Boolean)
;; bullet-fired indicates whether a bullet has been fired
(define-struct bullet [posn fired])

#;(define (bullet-temp abullet)
  (... (bullet-posn abullet) ...
       (bullet-fired abullet) ...))

;; A Gamestate is one of:
;; - "win"
;; - "lose"
;; - "play"

#;(define (gamestate-temp agamestate)
  (cond
    [(string=? agamestate "win") ...]
    [(string=? agamestate "lose") ...]
    [(string=? agamestate "play") ...]))

;; A World is a (make-world Player LoC Bullet LoM Gamestate Number)
(define-struct world [player loc bullet lom gamestate timer])
;; timer : counts up starting from 0 every time update function runs

#;(define (world-temp aworld)
  (... (player-temp (world-player aworld)) ...
       (loc-temp (world-loc aworld)) ...
       (bullet-temp (world-bullet aworld)) ...
       (gamestate-temp (world-gamestate aworld)) ...
       (world-timer aworld)))


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
(define MUSH-LIST
  (list (make-mush (make-posn(random GRID-WIDTH)
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)
        (make-mush (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                   (+ -.5 (random GRID-HEIGHT)))
                   4)))

(define WORLD0 (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 10 .5)
                                            (make-posn 9 .5)
                                            (make-posn 8 .5)
                                            (make-posn 7 .5)
                                            (make-posn 6 .5)
                                            (make-posn 5 .5)
                                            (make-posn 4 .5)
                                            (make-posn 3 .5)
                                            (make-posn 2 .5)
                                            (make-posn 1 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                        PLAYER-Y)
                                false)
                MUSH-LIST
                "play"
                0))

(define ANTI-WORLD0 (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "left" "down"
                                      (list (make-posn 15 .5)
                                            (make-posn 16 .5)
                                            (make-posn 17 .5)
                                            (make-posn 18 .5)
                                            (make-posn 19 .5)
                                            (make-posn 20 .5)
                                            (make-posn 21 .5)
                                            (make-posn 22 .5)
                                            (make-posn 23 .5)
                                            (make-posn 24 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                        PLAYER-Y)
                                false)
                MUSH-LIST
                "play"
                0))


(define WORLD-TURN (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 25 1.5)
                                            (make-posn 25 .5)
                                            (make-posn 24 .5)
                                            (make-posn 23 .5)
                                            (make-posn 22 .5)
                                            (make-posn 21 .5)
                                            (make-posn 20 .5)
                                            (make-posn 19 .5)
                                            (make-posn 18 .5)
                                            (make-posn 17 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                        PLAYER-Y)
                             false)
                MUSH-LIST
                "play"
                0))

(define WORLD-SHOT (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 10 .5)
                                            (make-posn 9 .5)
                                            (make-posn 8 .5)
                                            (make-posn 7 .5)
                                            (make-posn 6 .5)
                                            (make-posn 5 .5)
                                            (make-posn 4 .5)
                                            (make-posn 3 .5)
                                            (make-posn 2 .5)
                                            (make-posn 1 .5))))
                (make-bullet (make-posn 10 1) true)
                MUSH-LIST
                "play"
                0))

(define WORLD-FIRED (make-world
                     (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 25 1.5)
                                            (make-posn 25 .5)
                                            (make-posn 24 .5)
                                            (make-posn 23 .5)
                                            (make-posn 22 .5)
                                            (make-posn 21 .5)
                                            (make-posn 20 .5)
                                            (make-posn 19 .5)
                                            (make-posn 18 .5)
                                            (make-posn 17 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                        PLAYER-Y)
                             true)
                MUSH-LIST
                "play"
                0))


;; ____________________________________________________________________________
;; TO-DRAW RENDER FUNCTION
;; ____________________________________________________________________________

;; render : World -> Image
;; Draws the world.

(check-expect (render (make-world
                       (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                       (list (make-centipede "right" "down"
                              '())
                             (make-centipede "right" "down"
                              '()))
                       (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                              PLAYER-Y)
                                    false)
                       (make-mush (make-posn 1 2) 3)
                       "win"
                       0))
              (overlay WINNER BG))

(check-expect (render (make-world
                       (make-player (make-posn (* .5 GRID-WIDTH) PLAYER-Y))
                       (list (make-centipede "right" "down"
                              (list (make-posn
                                     (+ .5 (* .5 GRID-WIDTH))
                                     PLAYER-Y))))
                       (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                               PLAYER-Y)
                                    false)
                       (make-mush (make-posn 1 2) 3)
                       "lose"
                       0))
                      (overlay LOSER BG))
              
#;(check-expect (render WORLD0)
              (place-image/grid
               RIGHT-HEAD
               10 .5
               (place-image/grid
                CENTIPEDE-CELL
                9 .5
                (place-image/grid
                CENTIPEDE-CELL
                8 .5
                (place-image/grid
                CENTIPEDE-CELL
                7 .5
                (place-image/grid
                CENTIPEDE-CELL
                6 .5
                (place-image/grid
                CENTIPEDE-CELL
                5 .5
                (place-image/grid
                CENTIPEDE-CELL
                4 .5
                (place-image/grid
                CENTIPEDE-CELL
                3 .5
                (place-image/grid
                CENTIPEDE-CELL
                2 .5
                (place-image/grid
                CENTIPEDE-CELL
                1 .5
                (place-image/grid
                 PLAYER
                 (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y
                 BG))))))))))) )

(define (render aworld)
     (cond [(string=? "win" (world-gamestate aworld))
            (overlay WINNER BG)]
           [(string=? "lose" (world-gamestate aworld))
            (overlay LOSER BG)]
           [(string=? "play" (world-gamestate aworld))
            (draw-centipedes (world-loc aworld)
             (draw-player (world-player aworld)
                          (draw-mushrooms (world-lom aworld)
                           (draw-bullet (world-bullet aworld) BG))))]))

;; draw-mushrooms : LoM Image -> Image
;; places mushrooms on the scene

(define (draw-mushrooms alom ascene)
  (cond
    [(empty? alom) ascene]
    [(cons? alom) (if (not (= 0 (mush-state (first alom))))
                      (place-image/grid
                       (circle (/ CELL-SIZE 2)
                               'solid
                               (cond
                                 [(= 4 (mush-state (first alom)))
                                  MUSHROOM-4-C]
                                 [(= 3 (mush-state (first alom)))
                                  MUSHROOM-3-C]
                                 [(= 2 (mush-state (first alom)))
                                  MUSHROOM-2-C]
                                 [(= 1 (mush-state (first alom)))
                                  MUSHROOM-1-C] ))
                       (posn-x (mush-posn (first alom)))
                       (posn-y (mush-posn (first alom)))
                       (draw-mushrooms (rest alom) ascene))
                      (draw-mushrooms (rest alom) ascene))]))

;; draw-centipedes: LoC Image -> Image
;; adds the centipedes to the scene
(check-expect (draw-centipedes (list (make-centipede "right" "down"
                                      (list (make-posn 6 .5)
                                            (make-posn 5 .5)))
                                     (make-centipede "left" "down"
                                      (list (make-posn 3 .5)
                                            (make-posn 4 .5)))) BG)
              (place-image/grid
               RIGHT-HEAD 6 .5
               (place-image/grid
                CENTIPEDE-CELL 5 .5
                (place-image/grid LEFT-HEAD 3 .5
                                  (place-image/grid CENTIPEDE-CELL 4 .5
                                                    BG)))))

(define (draw-centipedes aloc ascene)
  (cond [(empty? aloc) ascene]
        [(cons? aloc) (if (not (empty? (centipede-segs (first aloc))))
                          (draw-centipede (first aloc)
                                      (draw-centipedes (rest aloc) ascene))
                          (draw-centipedes (rest aloc) ascene))]))
;; draw-centipede : Centipede Image -> Image
;; adds a centipede to the scene

(check-expect (draw-centipede (make-centipede "right" "down"
                                      (list (make-posn 2 .5)
                                            (make-posn 1 .5))) BG)
              (place-image/grid RIGHT-HEAD 2 .5
                                (place-image/grid CENTIPEDE-CELL 1 .5 BG)))

(define (draw-centipede acentipede ascene)
  (draw-head (first (centipede-segs acentipede))
             (centipede-hdir acentipede)
             (draw-body (rest (centipede-segs acentipede))
                        ascene)))

;; draw-body : LoPosn Image -> Image
;; Draws body segments on the scene

(check-expect (draw-body (list (make-posn 1 .5)) BG)
              (place-image/grid CENTIPEDE-CELL 1 .5 BG))
              

(define (draw-body aloposn ascene)
  (cond [(empty? aloposn) ascene]
        [(cons? aloposn) (place-image/grid CENTIPEDE-CELL
                                (posn-x (first aloposn))
                                (posn-y (first aloposn))
                                (draw-body (rest aloposn) ascene))]))

;; draw-head : Posn Dir Image -> Image
;; Draws segments on the scene

(check-expect (draw-head (make-posn 2 .5) "right" BG)
              (place-image/grid RIGHT-HEAD 2 .5 BG))
(check-expect (draw-head (make-posn 2 .5) "left" BG)
              (place-image/grid LEFT-HEAD 2 .5 BG))

(define (draw-head aposn adir ascene)
  (cond
    [(string=? adir "right") (place-image/grid RIGHT-HEAD
                                               (posn-x aposn)
                                               (posn-y aposn)
                                               ascene)]
    [(string=? adir "left") (place-image/grid LEFT-HEAD
                                               (posn-x aposn)
                                               (posn-y aposn)
                                               ascene)]))



;; draw-player : Player Image -> Image
;; adds the player to the scene

(check-expect (draw-player (make-player (make-posn
                                         (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                           BG)
              (place-image/grid PLAYER (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y BG))
                               
 
(define (draw-player aplayer ascene)
  (place-image/grid PLAYER
                    (posn-x (player-posn aplayer))
                    (posn-y (player-posn aplayer))
                    ascene))
  
;; draw-bullet : Bullet Image -> Image
;; adds the bullet to the scene if bullet-fired = true

(check-expect (draw-bullet (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                                   PLAYER-Y)
                                        true) BG)
              (place-image/grid BULLET
                                (+ .5 (* .5 GRID-WIDTH))
                                PLAYER-Y
                                BG))
(check-expect (draw-bullet (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                                   PLAYER-Y)
                                        false) BG)
                          BG)
                               
 
(define (draw-bullet abullet ascene)
  (if (bullet-fired abullet)
      (place-image/grid BULLET
                    (posn-x (bullet-posn abullet))
                    (posn-y (bullet-posn abullet))
                    ascene)
      ascene))

   
;; place-image/grid : Image Number Number Image -> Image
;; Places an image on a background in terms of grid coordinates

(check-expect (place-image/grid PLAYER 5 5 BG)
              (place-image PLAYER 75 75 BG))                          

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



(define (update-world aworld)
  (if (playing? (world-gamestate aworld))
      (make-world (world-player aworld)
                  (update-loc (world-loc aworld)
                              (world-bullet aworld)
                              (world-timer aworld)
                              (world-lom aworld))
                  (update-bullet (world-bullet aworld)
                                 (world-loc aworld)
                                 (world-player aworld)
                                 (world-lom aworld))
                  (update-lom (world-loc aworld)
                              (world-lom aworld)
                              (world-bullet aworld))
                  (update-gamestate aworld)
                  (update-timer (world-timer aworld)))
      aworld))

;; playing? : Gamestate -> Boolean
;; Checks if the game is being played


(define (playing? agamestate)
  (string=? agamestate "play"))

;;_____________________________________________________________________________
;; UPDATE MUSH FUNCTION
;;_____________________________________________________________________________

;; update-mush : LoM Bullet -> LoM
;;Update a alist of mushrooms
(check-expect (update-lom
               (world-loc WORLD0)
               (list (make-mush (make-posn 1 2) 1)
                     (make-mush (make-posn 4 2) 4)
                     (make-mush (make-posn 1 9) 0))
               (make-bullet (make-posn 1 2) true))
              (list (make-mush (make-posn 1 2) 0)
                    (make-mush (make-posn 4 2) 4)))

(define (update-lom aloc alom b)
  (cond
    [(empty? alom) empty]
    [(= 0 (mush-state (first alom))) (remove (first alom) alom)]
    [(loc-hit? aloc b) (cons (make-mush (bullet-posn b)
                   4) alom)]
    [(cons? alom) (cons (update-shroom (first alom) b)
                       (update-lom aloc (rest alom) b))]))

;;update-shroom : Mushroom Bullet -> Mushroom
;; Updates the state of the mushroom
(check-expect (update-shroom (make-mush (make-posn 1 2) 1)
                             (make-bullet (make-posn 1 2) true))
              (make-mush (make-posn 1 2) 0))
                             

(define (update-shroom m b)
  (cond
    [(posn=? (mush-posn m) (bullet-posn b))
     (hit-mush m)]
    [else m]))

;; hit-mush : Mushroom -> Mushroom
;;Decreases the state of the mushroom by 1

(check-expect (hit-mush (make-mush (make-posn 1 2) 1))
              (make-mush (make-posn 1 2) 0))

(define (hit-mush m)
  (make-mush (mush-posn m)
             (- (mush-state m) 1)))

;; ____________________________________________________________________________
;; UPDATE LOC FUNCTION
;; ____________________________________________________________________________

;; update-loc : LoC Bullet Timer -> LoC
;;Updates the List of Centipedes

(check-expect (update-loc (list (make-centipede "right" "down"
                                                (list (make-posn 7 4.5)
                                                      (make-posn 6 4.5)
                                                      (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5))))
                          (make-bullet (make-posn 6 4.5) true)
                          5
                          (list (make-mush
                                 (make-posn 1 2) 4)))
              (list (make-centipede "right" "down" (list (make-posn 7 4.5)))
                    (make-centipede "left" "down" (list (make-posn 5 4.5)))
                    (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5)))))

(check-expect (update-loc (list (make-centipede "right" "down"
                                                (list (make-posn 7 4.5)
                                                      (make-posn 6 4.5)
                                                      (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5))))
                          (make-bullet (make-posn 12 4.5) true)
                          2
                          (list (make-mush
                                 (make-posn 1 2) 4)))
              (list (make-centipede "right" "down"
                                                (list (make-posn 7 4.5)
                                                      (make-posn 6 4.5)
                                                      (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5)))))
(check-expect (update-loc (list (make-centipede "right" "down"
                                                (list (make-posn 7 4.5)
                                                      (make-posn 6 4.5)
                                                      (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5))))
                          (make-bullet (make-posn 3 4.5) true)
                          5
                          (list (make-mush
                                 (make-posn 1 2) 4)))
              (list (make-centipede "right" "down"
                                                (list (make-posn 8 4.5)
                                                      (make-posn 7 4.5)
                                                      (make-posn 6 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 11 4.5)
                                                      (make-posn 10 4.5)))))
(define (update-loc aloc abullet atimer alom)
  (cond
    [(empty? aloc) empty]
    [(and (and (cons? aloc)
          (= (modulo atimer 5) 0))
          (not (loc-hit? aloc abullet)))
     (move-loc aloc alom)]
    [(and (cons? aloc)
          #|(= (modulo atimer 5) 0))|#
          (loc-hit? aloc abullet))
     (break-loc aloc abullet)]
    [else aloc]))

;;break-loc : LoC Bullet -> LoC
;; Creates a LoC of new centipedes after breaking

(check-expect (break-loc (list (make-centipede "right" "down"
                                                (list (make-posn 7 4.5)
                                                      (make-posn 6 4.5)
                                                      (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5))))
                          (make-bullet (make-posn 6 4.5) true))
              (list (make-centipede "right" "down" (list (make-posn 7 4.5)))
                    (make-centipede "left" "down" (list (make-posn 5 4.5)))
                    (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5)))))
                    
(define (break-loc aloc abullet)
  (cond
    [(empty? aloc) empty]
    [(cons? aloc) (cons (first-centipede (centipede-hdir (which-hit? aloc abullet))
                                         (centipede-vdir (which-hit? aloc abullet))
                                         abullet
                                         (centipede-segs (which-hit? aloc abullet)))
                        (cons (rest-centipede (centipede-hdir (which-hit? aloc abullet))
                                         (centipede-vdir (which-hit? aloc abullet))
                                         abullet
                                         (centipede-segs (which-hit? aloc abullet)))
                              (remove (which-hit? aloc abullet) aloc)))]))

;; which-hit? : LoC Bullet -> Centipede
(check-expect (which-hit? (list (make-centipede "right" "down"
                                                (list(make-posn 6 4.5)
                                                     (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5))))
                          (make-bullet (make-posn 6 4.5) true))
              (make-centipede "right" "down" (list (make-posn 6 4.5)
                                                   (make-posn 5 4.5))))
(check-expect (which-hit? (list (make-centipede "right" "down"
                                                (list(make-posn 6 4.5)
                                                     (make-posn 5 4.5)))
                                (make-centipede "right" "down"
                                                (list (make-posn 10 4.5)
                                                      (make-posn 9 4.5))))
                          (make-bullet (make-posn 5 4.5) true))
              (make-centipede "right" "down" (list (make-posn 6 4.5)
                                                   (make-posn 5 4.5))))
(define (which-hit? aloc abullet)
  (if (cent-hit? (centipede-segs (first aloc)) abullet)
      (first aloc)
      (which-hit? (rest aloc) abullet)))

;; first-centipede : HDir VDir Bullet LoPosn -> Centipede
;; consumes an HDir, a VDir, a Bullet
;; and a List of Positions and creates a centipede 

(check-expect (first-centipede "right" "down"
                               (make-bullet (make-posn 7 4.5) #t)
                               (list (make-posn 9 4.5)
                                      (make-posn 8 4.5)
                                      (make-posn 7 4.5)
                                      (make-posn 6 4.5)
                                      (make-posn 5 4.5)))
              (make-centipede "right" "down" (list (make-posn 9 4.5)
                                                   (make-posn 8 4.5))))

(define (first-centipede anhdir avdir abullet aloposn)
  (make-centipede anhdir avdir (first-segbreaker aloposn abullet)))

;; rest-centipede : HDir VDir Bullet LoPosn -> Centipede
;; consumes an HDir, a VDir, a Bullet,
;; and a List of Positions and creates a centipede 

(check-expect (rest-centipede "right" "down"
                               (make-bullet (make-posn 7 4.5) #t)
                               (list (make-posn 9 4.5)
                                      (make-posn 8 4.5)
                                      (make-posn 7 4.5)
                                      (make-posn 6 4.5)
                                      (make-posn 5 4.5)))
              (make-centipede "left" "down" (list (make-posn 5 4.5)
                                                   (make-posn 6 4.5))))
(check-expect (rest-centipede "right" "down"
                               (make-bullet (make-posn 5 4.5) #t)
                               (list (make-posn 9 4.5)
                                      (make-posn 8 4.5)
                                      (make-posn 7 4.5)
                                      (make-posn 6 4.5)
                                      (make-posn 5 4.5)))
              (make-centipede "left" "down" empty))

(define (rest-centipede anhdir avdir abullet aloposn)
  (make-centipede (if (string=? anhdir "right")
                      "left"
                      "right")
                  avdir
                  (reverse (rest-segbreaker aloposn abullet))))

;; first-segbreaker : LoPosn Bullet -> LoPosn
;; consumes a hit list of centipede segments
;;  and outputs the segments before the hit

(check-expect (first-segbreaker (list (make-posn 9 4.5)
                                      (make-posn 8 4.5)
                                      (make-posn 7 4.5)
                                      (make-posn 6 4.5)
                                      (make-posn 5 4.5))
                                (make-bullet (make-posn 7 4.5) true))
              (list (make-posn 9 4.5)
                    (make-posn 8 4.5)))

(define (first-segbreaker aloposn abullet)
  (cond [(not (posn=? (bullet-posn abullet)
              (first aloposn)))
      (cons (first aloposn)
            (first-segbreaker (rest aloposn) abullet))]
      [else '()]))
  

;;rest-segbreaker : LoPosn Bullet -> LoPosn
;; consumes a hit list of centipede segments
;;  and outputs the segments after the hit
(check-expect (rest-segbreaker (list (make-posn 9 4.5)
                                      (make-posn 8 4.5)
                                      (make-posn 7 4.5)
                                      (make-posn 6 4.5)
                                      (make-posn 5 4.5))
                                (make-bullet (make-posn 7 4.5) true))
              (list (make-posn 6 4.5)
                    (make-posn 5 4.5)))
(check-expect (rest-segbreaker (list (make-posn 9 4.5)
                                      (make-posn 8 4.5)
                                      (make-posn 7 4.5)
                                      (make-posn 6 4.5)
                                      (make-posn 5 4.5))
                                (make-bullet (make-posn 5 4.5) true))
              empty)

(define (rest-segbreaker aloposn abullet)
  (cond
    [(empty? aloposn) empty]
    [(posn=? (bullet-posn abullet)
                 (first aloposn))
         (rest aloposn)]
      [else (rest-segbreaker (rest aloposn) abullet)]))

;;loc-hit? : LoC Bullet -> Boolean
;checks if any centipedes in the list are hit by the bullet
(check-expect (loc-hit? (list (make-centipede "right" "up"
                                              (list (make-posn 1 2)))
                              (make-centipede "right" "up"
                                              (list (make-posn 1 6))))
                        (make-bullet (make-posn 1 2) true))
              true)
(check-expect (loc-hit? (list (make-centipede "right" "up"
                                              (list (make-posn 1 4)))
                              (make-centipede "right" "up"
                                              (list (make-posn 1 6))))
                        (make-bullet (make-posn 1 2) true))
              false)

(define (loc-hit? aloc abullet)
  (cond
    [(empty? aloc) false]
    [(cons? aloc) (or (cent-hit? (centipede-segs (first aloc)) abullet) 
                  (loc-hit? (rest aloc) abullet))]))

;; cent-hit? : LoPosn Bullet -> Boolean
;;checks if the centipede is hit by bullet

(check-expect (cent-hit? (list (make-posn 1 2)
                               (make-posn 3 4)
                               (make-posn 1 5))
                         (make-bullet (make-posn 1 2) #true))
              #true)
(check-expect (cent-hit? (list (make-posn 1 2)
                               (make-posn 3 4)
                               (make-posn 1 5))
                         (make-bullet (make-posn 1 3) #true))
              #false)

(define (cent-hit? aloposn abullet)
  (cond
    [(empty? aloposn) false]
    [(cons? aloposn) (or (posn=? (bullet-posn abullet)
                                  (first aloposn)) 
                          (cent-hit? (rest aloposn) abullet))]))

;; move-loc : LoC -> LoC
;;Moves all centipedes
(check-expect (move-loc empty (list (make-mush
                                     (make-posn 1 2) 4))) empty)
(check-expect (move-loc (list (make-centipede "right" "down" (list (make-posn 3 .5)
                                                            (make-posn 2 .5)
                                                            (make-posn 1 .5)))
                              (make-centipede "right" "down" (list (make-posn 3 4.5)
                                                            (make-posn 2 4.5)
                                                            (make-posn 1 4.5))))
                        (list (make-mush
                                     (make-posn 1 2) 4)))
              (list (make-centipede "right" "down" (list (make-posn 4 .5)
                                                            (make-posn 3 .5)
                                                            (make-posn 2 .5)))
                    (make-centipede "right" "down" (list (make-posn 4 4.5)
                                                            (make-posn 3 4.5)
                                                            (make-posn 2 4.5)))))
(define (move-loc aloc alom)
  (cond
    [(empty? aloc) empty]
    [(cons? aloc) (cons (move-centipede (first aloc) alom)
                        (move-loc (rest aloc) alom))]))


;; move-centipede : Centipede -> Centipede
;; Moves a centipede depending on its direction and position on the grid
;;Right No Edge
#|
#;(check-expect (move-centipede (make-centipede "right" "down" (list (make-posn 3 .5)
                                                            (make-posn 2 .5)
                                                            (make-posn 1 .5))))
              (make-centipede "right" "down" (list (make-posn 4 .5)
                                                   (make-posn 3 .5)
                                                   (make-posn 2 .5))))
#;(check-expect (move-centipede (make-centipede "right" "up" (list (make-posn 3 3.5)
                                                            (make-posn 2 3.5)
                                                            (make-posn 1 3.5))))
              (make-centipede "right" "up" (list (make-posn 4 3.5)
                                                   (make-posn 3 3.5)
                                                   (make-posn 2 3.5))))
;;Left No Edge

(check-expect (move-centipede (make-centipede "left" "down"(list (make-posn 3 1.5)
                                                                 (make-posn 4 1.5))))
              (make-centipede "left" "down" (list (make-posn 2 1.5)
                                                  (make-posn 3 1.5))))
(check-expect (move-centipede (make-centipede "left" "up"(list (make-posn 3 1.5)
                                                               (make-posn 4 1.5))))
              (make-centipede "left" "up" (list (make-posn 2 1.5)
                                                (make-posn 3 1.5))))

;;Right Edge Not Top/Bottom
(check-expect (move-centipede (make-centipede "right" "down" (list (make-posn 25 3.5)
                                                                   (make-posn 24 3.5)
                                                                   (make-posn 23 3.5))))
              (make-centipede "left" "down" (list (make-posn 25 4.5)
                                                  (make-posn 25 3.5)
                                                  (make-posn 24 3.5))))

#;(check-expect (move-centipede (make-centipede "left" "down" (list (make-posn 25 4.5)
                                                                   (make-posn 25 3.5)
                                                                   (make-posn 24 3.5))))
              (make-centipede "left" "down" (list (make-posn 24 4.5)
                                                  (make-posn 25 4.5)
                                                  (make-posn 25 3.5))))
(check-expect (move-centipede (make-centipede "right" "up" (list (make-posn 25 3.5)
                                                                   (make-posn 24 3.5)
                                                                   (make-posn 23 3.5))))
              (make-centipede "left" "up" (list (make-posn 25 2.5)
                                                  (make-posn 25 3.5)
                                                  (make-posn 24 3.5))))
;;Left Edge Not Top/Bottom
(check-expect (move-centipede (make-centipede "left" "down" (list (make-posn 0 3.5)
                                                                   (make-posn 1 3.5)
                                                                   (make-posn 2 3.5))))
              (make-centipede "right" "down" (list (make-posn 0 4.5)
                                                  (make-posn 0 3.5)
                                                  (make-posn 1 3.5))))
(check-expect (move-centipede (make-centipede "left" "down" (list (make-posn 0 3.5)
                                                                   (make-posn 1 3.5)
                                                                   (make-posn 2 3.5))))
              (make-centipede "right" "down" (list (make-posn 0 4.5)
                                                  (make-posn 0 3.5)
                                                  (make-posn 1 3.5))))
(check-expect (move-centipede (make-centipede "left" "up" (list (make-posn 0 3.5)
                                                                   (make-posn 1 3.5)
                                                                   (make-posn 2 3.5))))
              (make-centipede "right" "up" (list (make-posn 0 2.5)
                                                  (make-posn 0 3.5)
                                                  (make-posn 1 3.5))))

;;Right Edge Top/Bottom 
(check-expect (move-centipede (make-centipede "right" "up" (list (make-posn 25 (- GRID-HEIGHT .5))
                                                                   (make-posn 24 (- GRID-HEIGHT .5))
                                                                   (make-posn 23 (- GRID-HEIGHT .5)))))
              (make-centipede "left" "up" (list (make-posn 25 (- GRID-HEIGHT 1.5))
                                                  (make-posn 25 (- GRID-HEIGHT .5))
                                                  (make-posn 24 (- GRID-HEIGHT .5)))))
(check-expect (move-centipede (make-centipede "right" "down" (list (make-posn 25 .5)
                                                                   (make-posn 24 .5)
                                                                   (make-posn 23 .5))))
              (make-centipede "left" "down" (list (make-posn 25 1.5)
                                                  (make-posn 25 .5)
                                                  (make-posn 24 .5))))

;;Left Edge Top/Bottom
(check-expect (move-centipede (make-centipede "left" "up" (list (make-posn 0 (- GRID-HEIGHT .5))
                                                                   (make-posn 1 (- GRID-HEIGHT .5))
                                                                   (make-posn 2 (- GRID-HEIGHT .5)))))
              (make-centipede "right" "up" (list (make-posn 0 (- GRID-HEIGHT 1.5))
                                                  (make-posn 0 (- GRID-HEIGHT .5))
                                                  (make-posn 1 (- GRID-HEIGHT .5)))))
(check-expect (move-centipede (make-centipede "left" "down" (list (make-posn 0 .5)
                                                                   (make-posn 1 .5)
                                                                   (make-posn 2 .5))))
              (make-centipede "right" "down" (list (make-posn 0 1.5)
                                                  (make-posn 0 .5)
                                                  (make-posn 1 .5))))


|#

(define (move-centipede acentipede alom)
  (make-centipede
   (hdir-determiner (centipede-hdir acentipede)
                    (centipede-segs acentipede)
                    alom)
   (vdir-determiner (centipede-vdir acentipede)
                    (centipede-segs acentipede))
   (drop-last (add-new-head (centipede-hdir acentipede)
                            (centipede-vdir acentipede)
                            (centipede-segs acentipede)
                            alom))))

;; drop-last HDir VDir LoPosn -> LoPosn
;;Adds a head to the list of posns

(define (add-new-head h v lp alom)
  (if (empty? lp)
      lp
      (cons (new-head h v (first lp) alom)
            lp)))

;; new-head HDir VDir Posn -> Posn
;; Creates a head

(define (new-head h v p alom)
  (cond
    [(and (string=? h "right")
          (or (= (posn-x p) 25)
              (lom=? h p alom)))
     (make-posn (posn-x p)
                (+ (if (string=? v "down") 1 -1)
                   (posn-y p)))]
    [(and (string=? h "right") (not (= (posn-x p) 25)))
     (make-posn (add1 (posn-x p))
                (posn-y p))]
    [(and (string=? h "left")
          (or (= (posn-x p) 0)
              (lom=? h p alom)))
     (make-posn (posn-x p)
                (+ (if (string=? v "down") 1 -1)
                   (posn-y p)))]
    [(and (string=? h "left") (not (= (posn-x p) 0)))
     (make-posn (sub1 (posn-x p))
                (posn-y p))]))

;; lom=? : HDir Posn LoM -> Boolean

(define (lom=? h p alom)
  (cond
    [(empty? alom) false]
  [(cons? alom)
   (or (mush=? h p (first alom)) (lom=? h p (rest alom)))]))

;; mush=? : HDir Posn Mushroom -> Boolean

(define (mush=? h p m)
  (cond
    [(string=? h "left")
     (and (= (- (posn-x p) 1) (posn-x (mush-posn m)))
          (= (posn-y p) (posn-y (mush-posn m))))]
    [(string=? h "right")
     (and (= (+ (posn-x p) 1) (posn-x (mush-posn m)))
          (= (posn-y p) (posn-y (mush-posn m))))]))

;; drop-last : LoPosn -> LoPosn
;; drops last posn in a list of posns

(define (drop-last lp)
  (cond
    [(empty? lp) empty]
    [(empty? (rest lp)) empty]
    [(cons? lp) (cons (first lp)
                      (drop-last (rest lp)))]))

;; update-hdir HDir -> HDir
(define (update-hdir h)
  (if (string=? h "right")
      "left"
      "right"))



;; hdir-determiner : HDir LoPosn -> HDir
;; Determines whether an HDir should change
(check-expect (hdir-determiner "right" (list (make-posn 5 3.5))
                               (list (make-mush (make-posn 1 2) 4)))
              "right")
(check-expect (hdir-determiner "left" (list (make-posn 5 3.5))
                               (list (make-mush (make-posn 1 2) 4)))
              "left")
(check-expect (hdir-determiner "right" (list (make-posn 25 3.5))
                               (list (make-mush (make-posn 1 2) 4)))
              "left")
(check-expect (hdir-determiner "left" (list (make-posn 0 3.5))
                               (list (make-mush (make-posn 1 2) 4)))
              "right")

(define (hdir-determiner anhdir aloposn alom)
  (cond
    [(empty? aloposn) "right"]
    [(and (string=? anhdir "right")
          (not (= 25 (posn-x (first aloposn))))
          (not (lom=? anhdir (first aloposn) alom)))
     "right"]
    [(and (string=? anhdir "left")
          (not (= 0 (posn-x (first aloposn))))
          (not (lom=? anhdir (first aloposn) alom)))
     "left"]
    [(and (string=? anhdir "right")
          (or (= 25 (posn-x (first aloposn)))
              (lom=? anhdir (first aloposn) alom)))
     "left"]
    [(and (string=? anhdir "left")
          (or (= 0 (posn-x (first aloposn)))
              (lom=? anhdir (first aloposn) alom)))
     "right"]))

;; vdir-determiner : VDir LoPosn -> VDir
;; Determines whether a VDir should change
(check-expect (vdir-determiner "down" (list (make-posn 3 4.5)))
              "down")
(check-expect (vdir-determiner "up" (list (make-posn 3 4.5)))
              "up")
(check-expect (vdir-determiner "down" (list (make-posn 25 (- GRID-HEIGHT .5))))
              "up")
(check-expect (vdir-determiner "up" (list (make-posn 25 .5)))
              "down")
 
(define (vdir-determiner avdir aloposn)
 (cond
   [(empty? aloposn) "down"]
   [(and (string=? avdir "down")
         (not (= (- GRID-HEIGHT .5) (posn-y (first aloposn)))))
    "down"]
   [(and (string=? avdir "up")
         (not (= .5 (posn-y (first aloposn)))))
    "up"]
   [(and (string=? avdir "down")
         (= (- GRID-HEIGHT .5) (posn-y (first aloposn))))
    "up"]
   [(and (string=? avdir "up")
         (= .5 (posn-y (first aloposn))))
    "down"]))



; Posn Posn -> Boolean
; Posns equal?

(check-expect (posn=? (make-posn 1 2)
                      (make-posn 1 2))
              #true)
(check-expect (posn=? (make-posn 1 2)
                      (make-posn 3 2))
              #false)
              
(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

;; ____________________________________________________________________________
;; UPDATE BULLET FUNCTION
;; ____________________________________________________________________________

;; update-bullet : Bullet LoC Player -> Bullet
;; Updates the bullet

(check-expect (update-bullet (make-bullet (make-posn 1 2) false)
                             (list (make-centipede "right" "down"
                                                   (list (make-posn 6 7)
                                                         (make-posn 5 7))))
                             (make-player (make-posn 5 PLAYER-Y))
                             (list (make-mush (make-posn 1 2) 4)
                                   (make-mush (make-posn 5 2) 4)))
              (make-bullet (make-posn 5 PLAYER-Y) false))

(check-expect (update-bullet (make-bullet (make-posn 1 2) true)
                             (list (make-centipede "right" "down"
                                                   (list (make-posn 6 7)
                                                         (make-posn 5 7))))
                             (make-player (make-posn 5 PLAYER-Y))
                             (list (make-mush (make-posn 1 2) 4)
                                   (make-mush (make-posn 5 2) 4)))
              (make-bullet (make-posn 5 39.5) false))

(check-expect (update-bullet (make-bullet (make-posn 6 7) true)
                             (list (make-centipede "right" "down"
                                                   (list (make-posn 6 7)
                                                         (make-posn 5 7))))
                             (make-player (make-posn 5 PLAYER-Y))
                             (list (make-mush (make-posn 1 2) 4)
                                   (make-mush (make-posn 5 2) 4)))
              (make-bullet (make-posn 5 PLAYER-Y) false))

(check-expect (update-bullet (make-bullet (make-posn 1 -.5) true)
                             (list (make-centipede "right" "down"
                                                   (list (make-posn 6 7)
                                                         (make-posn 5 7))))
                             (make-player (make-posn 5 PLAYER-Y))
                             (list (make-mush (make-posn 1 2) 4)
                                   (make-mush (make-posn 5 2) 4)))
              (make-bullet (make-posn 5 PLAYER-Y) false))


(define (update-bullet abullet aloc aplayer alom)
  (cond
    [(not (bullet-fired abullet))
     (make-bullet (make-posn (posn-x (player-posn aplayer))
                             PLAYER-Y)
                  false)]
    [(and (bullet-fired abullet)
          (not (or (loc-hit? aloc abullet)
                   (mush-hit? alom abullet)))
          (not (= -.5 (posn-y (bullet-posn abullet)))))
     (make-bullet (make-posn (posn-x (bullet-posn abullet))
                             (sub1 (posn-y (bullet-posn abullet))))
                  true)]
    [(and (bullet-fired abullet)
          (or (loc-hit? aloc abullet)
              (mush-hit? alom abullet)))
     (make-bullet (make-posn (posn-x (player-posn aplayer))
                             PLAYER-Y)
                  false)]
    [(and (bullet-fired abullet) (= -.5 (posn-y (bullet-posn abullet))))
     (make-bullet (make-posn (posn-x (player-posn aplayer))
                             PLAYER-Y)
                  false)]))

;; mush-hit? : LoM Bullet -> Boolean
;; Checks if a mushroom is hit

(define (mush-hit? alom b)
  (cond
    [(empty? alom) false]
    [(cons? alom) (or (posn=? (bullet-posn b)
                               (mush-posn (first alom)))
                       (mush-hit? (rest alom) b))]))

;; ____________________________________________________________________________
;; UPDATE GAMESTATE FUNCTION
;; ____________________________________________________________________________

;; update-gamestate : World -> Gamestate
;; Updates the gamestate
(check-expect (update-gamestate WORLD0)
              "play")
(check-expect (update-gamestate
               (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn
                                             (+ .5 (* .5 GRID-WIDTH))
                                             PLAYER-Y))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                        PLAYER-Y)
                             false)
                (make-mush (make-posn 1 2) 3)
                "play"
                0))
              "lose")

(check-expect (update-gamestate
               (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      '()))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                        PLAYER-Y)
                             false)
                (make-mush (make-posn 1 2) 3)
                "play"
                0))
               "win")
               
(define (update-gamestate aworld)
  (cond
    [(dead-centipedes (world-loc aworld)) "win"]
    [(dead-player aworld) "lose"]
    [else "play"]))

;; dead-centipedes : LoC -> Boolean
;; Reports whether all centipedes are dead (are empty/has no segments)

(check-expect (dead-centipedes (list (make-centipede "right" "down"
                                      (make-posn 2 .5))
                                     (make-centipede "right" "down"
                                      (make-posn 1 .5))))
              false)

(check-expect (dead-centipedes (list (make-centipede "right" "down"
                                      '())
                                     (make-centipede "right" "down"
                                      '())))
              true)

(define (dead-centipedes aloc)
  (cond [(empty? aloc) true]
        [else (and (dead-centipede (first aloc))
                   (dead-centipedes (rest aloc)))]))

;; dead-centipede : Centipede -> Boolean
;; Reports whether a centipede is dead (is empty/has no segments)

(check-expect (dead-centipede (make-centipede "right" "down"
                                      (make-posn 1 .5)))
              false)

(check-expect (dead-centipede (make-centipede "right" "down"
                                      '()))
              true)

(define (dead-centipede acentipede)
  (cond
    [(empty? (centipede-segs acentipede)) true]
    [else false]))
  

;; dead-player : World -> Boolean
;; reports whether the player is dead (player pos = centipede head pos)

(check-expect (dead-player WORLD0) false)
(check-expect (dead-player (make-world
                            (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                            (list (make-centipede "right" "down"
                                   (list (make-posn
                                          (+ .5 (* .5 GRID-WIDTH))
                                          PLAYER-Y))))
                            (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                                    PLAYER-Y)
                                         false)
                            (make-mush (make-posn 1 2) 3)
                            "play"
                            0))
              true)

(define (dead-player aworld)
  (player-hit? (world-player aworld) (world-loc aworld)))

;; player-hit? : Player LoC -> Boolean
;;Checks if the player was hit by any centipede

(check-expect (player-hit? (world-player WORLD0)
                           (world-loc WORLD0))
              false)
(check-expect (player-hit? (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                            (list (make-centipede "right" "down"
                                   (list (make-posn
                                          (+ .5 (* .5 GRID-WIDTH))
                                          PLAYER-Y)))))
              true)
          
(define (player-hit? aplayer aloc)
  (cond
    [(empty? aloc) false]
    [(cons? aloc) (or  (centipedehead aplayer (first aloc))
                        (player-hit? aplayer (rest aloc)))]))

(define (centipedehead aplayer acentipede)
  (cond [(empty? (centipede-segs acentipede)) false]
        [else (posn=? (player-posn aplayer)
                 (first (centipede-segs acentipede)))]))
  

;; ____________________________________________________________________________
;; UPDATE TIMER FUNCTION
;; ____________________________________________________________________________

;; update-timer : Number -> Number
;; updates the timer of a world
(check-expect (update-timer 1) 2)

(define (update-timer atimer)
  (add1 atimer))

;; ____________________________________________________________________________
;; ON-KEY PLAYER-INPUT FUNCTION
;; ____________________________________________________________________________

;;player-input : World KeyEvent -> World
;;Updates the world on a key input

;;MOVE PLAYER LEFT
(check-expect (player-input WORLD0 "left")
              (make-world
                (make-player (make-posn (- (+ .5 (* .5 GRID-WIDTH)) 1) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 10 .5)
                                            (make-posn 9 .5)
                                            (make-posn 8 .5)
                                            (make-posn 7 .5)
                                            (make-posn 6 .5)
                                            (make-posn 5 .5)
                                            (make-posn 4 .5)
                                            (make-posn 3 .5)
                                            (make-posn 2 .5)
                                            (make-posn 1 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)
                             false)
                MUSH-LIST
                "play"
                0))
;; MOVE PLAYER RIGHT
(check-expect (player-input WORLD0 "right")
              (make-world
                (make-player (make-posn (+ (+ .5 (* .5 GRID-WIDTH)) 1) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 10 .5)
                                            (make-posn 9 .5)
                                            (make-posn 8 .5)
                                            (make-posn 7 .5)
                                            (make-posn 6 .5)
                                            (make-posn 5 .5)
                                            (make-posn 4 .5)
                                            (make-posn 3 .5)
                                            (make-posn 2 .5)
                                            (make-posn 1 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)
                             false)
                MUSH-LIST
                "play"
                0))
                
;;FIRE BULLET
(check-expect (player-input WORLD0 " ")
              (make-world
                (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                (list (make-centipede "right" "down"
                                      (list (make-posn 10 .5)
                                            (make-posn 9 .5)
                                            (make-posn 8 .5)
                                            (make-posn 7 .5)
                                            (make-posn 6 .5)
                                            (make-posn 5 .5)
                                            (make-posn 4 .5)
                                            (make-posn 3 .5)
                                            (make-posn 2 .5)
                                            (make-posn 1 .5))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)
                             true)
                MUSH-LIST
                "play"
                0))


(define (player-input aworld ke)
  (cond
    [(and (key=? ke "left") (> (posn-x (player-posn (world-player aworld))) 1))
     (make-world (make-player (make-posn (sub1 (posn-x (player-posn (world-player aworld))))
                              (posn-y (player-posn (world-player aworld)))))
                 (world-loc aworld)
                 (world-bullet aworld)
                 (world-lom aworld)
                 (world-gamestate aworld)
                 (world-timer aworld))]
    [(and (key=? ke "right") (> (sub1 GRID-WIDTH) (posn-x (player-posn (world-player aworld)))))
     (make-world (make-player (make-posn (add1 (posn-x (player-posn (world-player aworld))))
                              (posn-y (player-posn (world-player aworld)))))
                 (world-loc aworld)
                 (world-bullet aworld)
                 (world-lom aworld)
                 (world-gamestate aworld)
                 (world-timer aworld))]
    [(and (key=? ke " ") (not (bullet-fired (world-bullet aworld))))
     (make-world (world-player aworld)
                 (world-loc aworld)
                 (make-bullet (make-posn (posn-x (player-posn (world-player aworld)))
                              PLAYER-Y)
                              true)
                 (world-lom aworld)
                 (world-gamestate aworld)
                 (world-timer aworld))]
    [else aworld]))
    
                              
;; ____________________________________________________________________________
;; BIG-BANG MAIN FUNCTION
;; ____________________________________________________________________________

(define (main w0)
  (big-bang w0
            [to-draw render]
            [on-tick update-world]
            [on-key player-input]))

(main WORLD0)
































