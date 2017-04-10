;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS2500 Problem Set 10 - Seth Carroll & Kevin Khomiakov|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; PROBLEM SET 10
;; Seth Carroll and Kevin Khomiakov

(require 2htdp/image)
(require 2htdp/universe)

;; Data Definitions
;; A Dir is one of:
;; - HDir
;; - VDir

;; An HDir is one of:
;; -"left"
;; -"right"

;; A VDir is one of:
;; -"up"
;; -"down"

;; A Mushroom is a (make-mush Posn Number)
(define-struct mush [posn state])

;;A Centipede is a (make-centipede Dir Dir [List-of Posn])
(define-struct centipede [hdir vdir segs])
;  - Centipede head is the first segment in list.

;;A Player is a (make-player posn)
(define-struct player [posn])

;; A Bullet is a (make-bullet Posn Boolean)
;; bullet-fired indicates whether a bullet has been fired
(define-struct bullet [posn fired])

;; A Gamestate is one of:
;; - "win"
;; - "lose"
;; - "play"

;; A World is a (make-world
;; Player [List-of Centipede] Bullet [List-of Mushroom] Gamestate Number)
;; timer : counts up starting from 0 every time update function runs
(define-struct world [player loc bullet lom gamestate timer])


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
  (build-list 30
              (lambda (x)
                (make-mush
                 (make-posn (+ 1 (random (- GRID-WIDTH 1)))
                            (+ -.5 (random GRID-HEIGHT))) 4))))

(define TESTWORLD (make-world
                   (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                   (list
                    (make-centipede
                     "right" "down"
                     (reverse
                      (build-list 10 (lambda (x) (make-posn (+ x 1) .5))))))
                   (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                           PLAYER-Y)
                                false)
                   MUSH-LIST
                   "play"
                   0))

;; ____________________________________________________________________________
;; TO-DRAW RENDER FUNCTION
;; ____________________________________________________________________________

;; render : World -> Image
;; Draws the world.
(check-expect (render (make-world
                       (make-player
                        (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
                       (list (make-centipede "right" "down"
                                             '())
                             (make-centipede "right" "down"
                                             '()))
                       (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                               PLAYER-Y)
                                    false)
                       (list (make-mush (make-posn 1 2) 3))
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
                       (list (make-mush (make-posn 1 2) 3))
                       "lose"
                       0))
              (overlay LOSER BG))

(check-expect
 (render (make-world
          (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
          (list (make-centipede "right" "down"
                                (list (make-posn 5 .5)
                                      (make-posn 4 .5))))
          (make-bullet (make-posn 10 10)
                       true)
          (list (make-mush (make-posn 1 2) 3))
          "play"
          10))
 (place-image/grid
  RIGHT-HEAD
  5 .5
  (place-image/grid
   CENTIPEDE-CELL
   4 .5
   (place-image/grid
    PLAYER
    (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y
    (place-image/grid BULLET 10 10
                      (place-image/grid
                       (circle (/ CELL-SIZE 2) 'solid MUSHROOM-3-C) 1 2
                       BG))))))

(define (render aworld)
  (cond [(string=? "win" (world-gamestate aworld))
         (overlay WINNER BG)]
        [(string=? "lose" (world-gamestate aworld))
         (overlay LOSER BG)]
        [(string=? "play" (world-gamestate aworld))
         (draw-centipedes
          (world-loc aworld)
          (draw-player (world-player aworld)
                       (draw-mushrooms (world-lom aworld)
                                       (draw-bullet (world-bullet aworld)
                                                    BG))))]))

;; draw-mushrooms : [List-of Mushroom] Image -> Image
;; places mushrooms on the scene
(check-expect (draw-mushrooms '() BG) BG)
(check-expect (draw-mushrooms (list (make-mush (make-posn 10 10) 4)
                                    (make-mush (make-posn 10 20) 3)
                                    (make-mush (make-posn 10 30) 2)
                                    (make-mush (make-posn 10 35) 1)) BG)
              (place-image/grid
               (circle (/ CELL-SIZE 2) 'solid MUSHROOM-4-C) 10 10
               (place-image/grid
                (circle (/ CELL-SIZE 2) 'solid MUSHROOM-3-C) 10 20
                (place-image/grid
                 (circle (/ CELL-SIZE 2) 'solid MUSHROOM-2-C) 10 30
                 (place-image/grid
                  (circle (/ CELL-SIZE 2) 'solid MUSHROOM-1-C) 10 35 BG)))))

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

;; draw-centipedes: [List-of Centipede] Image -> Image
;; adds the centipedes to the scene
(check-expect (draw-centipedes '() BG) BG)
(check-expect (draw-centipedes (list (make-centipede "right" "down"
                                                     (list (make-posn 6 .5)
                                                           (make-posn 5 .5)))
                                     (make-centipede "left" "down"
                                                     (list (make-posn 3 .5)
                                                           (make-posn 4 .5))))
                               BG)
              (place-image/grid
               RIGHT-HEAD 6 .5
               (place-image/grid
                CENTIPEDE-CELL 5 .5
                (place-image/grid LEFT-HEAD 3 .5
                                  (place-image/grid CENTIPEDE-CELL 4 .5
                                                    BG)))))

(define (draw-centipedes aloc ascene)
  (local (;; draw-centipede : Centipede Image -> Image
          ;; adds a centipede to the scene
          (define (draw-centipede acentipede ascene)
            (draw-head (first (centipede-segs acentipede))
                       (centipede-hdir acentipede)
                       (draw-body (rest (centipede-segs acentipede))
                                  ascene)))
          
          ;; draw-head : Posn Dir Image -> Image
          ;; Draws segments on the scene
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
          
          ;; draw-body : [List-of Posn] Image -> Image
          ;; Draws body segments on the scene
          (define (draw-body aloposn ascene)
            (foldr draw-seg ascene aloposn))
          
          ;;draw-seg : Posn Image -> Image
          ;; draws individual body segment on the scene
          (define (draw-seg aposn ascene)
            (place-image/grid CENTIPEDE-CELL
                              (posn-x aposn) (posn-y aposn) ascene)))
          
    (cond [(empty? aloc) ascene]
          [(cons? aloc) (if (not (empty? (centipede-segs (first aloc))))
                            (draw-centipede
                             (first aloc)
                             (draw-centipedes (rest aloc) ascene))
                            (draw-centipedes (rest aloc) ascene))])))

;; draw-player : Player Image -> Image
;; adds the player to the scene
(check-expect (draw-player
               (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)) BG)
              (place-image/grid PLAYER (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y BG))

(define (draw-player aplayer ascene)
  (place-image/grid PLAYER
                    (posn-x (player-posn aplayer))
                    (posn-y (player-posn aplayer))
                    ascene))

;; draw-bullet : Bullet Image -> Image
;; adds the bullet to the scene if bullet-fired = true
(check-expect (draw-bullet (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                                   PLAYER-Y) true) BG)
              (place-image/grid BULLET (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y BG))
(check-expect (draw-bullet
               (make-bullet
                (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y) false) BG) BG)


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
  (place-image grid-image (* x CELL-SIZE) (* y CELL-SIZE) bg))


;; ____________________________________________________________________________
;; ON-TICK UPDATE-WORLD FUNCTION
;; ____________________________________________________________________________

(check-expect (update-world TESTWORLD)
              (make-world
               (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
               (list (make-centipede
                      "right" "down"
                      (reverse
                       (build-list 10 (lambda (x) (make-posn (+ x 2) .5))))))
               (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                       PLAYER-Y)
                            false)
                MUSH-LIST
                "play"
                1))

;; update : World -> World
;; updates the world state
(define (update-world aworld)
  (local (;; update-gamestate : World -> Gamestate
          ;; Updates the gamestate
          (define (update-gamestate aworld)
            (cond
              [(dead-centipedes (world-loc aworld)) "win"]
              [(dead-player aworld) "lose"]
              [else "play"]))
          
          ;; update-timer : Number -> Number
          ;; updates the timer of a world
          (define (update-timer atimer)
            (add1 atimer)))
    
    (if (string=? "play" (world-gamestate aworld))
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
        aworld)))


;; dead-centipedes : [List-of Centipede] -> Boolean
;; Reports whether all centipedes are dead (are empty/has no segments)
(check-expect (dead-centipedes '()) true)
(check-expect (dead-centipedes (list (make-centipede "right" "down"
                                                     (make-posn 2 .5))
                                     (make-centipede "right" "down"
                                                     (make-posn 1 .5)))) false)
(check-expect (dead-centipedes (list (make-centipede "right" "down" '())
                                     (make-centipede "right" "down" '()))) true)

(define (dead-centipedes aloc)
  (local (;; dead-centipede : Centipede -> Boolean
          ;; Reports whether a centipede is dead (is empty/has no segments)
          
          (define (dead-centipede acentipede)
            (cond
              [(empty? (centipede-segs acentipede)) true]
              [else false])))
    (cond [(empty? aloc) true]
          [else (and (dead-centipede (first aloc))
                     (dead-centipedes (rest aloc)))])))

;; dead-player : World -> Boolean
;; reports whether the player is dead (player pos = centipede head pos)
(check-expect (dead-player TESTWORLD) false)
(check-expect (dead-player
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
              true)

(define (dead-player aworld)
  (local (;; player-hit? : Player [List-of Centipedes] -> Boolean
          ;;Checks if the player was hit by any centipede
          (define (player-hit? aplayer aloc)
            (cond
              [(empty? aloc) false]
              [(cons? aloc) (or  (centipedehead aplayer (first aloc))
                                 (player-hit? aplayer (rest aloc)))]))
          ;; centipedehead : Player Centipede -> Boolean
          ;; checks if a player and a centipede head meet
          (define (centipedehead aplayer acentipede)
            (cond [(empty? (centipede-segs acentipede)) false]
                  [else (posn=? (player-posn aplayer)
                                (first (centipede-segs acentipede)))])))
    
    (player-hit? (world-player aworld) (world-loc aworld))))

;; ____________________________________________________________________________
;; UPDATE [List-of Centipede] FUNCTION
;; ____________________________________________________________________________

;; update-loc : [List-of Centipede] Bullet Timer -> [List-of Centipede]
;;Updates the List of Centipedes
(check-expect (update-loc '() (make-bullet (make-posn 6 4.5) true) 5
                          (list (make-mush
                                 (make-posn 1 2) 4))) '())
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

(define (update-loc aloc abullet atimer alom)
  (cond
    [(empty? aloc) empty]
    [(and (and (cons? aloc)
               (= (modulo atimer 5) 0))
          (not (loc-hit? aloc abullet)))
     (move-loc aloc alom)]
    [(and (cons? aloc)
          (loc-hit? aloc abullet))
     (break-loc aloc abullet)]
    [else aloc]))

;;break-loc : [List-of Centipede] Bullet -> [List-of Centipede]
;; Creates a [List-of Centipede] of new centipedes after breaking
(check-expect (break-loc '() (make-bullet (make-posn 1 1) true)) '())
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
  (local (;; which-hit? : [List-of Centipede] Bullet -> Centipede
          ;; identifies which centipedes have been hit from a list of centipede
          (define (which-hit? aloc abullet)
            (if (cent-hit? (centipede-segs (first aloc)) abullet)
                (first aloc)
                (which-hit? (rest aloc) abullet)))
          
          ;; first-centipede : HDir VDir Bullet [List-of Posn] -> Centipede
          ;; consumes an HDir, a VDir, a Bullet
          ;; and a List of Positions and creates a centipede 
          (define (first-centipede anhdir avdir abullet aloposn)
            (make-centipede anhdir avdir (first-segbreaker aloposn abullet)))
          
          ;; rest-centipede : HDir VDir Bullet [List-of Posn] -> Centipede
          ;; consumes an HDir, a VDir, a Bullet,
          ;; and a List of Positions and creates a centipede
          (define (rest-centipede anhdir avdir abullet aloposn)
            (make-centipede (if (string=? anhdir "right")
                                "left"
                                "right")
                            avdir
                            (reverse (rest-segbreaker aloposn abullet))))
          ;; first-segbreaker : [List-of Posn] Bullet -> [List-of Posn]
          ;; consumes a hit list of centipede segments
          ;;  and outputs the segments before the hit
          (define (first-segbreaker aloposn abullet)
            (cond [(not (posn=? (bullet-posn abullet)
                                (first aloposn)))
                   (cons (first aloposn)
                         (first-segbreaker (rest aloposn) abullet))]
                  [else '()]))
          
          ;;rest-segbreaker : [List-of Posn] Bullet -> LoPosn
          ;; consumes a hit list of centipede segments
          ;;  and outputs the segments after the hit  
          (define (rest-segbreaker aloposn abullet)
            (cond
              [(empty? aloposn) empty]
              [(posn=? (bullet-posn abullet)
                 (first aloposn))
               (rest aloposn)]
              [else (rest-segbreaker (rest aloposn) abullet)])))
    (cond
      [(empty? aloc) empty]
      [(cons? aloc)
       (cons (first-centipede (centipede-hdir (which-hit? aloc abullet))
                              (centipede-vdir (which-hit? aloc abullet))
                              abullet
                              (centipede-segs (which-hit? aloc abullet)))
             (cons (rest-centipede (centipede-hdir (which-hit? aloc abullet))
                                   (centipede-vdir (which-hit? aloc abullet))
                                   abullet
                                   (centipede-segs (which-hit? aloc abullet)))
                   (remove (which-hit? aloc abullet) aloc)))])))

;;loc-hit? : [List-of Centipede] Bullet -> Boolean
;checks if any centipedes in the list are hit by the bullet
(check-expect (loc-hit? '() (make-bullet (make-posn 1 1) true)) false)

(check-expect (loc-hit? (list (make-centipede "right" "up"
                                              (list (make-posn 1 2)))
                              (make-centipede "right" "up"
                                              (list (make-posn 1 6))))
                        (make-bullet (make-posn 1 2) true)) true)
(check-expect (loc-hit? (list (make-centipede "right" "up"
                                              (list (make-posn 1 4)))
                              (make-centipede "right" "up"
                                              (list (make-posn 1 6))))
                        (make-bullet (make-posn 1 2) true)) false)

(define (loc-hit? aloc abullet)
  (ormap (lambda (x) (cent-hit? (centipede-segs x) abullet)) aloc))

;; cent-hit? : [List-of Posn] Bullet -> Boolean
;;checks if the centipede is hit by bullet
(check-expect (cent-hit? '() (make-bullet (make-posn 1 2) #true)) #false)

(check-expect (cent-hit? (list (make-posn 1 2)
                               (make-posn 3 4)
                               (make-posn 1 5))
                         (make-bullet (make-posn 1 2) #true)) #true)

(check-expect (cent-hit? (list (make-posn 1 2)
                               (make-posn 3 4)
                               (make-posn 1 5))
                         (make-bullet (make-posn 1 3) #true)) #false)

(define (cent-hit? aloposn abullet)
  (ormap (lambda (x) (posn=? x (bullet-posn abullet))) aloposn))

;; move-loc : [List-of Centipede] -> [List-of Centipede]
;;Moves all centipedes
(check-expect (move-loc empty (list (make-mush
                                     (make-posn 1 2) 4))) empty)
(check-expect
 (move-loc (list (make-centipede "right" "down" (list (make-posn 3 .5)
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
  (local (;; move-centipede : Centipede -> Centipede
          ;; Moves a centipede depending on its direction and position on grid
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
          ;; drop-last : [List-of Posn] -> [List-of Posn]
          ;; drops last posn in a list of posns
          (define (drop-last aloposn)
            (cond
              [(empty? aloposn) empty]
              [(empty? (rest aloposn)) empty]
              [(cons? aloposn) (cons (first aloposn)
                                (drop-last (rest aloposn)))])) 
          ;; add-new-head HDir VDir [List-of Posn] -> [List-of Posn]
          ;;Adds a head to the list of posns
          (define (add-new-head h v lp alom)
            (local (;; new-head HDir VDir Posn -> Posn
                    ;; Creates a head
                    (define (new-head h v p alom)
                      (cond
                        [(and (string=? h "right")
                              (or (= (posn-x p) 25)
                                  (lomposn=? h p alom)))
                         (make-posn (posn-x p)
                                    (+ (if (string=? v "down") 1 -1)
                                       (posn-y p)))]
                        [(and (string=? h "right") (not (= (posn-x p) 25)))
                         (make-posn (add1 (posn-x p))
                                    (posn-y p))]
                        [(and (string=? h "left")
                              (or (= (posn-x p) 0)
                                  (lomposn=? h p alom)))
                         (make-posn (posn-x p)
                                    (+ (if (string=? v "down") 1 -1)
                                       (posn-y p)))]
                        [(and (string=? h "left") (not (= (posn-x p) 0)))
                         (make-posn (sub1 (posn-x p))
                                    (posn-y p))])))
              (if (empty? lp) lp
                  (cons (new-head h v (first lp) alom)
                        lp)))))
    (map (lambda (x) (move-centipede x alom)) aloc)))

;; lomposn=? : HDir Posn [List-of Mushroom] -> Boolean
;; Checks if a posn's next placement is equal to
;;   any mushroom's posn in a [List-of Mushroom]
(check-expect (lomposn=? "right" (make-posn 1 1) '()) false)

(check-expect (lomposn=? "right" (make-posn 10 10)
                         (list (make-mush (make-posn 11 10) 4))) true)

(check-expect (lomposn=? "right" (make-posn 10 10)
                         (list (make-mush (make-posn 2 2) 4))) false)

(define (lomposn=? anhdir aposn alom)
  (local (;; mushposn=? : Dir Posn Mushroom -> Boolean
          ;; Checks if a posn is equal to a mushroom's posn
          
          (define (mushposn=? anhdir aposn amush)
            (cond
              [(string=? anhdir "left")
               (and (= (- (posn-x aposn) 1) (posn-x (mush-posn amush)))
                    (= (posn-y aposn) (posn-y (mush-posn amush))))]
              [(string=? anhdir "right")
               (and (= (+ (posn-x aposn) 1) (posn-x (mush-posn amush)))
                    (= (posn-y aposn) (posn-y (mush-posn amush))))])))
    (ormap (lambda (x) (mushposn=? anhdir aposn x)) alom)))

;; hdir-determiner : HDir [List-of Mushroom] [List-of Posn] -> HDir
;; Determines whether an HDir should change
(check-expect (hdir-determiner "left" empty empty) "right")
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
          (not (lomposn=? anhdir (first aloposn) alom)))
     "right"]
    [(and (string=? anhdir "left")
          (not (= 0 (posn-x (first aloposn))))
          (not (lomposn=? anhdir (first aloposn) alom)))
     "left"]
    [(and (string=? anhdir "right")
          (or (= 25 (posn-x (first aloposn)))
              (lomposn=? anhdir (first aloposn) alom)))
     "left"]
    [(and (string=? anhdir "left")
          (or (= 0 (posn-x (first aloposn)))
              (lomposn=? anhdir (first aloposn) alom)))
     "right"]))

;; vdir-determiner : VDir [List-of Mushroom] [List-of Posn] -> VDir
;; Determines whether a VDir should change
(check-expect (vdir-determiner "up" empty) "down")
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

;; update-bullet :
;;      Bullet [List-of Centipede] Player [List-of Mushroom] -> Bullet
;; Updates the bullet
(check-expect (update-bullet (make-bullet (make-posn 5 PLAYER-Y) false) '()
                             (make-player (make-posn 5 PLAYER-Y))
                             '())
              (make-bullet (make-posn 5 PLAYER-Y) false))

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

;; mush-hit? : [List-of Mushroom] Bullet -> Boolean
;; Checks if any mushroom on a list is hit
(check-expect (mush-hit? '() (make-bullet (make-posn 10 10) true)) false)
(check-expect (mush-hit? (list (make-mush (make-posn 10 10) 4))
                         (make-bullet (make-posn 10 10) true)) true)
(check-expect (mush-hit? (list (make-mush (make-posn 2 2) 4))
                         (make-bullet (make-posn 10 10) true))false)

(define (mush-hit? alom abullet)
  (ormap (lambda (x) (posn=? (mush-posn x) (bullet-posn abullet))) alom))

;;_____________________________________________________________________________
;; UPDATE [List-of Mushroom] FUNCTION
;;_____________________________________________________________________________

;; update-lom :
;;    [List-of Centipede] [List-of Mushroom] Bullet -> [List-of Mushroom]
;;Update a list of mushrooms
(check-expect (update-lom (world-loc TESTWORLD) '()
                          (make-bullet (make-posn 10 10) true)) '())
(check-expect (update-lom (world-loc TESTWORLD)
                          (list (make-mush (make-posn 1 2) 1)
                                (make-mush (make-posn 4 2) 4)
                                (make-mush (make-posn 1 9) 0))
                          (make-bullet (make-posn 1 2) true))
              (list (make-mush (make-posn 4 2) 4)))
(check-expect (update-lom (world-loc TESTWORLD)
                          (list (make-mush (make-posn 1 2) 1)
                                (make-mush (make-posn 4 2) 4)
                                (make-mush (make-posn 1 9) 0))
                          (make-bullet (make-posn 1 2) false))
              (list (make-mush (make-posn 4 2) 4)))

(define (update-lom aloc alom b)
  (local (;; damage-mush : Mushroom -> Mushroom
          ;;Decreases the state of the mushroom by 1
          (define (damage-mush m)
            (make-mush (mush-posn m)
                       (- (mush-state m) 1))))
    (cond
      [(empty? alom) empty]
      [(loc-hit? aloc b) (cons (make-mush (bullet-posn b)
                                          4) alom)]
      [(cons? alom)(filter (lambda (x) (not(= 0 (mush-state x))))
                           (map (lambda (x)
                                  (cond [(posn=? (mush-posn x) (bullet-posn b))
                                         (damage-mush x)]
                                        [else x])) alom))])))


;; ____________________________________________________________________________
;; ON-KEY PLAYER-INPUT FUNCTION
;; ____________________________________________________________________________

;;player-input : World KeyEvent -> World
;;Updates the world on a key input
(check-expect (player-input TESTWORLD "left")
              (make-world
               (make-player
                (make-posn (- (+ .5 (* .5 GRID-WIDTH)) 1) PLAYER-Y))
               (list (make-centipede
                      "right" "down"
                      (reverse
                       (build-list 10 (lambda (x) (make-posn (+ x 1).5))))))
               (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)
                            false)
               MUSH-LIST
               "play"
                0))
(check-expect (player-input TESTWORLD "right")
              (make-world
               (make-player
                (make-posn (+ (+ .5 (* .5 GRID-WIDTH)) 1) PLAYER-Y))
               (list (make-centipede
                      "right" "down"
                      (reverse
                       (build-list 10 (lambda (x) (make-posn (+ x 1) .5))))))
                (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)
                             false)
                MUSH-LIST
                "play"
                0))

(check-expect (player-input TESTWORLD " ")
              (make-world
               (make-player
                (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
               (list (make-centipede
                      "right" "down"
                      (reverse
                       (build-list 10 (lambda (x) (make-posn (+ x 1) .5))))))
               (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y)
                            true)
               MUSH-LIST
               "play"
               0))

(define (player-input aworld ke)
  (cond
    [(and (key=? ke "left")
          (> (posn-x (player-posn (world-player aworld))) 1))
     (make-world (make-player
                  (make-posn (sub1 (posn-x (player-posn (world-player aworld))))
                             (posn-y (player-posn (world-player aworld)))))
                 (world-loc aworld)
                 (world-bullet aworld)
                 (world-lom aworld)
                 (world-gamestate aworld)
                 (world-timer aworld))]
    [(and (key=? ke "right")
          (> (sub1 GRID-WIDTH) (posn-x (player-posn (world-player aworld)))))
     (make-world (make-player
                  (make-posn (add1 (posn-x (player-posn (world-player aworld))))
                             (posn-y (player-posn (world-player aworld)))))
                 (world-loc aworld)
                 (world-bullet aworld)
                 (world-lom aworld)
                 (world-gamestate aworld)
                 (world-timer aworld))]
    [(and (key=? ke " ") (not (bullet-fired (world-bullet aworld))))
     (make-world (world-player aworld)
                 (world-loc aworld)
                 (make-bullet
                  (make-posn (posn-x (player-posn (world-player aworld)))
                             PLAYER-Y)
                  true)
                 (world-lom aworld)
                 (world-gamestate aworld)
                 (world-timer aworld))]
    [else aworld]))                         
;; ____________________________________________________________________________
;; BIG-BANG MAIN FUNCTION
;; ____________________________________________________________________________

(define (main mushnumber centipedelength)
  (big-bang (make-world
             (make-player (make-posn (+ .5 (* .5 GRID-WIDTH)) PLAYER-Y))
             (list (make-centipede
                    "right" "down"
                    (reverse
                     (build-list centipedelength
                                 (lambda (x) (make-posn (+ x 1) .5))))))
             (make-bullet (make-posn (+ .5 (* .5 GRID-WIDTH))
                                     PLAYER-Y)
                          false)
             (build-list mushnumber
                         (lambda (x)
                           (make-mush (make-posn
                                       (+ 1 (random (- GRID-WIDTH 1)))
                                       (+ 1.5 (random (- GRID-HEIGHT 2))))
                                      4)))
             "play"
             0)
            [to-draw render]
            [on-tick update-world]
            [on-key player-input]))

(main 40 15)