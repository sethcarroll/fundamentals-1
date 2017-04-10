;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS2500 Problem Set 13 - Seth Carroll & Kevin Khomiakov|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CS2500 Problem Set 13
; Seth Carroll & Kevin Khomiakov
; carroll.se@husky.neu.edu & khomiakov.k@husky.neu.edu

; Problem 1

;; make-palindrome : String -> String
;; constructs a palindrome by mirroring a an non-empty string
;;   around the last letter.

(check-expect (make-palindrome "fundies") "fundieseidnuf")
(check-expect (make-palindrome "a") "a")
(check-expect (make-palindrome "Kevin") "KeviniveK")


(define (make-palindrome str)
  (local [;; reverse-string: Number String-> String
          ;; takes a String-length n, an accumulator (string "") and produce 
          ;; the right side of the mirroring String
          (define (reverse-string sl acc)
            (cond [(zero? (sub1 sl)) acc]
                  [else (reverse-string
                         (sub1 sl)
                         (string-append acc
                                        (substring str (- sl 2) (- sl 1))))]))]
    (reverse-string (string-length str) str)))

;;;; This is second version of this function which does not need local in it.
;;;; TA said that we need to put it in our final submission so that graders
;;;; will themselves decide whether to grade this version or the previous one.

;; make-palindrome.v2 : String -> String
;; constructs a palindrome by mirroring a an non-empty string
;;   around the last letter.

(check-expect (make-palindrome.v2 "fundies") "fundieseidnuf")
(check-expect (make-palindrome.v2 "a") "a")
(check-expect (make-palindrome.v2 "Kevin") "KeviniveK")

(define (make-palindrome.v2 str)
  (implode (append (explode str) (rest (reverse (explode str))))))


;; is-palindrome? : String -> Boolean
;; determines whether the String is a palindrome or not

(check-expect (is-palindrome? "fundieseidnuf") true)
(check-expect (is-palindrome? "a") true)
(check-expect (is-palindrome? "bab") true)
(check-expect (is-palindrome? "") true)
(check-expect (is-palindrome? "foo") false)
(check-expect (is-palindrome? "maam") true)
(check-expect (is-palindrome? "aa") true)

(define (is-palindrome? s)
  (local [;; reverse-string.v2 : String -> String
          ;; takes a string and reverses it
          (define (reverse-string.v2 str acc)
           (cond [(string=? "" str) acc]
                 [else 
                  (reverse-string.v2 (substring str 1)
                                  (string-append (substring str 0 1) acc))]))]
    (string=? s (reverse-string.v2 s ""))))

;; is-palindrome?.v2 : String -> Boolean
;; determines whether the String is a palindrome or not

;;;; This is second version of this function which does not need local in it.
;;;; TA said that we need to put it in our final submission so that graders
;;;; will themselves decide whether to grade this version or the previous one.

(check-expect (is-palindrome?.v2 "fundieseidnuf") true)
(check-expect (is-palindrome?.v2 "a") true)
(check-expect (is-palindrome?.v2 "bab") true)
(check-expect (is-palindrome?.v2 "") true)
(check-expect (is-palindrome?.v2 "foo") false)
(check-expect (is-palindrome?.v2 "maam") true)
(check-expect (is-palindrome?.v2 "aa") true)

(define (is-palindrome?.v2 s)
  (string=? s (implode (reverse (explode s)))))


; Problem 2

;; prime? : Number -> Boolean
;; consumes a Natural Number, n and returns true
;; if it is prime and false otherwise

(check-expect (prime? 11) true)
(check-expect (prime? 2) true)
(check-expect (prime? 1) false)
(check-expect (prime? 6) false)
(check-expect (prime? 0) false)
(check-expect (prime? -1) false)

(define (prime? nn)
  (local [;; find-prime: NN NN -> Boolean
          ;; consumes a Natural Number, n and
          ;; the integer of the square root of n (which is also NN),
          ;; and returns true if it is prime and false ohterwise
          (define (find-prime nn1 nn2)
            (cond [(and (zero? (remainder nn1 nn2)) (= 1 nn2)) true]
                  [(and (zero? (remainder nn1 nn2)) (not (= 1 nn2))) false]
                  [else (find-prime nn1 (sub1 nn2))]))]
    (cond
      [(<= nn 1) false]
      [else (find-prime nn (integer-sqrt nn))])))


; list-primes : NN -> [List-of NN]
; consumes a Natural Number, n, and produces the list of
; prime numbers up to n.

(check-expect (list-primes 15) (list 2 3 5 7 11 13))
(check-expect (list-primes 5) (list 2 3 5))
(check-expect (list-primes 8) (list 2 3 5 7))
(check-expect (list-primes 0) '())
(check-expect (list-primes 1) '())

(define (list-primes n)
  (local [; lp-helper : [List-of NN] -> [List-of NN]
          (define (lp-helper lon)
            (cond [(empty? lon) lon]
                  [(< (floor (sqrt n)) (first lon)) lon]
                  [else 
                   (cons (first lon)
                         (lp-helper
                          (filter (λ (x)
                                    (not (zero? (remainder x (first lon)))))
                                  lon)))]))]
    (lp-helper (range 2 (add1 n) 1))))

; list-primes.v2 : NN -> [List-of NN]
; consumes a Natural Number, n, and produces the list of
; prime numbers up to n.

;;;; This is second version of this function which does not need local in it.
;;;; TA said that we need to put it in our final submission so that graders
;;;; will themselves decide whether to grade this version or the previous one.

(check-expect (list-primes.v2 15) (list 2 3 5 7 11 13))
(check-expect (list-primes.v2 5) (list 2 3 5))
(check-expect (list-primes.v2 8) (list 2 3 5 7))
(check-expect (list-primes.v2 0) '())
(check-expect (list-primes.v2 1) '())

(define (list-primes.v2 n)
  (filter prime? (build-list n add1)))

; Problem 3

;; A Card is a (make-card value suit)
;; Interp.
;;   a value is one of:
;;      A Number, 2-10
;;      JACK
;;      QUEEN
;;      KING
;;      ACE
;;   a suit is one of:
;;      CLUBS
;;      DIAMONDS
;;      HEARTS
;;      SPADES
(define-struct card [value suit])

(define JACK 11)
(define QUEEN 12)
(define KING 13)
(define ACE 14)

(define CLUBS 1)
(define DIAMONDS 2)
(define HEARTS 3)
(define SPADES 4)

;; A Player is a (make-player String Card)
;; interp. string is the player's name
(define-struct player [name card])

(define SETH (make-player "Seth" (make-card 2 DIAMONDS)))
(define SETH2 (make-player "Seth" (make-card ACE SPADES)))
(define KEVIN (make-player "Kevin" (make-card 1 DIAMONDS)))
(define KEVIN2 (make-player "Kevin" (make-card 2 CLUBS)))
(define BEN (make-player "Ben" (make-card 1 HEARTS)))
(define SARAH (make-player "Sarah" (make-card 2 SPADES)))
(define ABBY (make-player "Abby" (make-card ACE CLUBS)))

;; A Trick is a [List-of Player] with a length of 4
(define TRICK1 (list SETH KEVIN BEN SARAH))
(define TRICK2 (list ABBY SETH KEVIN BEN))
(define TRICK3 (list SETH KEVIN BEN ABBY))
(define TRICK4 (list SETH KEVIN BEN KEVIN2))
(define TRICK5 (list SETH2 KEVIN BEN SARAH))

;; A Game is a [List-of Trick]
(define GAME1 (list TRICK1 TRICK2 TRICK3))
(define GAME2 (list TRICK1 TRICK2))
(define GAME3 (list TRICK1 TRICK4 TRICK5))

;; high-card-victor : Trick -> Player
;; determines the winner in a trick,
;;   with the winner being the card of highest value

(check-expect (high-card-victor TRICK1)
              SARAH)
(check-expect (high-card-victor TRICK2)
              ABBY)
(check-expect (high-card-victor TRICK3)
              ABBY)
(check-expect (high-card-victor TRICK4)
              SETH)
(check-expect (high-card-victor TRICK5)
              SETH2)

(define (high-card-victor atrick)
  (local (;; Player Player -> Player
          ;; checks which player has the winning card
          (define (victor? aplayer1 aplayer2)
            (if (equal?
                 (better-card (player-card aplayer1) (player-card aplayer2))
                 (player-card aplayer1))
                aplayer1
                aplayer2)))
    (foldr victor? (first atrick) (rest atrick))))

;; better-card : Card Card -> Card
;; checks which card wins by having higher value/suit

(check-expect (better-card (make-card 10 SPADES)
                           (make-card 9 SPADES))
              (make-card 10 SPADES))
(check-expect (better-card (make-card 9 SPADES)
                           (make-card 10 SPADES))
              (make-card 10 SPADES))
(check-expect (better-card (make-card 10 CLUBS)
                           (make-card 10 SPADES))
              (make-card 10 SPADES))
 
(define (better-card acard1 acard2)
  (cond [(< (card-value acard1)
            (card-value acard2)) acard2]
        [(> (card-value acard1)
            (card-value acard2)) acard1]
        [(= (card-value acard1)
            (card-value acard2)) (better-suit acard1 acard2)]))

;; better-suit : Card Card -> [Maybe-Card]
;; checks which card has a better suit
;; if two cards have the same suit and number, error given

(check-expect (better-card (make-card 10 CLUBS)
                           (make-card 10 SPADES))
              (make-card 10 SPADES))
(check-expect (better-card (make-card 10 SPADES)
                           (make-card 10 CLUBS))
              (make-card 10 SPADES))

(define (better-suit acard1 acard2)
  (cond [(<= (card-suit acard1)
             (card-suit acard2)) acard2]
        [(> (card-suit acard1)
            (card-suit acard2)) acard1]))

;; game-winner : Game -> String
;; determines who wins a game by winning the most tricks,
;;   reports the winning player's name or reports a tie if no player wins.

(check-expect (game-winner GAME1) "Abby")
(check-expect (game-winner GAME2) "Tie Game")
(check-expect (game-winner GAME3) "Seth")

(define (game-winner agame)
  (if (empty? (the-winner (winner-list agame empty)))
      "Tie Game"
      (first (the-winner (winner-list agame empty)))))

;; the-winner : [List-of String] -> [List-of String]
;; outputs a list with the most frequent winner in a list of players
;;   and reports an empty list in the case of a tie

(check-expect (the-winner (list "Abby" "Sarah" "Abby" "Sarah" "Abby"))
              (list "Abby"))
(check-expect (the-winner (list "Abby" "Sarah")) '())

(define (the-winner alop)
   (cond [(and (= (length alop) 2) (not (string=? (first alop) (second alop))))
           '()]
          [(> (length alop) 1)
           (the-winner (repeat-winner? (sort alop string<?)))]
          [else alop]))

;; repeat-winner? [List-of String] -> [List-of String]
;; consolidates repeated wins in an alphabetized list of player names
;;   and removes players with no repeat wins

(check-expect (repeat-winner? (list "Abby" "Abby" "Abby" "Sarah" "Sarah"))
              (list "Abby" "Abby" "Sarah"))
(check-expect (repeat-winner? (list "Abby" "Abby" "Ben")) (list"Abby"))

(define (repeat-winner? alop)
  (cond [(empty? (rest alop)) empty]
        [else (if (string=? (first alop) (second alop))
                  (cons (first alop) (repeat-winner? (rest alop)))
                  (repeat-winner? (rest alop)))]))

;; winner-list : Game [List-of String] -> [List-of String]
;; adds the names of winners of tricks in a game to a list of names

(check-expect (winner-list '() (list "Seth")) (list "Seth"))
(check-expect (winner-list GAME1 '()) (list "Abby" "Abby" "Sarah"))
(check-expect (winner-list GAME3 '()) (list "Seth" "Seth" "Sarah"))
              
(define (winner-list agame alist)
  (cond [(empty? agame) alist]
        [else (winner-list (rest agame)
                           (cons (player-name (high-card-victor (first agame)))
                                 alist))]))

; Problem 4

;; A Tweeps is one of:
;; empty
;; (list String [List-of String])

;; A User is: (make-user string [List-of String])
;; handle=name=string and tweeps=followers=[List-of String]

(define-struct user (handle tweeps))

;; Network is one of:
;; empty
;; (list User [List-of Tweep])

(define network1
  (list (make-user "Kevin" (list "Nat" "Seth"))
        (make-user "Nat" (list "John" "Kevin" "Ted"))
        (make-user "Ted" '())
        (make-user "Margo" '())
        (make-user "John" (list "Nat" "Margo"))))

(define network2
  (list (make-user "Ted" '())
        (make-user "Kevin" (list "Nat" "Seth"))
        (make-user "Margo" '())
        (make-user "Nat" (list "John" "Kevin" "Ted"))
        (make-user "John" (list "Nat" "Margo"))))

(define network3
  (list (make-user "Ted" (list "Kevin"))
        (make-user "Kevin" (list "Nat" "Seth"))))

;; list-handles: Network -> [List-of String]
;; creates a list of handles from all users in the network

(check-expect (list-handles empty) empty)
(check-expect (list-handles network1) (list "John" "Margo" "Ted" "Nat" "Kevin"))
(check-expect (list-handles network2) (list "John" "Nat" "Margo" "Kevin" "Ted"))
(check-expect (list-handles network3) (list "Kevin" "Ted"))

(define (list-handles nw)
  (local [(define (names nw acc)
            (cond [(empty? nw) acc]
                  [else
                   (names (rest nw) (cons (user-handle (first nw)) acc))]))]
    (names nw '())))

;; most-followers: Network -> String
;; produces the handle of the user with the most folowers in the network

(check-expect (most-followers network1) "Nat")
(check-expect (most-followers network2) "Nat")
(check-expect (most-followers network3) "Kevin")

(define (most-followers nw)
  (local [; most-followers-helper : Network -> [List-of String]???????
          ; 
          (define (most-followers-helper nw acc)
            (cond [(empty? nw) (user-handle acc)]
                  [else (if (< (length (user-tweeps (first nw)))
                               (length (user-tweeps acc)))
                            (most-followers-helper (rest nw) acc)
                            (most-followers-helper (rest nw) (first nw)))]))]
    (most-followers-helper (rest nw) (first nw))))
      

;; friends?: Network -> Boolean
;; checks if a network has two mutual followers

(check-expect (friends? '()) false)
(check-expect (friends? (list (make-user "Kevin" (list "Nat" "Seth")))) false)
(check-expect (friends? network1) true)
(check-expect (friends? network2) true)
(check-expect (friends? network3) false)

(define (friends? nw)
  (cond
    [(empty? nw) false]
    [(empty? (rest nw)) false]
    [(2-friends? (first nw) (second nw)) true]
    [else (friends? (rest nw))]))

;; 2-friends? : User User -> Boolean
;; checks if two users are in each other's list of tweeps

(check-expect (2-friends? (make-user "Kevin" (list "Nat" "Seth"))
                          (make-user "Nat" (list "John" "Kevin" "Ted"))) true)
(check-expect (2-friends? (make-user "Ted" (list "Kevin"))
                          (make-user "Kevin" (list "Nat" "Seth"))) false)

(define (2-friends? user1 user2)
  (and (member? (user-handle user1) (user-tweeps user2))
       (member? (user-handle user2) (user-tweeps user1))))
