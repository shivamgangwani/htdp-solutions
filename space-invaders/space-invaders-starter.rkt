;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 0.25)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 0.25)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 5)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define BACKGROUND-TRANSPARENT (empty-scene WIDTH HEIGHT "transparent"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))
(define INVADER-DX-BASE 9)
(define INVADER-DX-RANGE 5)

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M1 (cons M2 empty)))
(define LOM3 (cons M1 (cons M2 (cons M3 empty))))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)
                   (fn-for-lom (rest lom)))]))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I1 (cons I2 empty)))
(define LOI3 (cons I1 (cons I2 (cons I3 empty))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty T0))
;; 
(define (main s)
  (big-bang s                ; Game
    (on-tick   tock)         ; Game -> Game
    (to-draw   render)       ; Game -> Image
    (stop-when invaded?)     ; Game -> Boolean
    (on-key    handle-key))) ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state, by:
;; - Tank     -> Moving in the DX direction by TANK-SPEED units
;; - Invaders -> Move them downward along 45 degree diagonals (in DX direction) [until they hit Y = (HEIGHT - TANKHEIGHT/2)]
;;            -> Randomly generate new ones at the top
;; Missiles   -> Move them upward, until they collide with an invader or reach the top of the screen
;; !!!
(check-expect (tock G0) (make-game (tock-invaders G0)
                                   (tock-missiles G0)
                                   (tock-tank (game-tank G0))))

(check-expect (tock G1) (make-game (tock-invaders G1)
                                   (tock-missiles G1)
                                   (tock-tank (game-tank G1))))

(check-expect (tock G2) (make-game (tock-invaders G2)
                                   (tock-missiles G2)
                                   (tock-tank (game-tank G2))))


;(define (tock s) s) ;stub
(define (tock s)
  (make-game (tock-invaders s)
             (tock-missiles s)
             (tock-tank (game-tank s))))

;; Game -> ListOfInvader
;; Produce the next list of Invader in the game state by:
;; - Removing the ones that have crashed with missiles
;; - Moving the remaining ones forward
;; !!!                                        
(check-random (tock-invaders G2) (random-spawn-invader (move-invaders (filter-invaders (game-missiles G2) (game-invaders G2))) (random (sqr INVADE-RATE)) (random WIDTH) (random INVADER-DX-RANGE)))  
(check-random (tock-invaders G3) (random-spawn-invader (move-invaders (filter-invaders (game-missiles G3) (game-invaders G3))) (random (sqr INVADE-RATE)) (random WIDTH) (random INVADER-DX-RANGE)))
;(define (tock-invaders s) (game-invaders s)) ;stub
(define (tock-invaders s)
  (random-spawn-invader (move-invaders (filter-invaders (game-missiles s)
                                                        (game-invaders s)))
                        (random (sqr INVADE-RATE))
                        (random WIDTH)
                        (+ INVADER-DX-BASE (random INVADER-DX-RANGE))))


;; ListOfInvader -> ListOfInvader
;; If rnum is INVADE-RATE, add a new invader at y = 0, x = xpos, dx = idx
(check-expect (random-spawn-invader empty INVADE-RATE 0 10) (list (make-invader 0 0 10)))
(check-expect (random-spawn-invader LOI1 INVADE-RATE 20 10) (list (make-invader 20 0 10) I1))
(check-expect (random-spawn-invader empty 0 20 10) empty)
;(define (random-spawn-invader loi rnum xpos idx) loi) ;stub
(define (random-spawn-invader loi rnum xpos idx)
  (if (= rnum INVADE-RATE)
      (cons (make-invader xpos 0 idx) loi)
      loi))

;; ListOfMissiles ListOfInvader -> ListOfInvader
;; Return a filtered list of invaders (after removing the ones that have been hit by a missile)
(check-expect (filter-invaders empty empty) empty)
(check-expect (filter-invaders LOM1 empty) empty)
(check-expect (filter-invaders empty LOI1) LOI1)
(check-expect (filter-invaders LOM1 LOI1) LOI1)
(check-expect (filter-invaders LOM2 LOI1) empty)
;(define (filter-invaders lom loi) loi) ;stub
(define (filter-invaders lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [(invader-collision? (first loi) lom)    (filter-invaders  lom (rest loi))]
        [else                                    (cons (first loi) (filter-invaders lom (rest loi)))]))

;; Invader ListOfMissile -> Boolean
;; Return true if the invader has collided with a missile in the ListOfMissile
(check-expect (invader-collision? I1 (list M1))     false)
(check-expect (invader-collision? I1 (list M2))     true)
(check-expect (invader-collision? I1 (list M1 M2))  true)
;(define (invader-collision? i lom) false) ;stub
(define (invader-collision? i lom)
  (cond [(empty? lom) false]
        [else (if (missile-invader-collision? (first lom) i)
                  true
                  (invader-collision? i (rest lom)))]))

;; ListOfInvader -> ListOfInvade
;; Return a list of invaders after moving each along its respective diagonal path
(check-expect (move-invaders LOI0) empty)
(check-expect (move-invaders LOI1) (list (move-invader I1)))
;(define (move-invaders loi) loi) ;stub
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (move-invaders (rest loi)))]))


;; Invader -> Invader
;; Move an invader along its diagonal path, bouncing off the edges of the wall
(check-expect (move-invader I1) (make-invader (+ (invader-x I1) (* (invader-dx I1) INVADER-X-SPEED))
                                              (+ (invader-y I1) (abs (* (invader-dx I1) INVADER-Y-SPEED)))
                                              (invader-dx I1)))

(check-expect (move-invader (make-invader -1 100 -10))  ; left edge, bounce to right
              (make-invader -1 100 10))


(check-expect (move-invader (make-invader (+ WIDTH 1) 100 10))  ; right edge, bounce to left
              (make-invader (+ WIDTH 1) 100 -10))

;(define (move-invader i) i) ;stub
(define (move-invader i)
  (cond [(and (> (invader-x i) WIDTH) (> (invader-dx i) 0)) (flip-invader i)]
        [(and (< (invader-x i) 0) (< (invader-dx i) 0))     (flip-invader i)]
        [else (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                            (+ (invader-y i) (abs (* (invader-dx i) INVADER-Y-SPEED)))
                            (invader-dx i))]))

;; Invader -> Invader
;; Flip the direction of the invader

(check-expect (flip-invader (make-invader 0 0 10)) (make-invader 0 0 -10))
(check-expect (flip-invader (make-invader 0 0 -10)) (make-invader 0 0 10))
;(define (flip-invader i) i) ;stub
(define (flip-invader i)
  (make-invader (invader-x i) (invader-y i) (* -1 (invader-dx i))))



;; Game -> ListOfMissile
;; Produce the next list of Missile in the game state by:
;; - Removing the ones that have crashed with invaders
;; - Moving the remaining ones forward
(check-expect (tock-missiles G0) (game-missiles G0))                                                        
(check-expect (tock-missiles G2) (move-missiles (filter-missiles (game-missiles G2) (game-invaders G2))))  
(check-expect (tock-missiles G3) (move-missiles (filter-missiles (game-missiles G3) (game-invaders G3))))
;(define (tock-missiles s) (game-missiles s)) ;stub
(define (tock-missiles s)
  (move-missiles (filter-missiles (game-missiles s) (game-invaders s)))) 


;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Return a filtered list of missiles (after removing the ones that have crossed the screen or hit an invader)
;; !!!
(check-expect (filter-missiles (game-missiles G0) (game-invaders G0)) empty)               ; 0 missiles, 0 removed
(check-expect (filter-missiles (game-missiles G2) (game-invaders G2)) (cons M1 empty))     ; 2 missiles, 1 removed (hit an invader)
(check-expect (filter-missiles (list (make-missile (/ WIDTH 2) -2)) empty) empty); 1 missile, 1 removed (beyond screen)
                                          

(check-expect (filter-missiles (list (make-missile (/ WIDTH 2) -2) M2); 2 missiles, 2 removed (one hit invader, one beyond screen)
                               (list I1))
              empty)

;(define (filter-missiles s) (game-missiles s)) ;stub
(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [(missile-collision?     (first lom) loi)         (filter-missiles (rest lom) loi)]
        [(missile-beyond-screen? (first lom))             (filter-missiles (rest lom) loi)]
        [else                                             (cons (first lom) (filter-missiles (rest lom) loi))]))


;; Missile ListOfInvader -> Boolean
;; Return true if the missile has collided with an invader in the ListOfInvader
(check-expect (missile-collision? M1 (list I1))     false)
(check-expect (missile-collision? M2 (list I1))     true)
(check-expect (missile-collision? M2 (list I1 I2))  true)
;(define (missile-collision? m loi) false) ;stub
(define (missile-collision? m loi)
  (cond [(empty? loi) false]
        [else (if (missile-invader-collision? m (first loi))
                  true
                  (missile-collision? m (rest loi)))]))


;; Missile Invader -> Boolean
;; Return true if the given missile and invader have collided
(check-expect (missile-invader-collision? M1 I1) false)
(check-expect (missile-invader-collision? M2 I1) true)
;(define (missile-invader-collision? m i) false) ;stub
(define (missile-invader-collision? m i)
  (<= (distance (missile-x m) (missile-y m) (invader-x i) (invader-y i)) HIT-RANGE))

;; Number Number Number Number -> Number
;; Return the Euclidean distance between (x1, y1) and (x2, y2)
(check-expect (distance 0 0 0 1) 1)
(check-expect (distance 0 0 1 0) 1)
(check-expect (distance 0 0 3 4) 5)
;(define (distance x1 y1 x2 y2) 0) ;stub
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

;; Missile -> Boolean
;; Return true if the missile is beyond the screen, i.e, (missile-y m) < 0
(check-expect (missile-beyond-screen? (make-missile 0 (- HEIGHT 2))) false)
(check-expect (missile-beyond-screen? (make-missile 0 -5))           true)
;(define (missile-beyond-screen? m) false) ;stub
(define (missile-beyond-screen? m)
  (< (missile-y m) 0))

;; ListOfMissiles -> ListOfMissiles
;; Move all the missiles upward by MISSILE-SPEED units
(check-expect (move-missiles LOM0) empty)
(check-expect (move-missiles LOM1) (list (move-missile M1)))
(check-expect (move-missiles LOM2) (list (move-missile M1)
                                         (move-missile M2)))

;(define (move-missiles lom) lom) ;stub
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (move-missile (first lom))
                    (move-missiles (rest lom)))]))


;; Missile -> Missile
;; Move a missile upward by MISSILE-SPEED units
(check-expect (move-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
;(define (move-missile m) m) ;stub
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; Produce the next state of Tank by moving it forward in direction (tank-dir t)
(check-expect (tock-tank T0) (make-tank (+ (tank-x T0) (* (tank-dir T0) TANK-SPEED)) (tank-dir T0)));middle
(check-expect (tock-tank (make-tank 0 -1))    (make-tank 1 1))                       ;left edge, not 
(check-expect (tock-tank (make-tank WIDTH 1)) (make-tank (- WIDTH 1) -1))                                 ;right edge

;(define (tock-tank t) t) ;stub
;<took template from Tank>
(define (tock-tank t)
  (cond [(<= (tank-x t) 0) (make-tank 1 1)]                                          ; left-edge, change direction to right
        [(>= (tank-x t) WIDTH) (make-tank (- WIDTH 1 ) -1)]                          ; right-edge, change direction to left
        [else (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))])) ; middle, continue moving

;; Game -> Image
;; render the present state of the game, with the tank, missiles and invaders
(check-expect (render G0) (overlay BACKGROUND-TRANSPARENT
                                   BACKGROUND-TRANSPARENT
                                   (place-image TANK
                                                (tank-x (game-tank G0)) TANK-Y
                                                BACKGROUND-TRANSPARENT)
                                   BACKGROUND))

(check-expect (render G1) (overlay BACKGROUND-TRANSPARENT
                                   BACKGROUND-TRANSPARENT
                                   (place-image TANK
                                                (tank-x (game-tank G1)) TANK-Y
                                                BACKGROUND-TRANSPARENT)
                                   BACKGROUND))

(check-expect (render G2) (overlay (place-image MISSILE
                                                (missile-x M1) (missile-y M1)
                                                BACKGROUND-TRANSPARENT)
                                   (place-image INVADER
                                                (invader-x I1) (invader-y I1)
                                                BACKGROUND-TRANSPARENT)
                                   (place-image TANK
                                                (tank-x (game-tank G2)) TANK-Y
                                                BACKGROUND-TRANSPARENT)
                                   BACKGROUND))


(check-expect (render G3) (overlay (place-images (list MISSILE MISSILE)
                                                 (list (make-posn (missile-x M1) (missile-y M1))
                                                       (make-posn (missile-x M2) (missile-y M2)))
                                                 BACKGROUND-TRANSPARENT)
                                   (place-images (list INVADER INVADER)
                                                 (list (make-posn (invader-x I1) (invader-y I1))
                                                       (make-posn (invader-x I2) (invader-y I2)))
                                                 BACKGROUND-TRANSPARENT)
                                   (place-image TANK (tank-x (game-tank G2)) TANK-Y BACKGROUND-TRANSPARENT)
                                   BACKGROUND))


;(define (render s) BACKGROUND) ;stub
(define (render s)
  (overlay (render-missiles (game-missiles s))
           (render-invaders (game-invaders s))
           (render-tank (game-tank s))
           BACKGROUND))



;; listof Invader -> Image
;; Render the invaders on an empty background which can be superimposed on the game screen with the remaining elements
;; !!!
(check-expect (render-invaders LOI0) BACKGROUND-TRANSPARENT)
(check-expect (render-invaders LOI1) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND-TRANSPARENT))
(check-expect (render-invaders LOI2) (place-image INVADER
                                                  (invader-x I1) (invader-y I1)
                                                  (place-image INVADER
                                                               (invader-x I2) (invader-y I2)
                                                               BACKGROUND-TRANSPARENT)))
(check-expect (render-invaders LOI3) (place-image INVADER
                                                  (invader-x I1) (invader-y I1)
                                                  (place-image INVADER
                                                               (invader-x I2) (invader-y I2)
                                                               (place-image INVADER
                                                                            (invader-x I3) (invader-y I3)
                                                                            BACKGROUND-TRANSPARENT))))

;(define (render-invaders loi) BACKGROUND-TRANSPARENT) ;stub
(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND-TRANSPARENT]
        [else (place-image INVADER
                           (invader-x (first loi)) (invader-y (first loi))
                           (render-invaders (rest loi)))]))

;; listof Missile -> Image
;; Render the missiles on an empty background which can be superimposed on the game screen with the remaining elements
(check-expect (render-missiles LOM0) BACKGROUND-TRANSPARENT)
(check-expect (render-missiles LOM1) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND-TRANSPARENT))
(check-expect (render-missiles LOM2) (place-image MISSILE
                                                  (missile-x M1) (missile-y M1)
                                                  (place-image MISSILE
                                                               (missile-x M2) (missile-y M2)
                                                               BACKGROUND-TRANSPARENT)))
(check-expect (render-missiles LOM3) (place-image MISSILE
                                                  (missile-x M1) (missile-y M1)
                                                  (place-image MISSILE
                                                               (missile-x M2) (missile-y M2)
                                                               (place-image MISSILE
                                                                            (missile-x M3) (missile-y M3)
                                                                            BACKGROUND-TRANSPARENT))))



;(define (render-missiles lom) BACKGROUND-TRANSPARENT) ;stub
(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND-TRANSPARENT]
        [else (place-image MISSILE
                           (missile-x (first lom)) (missile-y (first lom))
                           (render-missiles (rest lom)))]))



;; Tank -> Image
;; Render the tank on an empty background which can be superimposed on the game screen with the remaining elements
(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND-TRANSPARENT))
;(define (render-tank t) BACKGROUND-TRANSPARENT) ;stub
(define (render-tank t)
  (place-image TANK
               (tank-x t) TANK-Y
               BACKGROUND-TRANSPARENT))



;; Game KeyEvent -> Game
(check-expect (handle-key G0 "left") (make-game empty empty (make-tank (tank-x (game-tank G0)) -1)))
; (define (handle-key s ke) s) ;stub
(define (handle-key s ke)
  (make-game (game-invaders s)
             (if (key=? ke " ")
                 (shoot-missile s)
                 (game-missiles s))
             (cond [(and (key=? ke "left")  (= (tank-dir (game-tank s)) 1))  (flip-tank-direction (game-tank s))]
                   [(and (key=? ke "right") (= (tank-dir (game-tank s)) -1)) (flip-tank-direction (game-tank s))]
                   [else (game-tank s)])))


;; Tank -> Tank
;; Flip the tank's direction
(check-expect (flip-tank-direction T0) (make-tank (tank-x T0) -1))
(check-expect (flip-tank-direction T2) (make-tank (tank-x T2) 1))
;(define (flip-tank-direction t) t) ;stub
(define (flip-tank-direction t)
  (make-tank (tank-x t) (* -1 (tank-dir t))))


;; Game -> ListOfMissiles
;; Given the state of the game, shoot another missile upward from the tank
(check-expect (shoot-missile G0) (cons (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2)) empty))
;(define (shoot-missile s) (game-missiles s)) ;stub
(define (shoot-missile s)
  (cons (make-missile (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT/2))
        (game-missiles s)))


;; Game -> Boolean
;; Return true if any of the invaders have reached the ground, to stop the game
(check-expect (invaded? G0) false)
(check-expect (invaded? G1) false)
(check-expect (invaded? G3) true)
;(define (invaded? s) false) ;stub
(define (invaded? s)
  (invaders-reached-ground? (game-invaders s)))


;; ListOfInvader -> Boolean
;; Return true if any of the invaders have reached the ground
(check-expect (invaders-reached-ground? LOI0) false)
(check-expect (invaders-reached-ground? LOI1) false)
(check-expect (invaders-reached-ground? LOI2) true)

;(define (invaders-reached-ground? loi) false) ;stub
(define (invaders-reached-ground? loi)
  (cond [(empty? loi) false]
        [else (if (>= (invader-y (first loi)) HEIGHT)
                  true
                  (invaders-reached-ground? (rest loi)))]))
