(require 2htdp/universe)
(require 2htdp/image)

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

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

(define MISSILE (ellipse 5 15 "solid" "red"))

(define GAMEOVER-TEXT (text/font "GAMEOVER" 24 "red" "Phosphate" "swiss" "normal" "bold" #f))
(define GAMEOVER-BACKGROUND (rectangle WIDTH HEIGHT "solid" (make-color 255 255 255 200)))
(define GAMEOVER (overlay GAMEOVER-TEXT GAMEOVER-BACKGROUND))

(define TANK-HEIGHT (image-height TANK))
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-X-START (/ WIDTH 2))
(define INVADER-WIDTH (image-width INVADER))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define RANDOM-INVADE-RANGE (- WIDTH (* INVADER-WIDTH 2)))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))
(define MISSILE-Y-START (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT/2))

;; Data Definitions:

(define-struct game (invaders missiles tank invade))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Natural)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles, tank position and invade counter

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))
       (fn-for-invade (game-invade s))))


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

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 0))
(define G2 (make-game (list I1) (list M1) T1 0))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 0))


(define-struct point (x y))
;; Point is (make-point Natural Natural)
;; interp. the point is the (x, y) coordinate on the screen

(define P1 (make-point 0 0)) ; the top left
(define P2 (make-point 100 100))
(define P3 (make-point 100 200))

#;
(define (fn-for-point p)
  (... (point-x p)
       (point-y p)))


;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank TANK-X-START 0) 0))
;; (main (make-game (list (make-invader 150 0 (- INVADER-X-SPEED)) (make-invader 250 0 INVADER-X-SPEED)) empty (make-tank (/ WIDTH 2) 0) 0))
;; (main (make-game (list (make-invader 150 150 -1) (make-invader 250 250 1)) (list (make-missile 50 50) (make-missile 100 100)) (make-tank (/ WIDTH 2) 0) 0))

(define (main game)
  (big-bang game
    [to-draw render]
    [on-tick handle-tick]
    [on-key handle-key]
    [on-release handle-release]
    [stop-when game-over? end-scene]))


;; Game -> Image
;; produce them game scene with the current game state
(check-expect (render (make-game empty empty (make-tank TANK-X-START 0) 0))
              (place-image TANK TANK-X-START (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render (make-game (list (make-invader 50 50 -1) (make-invader 100 100 1))
                                 empty
                                 (make-tank TANK-X-START 0)
                                 0))
              (place-image INVADER 50 50
                           (place-image INVADER 100 100
                                        (place-image TANK TANK-X-START (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
(check-expect (render (make-game empty
                                 (list (make-missile 50 50) (make-missile 100 100))
                                 (make-tank TANK-X-START 0)
                                 0))
              (place-image MISSILE 50 50
                           (place-image MISSILE 100 100
                                        (place-image TANK TANK-X-START (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
(check-expect (render (make-game (list (make-invader 150 150 -1) (make-invader 200 200 1))
                                 (list (make-missile 50 50) (make-missile 100 100))
                                 (make-tank TANK-X-START 0)
                                 0))
              (place-image INVADER 150 150
                           (place-image INVADER 200 200
                                        (place-image MISSILE 50 50
                                                     (place-image MISSILE 100 100
                                                                  (place-image TANK TANK-X-START (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

; (define (render g) empty-image) ; stub

(define (render g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)))))


;; Tank -> Image
;; produce the tank scene with the current tank position
(check-expect (render-tank (make-tank TANK-X-START 0))
              (place-image TANK TANK-X-START (- HEIGHT TANK-HEIGHT/2) BACKGROUND)) ; center
(check-expect (render-tank (make-tank TANK-WIDTH/2 0))
              (place-image TANK TANK-WIDTH/2 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)) ; left most
(check-expect (render-tank (make-tank (- WIDTH TANK-WIDTH/2) 0))
              (place-image TANK (- WIDTH TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)) ; right most
(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; ListOfMissile Image -> Image
;; produce new image scene with list of missile
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 200 200) (make-missile 100 100)) BACKGROUND)
              (place-image MISSILE 200 200
                           (place-image MISSILE 100 100 BACKGROUND)))

; (define (render-missile lom img) empty-image) ; stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) img))]))


;; ListOfInvader Image -> Image
;; produce new image scene with list of ufo
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list (make-invader 50 50 -1) (make-invader 150 150 1)) BACKGROUND)
              (place-image INVADER 50 50
                           (place-image INVADER 150 150 BACKGROUND)))

; (define (render-invaders loi img) img) ; stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) img))]))


;; Game -> Game
;; call a helper function that produce the next game state
(define (handle-tick g)
  (next-game g (+ (random RANDOM-INVADE-RANGE) INVADER-WIDTH)))

;; Game Natural -> Game
;; produce the next game state from the given game state and the center-x position of the invader
;; if the invade counter is 100 then the new invader will be added to the ListOfInvaders
(check-expect (next-game (make-game empty empty (make-tank TANK-X-START 1) 0) 150)
              (make-game empty empty (make-tank (+ TANK-X-START TANK-SPEED) 1) 1))

(check-expect (next-game (make-game empty empty (make-tank TANK-X-START 1) INVADE-RATE) 150)
              (make-game (list (make-invader 150 0 (- INVADER-X-SPEED)))
                         empty
                         (make-tank (+ TANK-X-START TANK-SPEED) 1)
                         0))

(check-expect (next-game (make-game (list (make-invader 50 50 -1) (make-invader 100 100 1))
                                    empty
                                    (make-tank TANK-X-START -1)
                                    0)
                         150)
              (make-game (list (make-invader (- 50 1) (+ 50 INVADER-Y-SPEED) -1)
                               (make-invader (+ 100 1) (+ 100 INVADER-Y-SPEED) 1))
                         empty
                         (make-tank (- TANK-X-START TANK-SPEED) -1)
                         1))

(check-expect (next-game (make-game empty
                                    (list (make-missile 50 50) (make-missile 100 100))
                                    (make-tank TANK-X-START 0)
                                    0)
                         150)
              (make-game empty
                         (list (make-missile 50 (- 50 MISSILE-SPEED))
                               (make-missile 100 (- 100 MISSILE-SPEED)))
                         (make-tank TANK-X-START 0)
                         1))
#;
(check-expect (next-game (make-game (list (make-invader 150 150 -1) (make-invader 200 200 1))
                                    (list (make-missile 50 50) (make-missile 100 100))
                                    (make-tank TANK-X-START 0)
                                    0)
                         150)
              (make-game (list (make-invader (- 150 1) (+ 150 INVADER-Y-SPEED) -1)
                               (make-invader (+ 200 1) (+ 200 INVADER-Y-SPEED) 1))
                         (list (make-missile 50 (- 50 MISSILE-SPEED))
                               (make-missile 100 (- 100 MISSILE-SPEED)))
                         (make-tank TANK-X-START 0)
                         1))

; (define (next-game g x) g) ; stub

(define (next-game g x)
  (make-game (add-invader x (game-invade g)
                          (move-invaders (remove-hit-invaders (game-invaders g) (game-missiles g))))
             (move-missiles (remove-missiles (remove-hit-missiles (game-missiles g) (game-invaders g))))
             (move-tank (game-tank g))
             (next-invade (game-invade g))))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; filter out the list of invader that hit the missile

(check-expect (remove-hit-invaders empty empty) empty)
(check-expect (remove-hit-invaders (list (make-invader 100 100 -2) (make-invader 200 200 2)) empty)
              (list (make-invader 100 100 -2) (make-invader 200 200 2)))
(check-expect (remove-hit-invaders empty (list (make-missile 100 100) (make-missile 200 200))) empty)
(check-expect (remove-hit-invaders (list (make-invader 100 100 -2) (make-invader 200 200 2))
                                   (list (make-missile 200 100) (make-missile 300 200)))
              (list (make-invader 100 100 -2) (make-invader 200 200 2)))
(check-expect (remove-hit-invaders (list (make-invader 100 100 -2) (make-invader 200 200 2))
                                   (list (make-missile 100 100) (make-missile 300 200)))
              (list (make-invader 200 200 2)))
(check-expect (remove-hit-invaders (list (make-invader 100 100 -2) (make-invader 200 200 2))
                                   (list (make-missile 100 100) (make-missile 200 200)))
              empty)

; (define (remove-hit-invaders loi lom) loi) ; stub

(define (remove-hit-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (hit? (make-point (invader-x (first loi))
                               (invader-y (first loi)))
                   (missiles-to-points lom))
             (remove-hit-invaders (rest loi) lom)
             (cons (first loi) (remove-hit-invaders (rest loi) lom)))]))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; filter out the list of missile that hit the invader

(check-expect (remove-hit-missiles empty empty) empty)
(check-expect (remove-hit-missiles (list (make-missile 100 100) (make-missile 200 200)) empty)
              (list (make-missile 100 100) (make-missile 200 200)))
(check-expect (remove-hit-missiles empty (list (make-invader 100 100 -2) (make-invader 200 200 -2))) empty)
(check-expect (remove-hit-missiles (list (make-missile 100 100) (make-missile 200 200))
                                   (list (make-invader 200 100 -2) (make-invader 300 200 -2)))
              (list (make-missile 100 100) (make-missile 200 200)))
(check-expect (remove-hit-missiles (list (make-missile 100 100) (make-missile 200 200))
                                   (list (make-invader 100 100 -2) (make-invader 300 200 -2)))
              (list (make-missile 200 200)))
(check-expect (remove-hit-missiles (list (make-missile 100 100) (make-missile 200 200))
                                   (list (make-invader 100 100 -2) (make-invader 200 200 -2)))
              empty)

; (define (remove-hit-missiles lom loi) lom) ;stub

(define (remove-hit-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (hit? (make-point (missile-x (first lom))
                               (missile-y (first lom)))
                   (invaders-to-points loi))
             (remove-hit-missiles (rest lom) loi)
             (cons (first lom) (remove-hit-missiles (rest lom) loi)))]))


;; Point ListOfPoint -> Boolean
;; return true if the distance between
;; two point is less than or equal the HIT-RANGE
;; otherwise return false

(check-expect (hit? (make-point 100 100) empty) #false)
(check-expect (hit? (make-point 100 100) (list (make-point (+ 100 HIT-RANGE 1) 100) (make-point 200 100))) #false)
(check-expect (hit? (make-point 100 100) (list (make-point (+ 100 HIT-RANGE) 100) (make-point 200 100))) #true)

; (define (hit? p lop) #false) ;stub

(define (hit? p lop)
  (cond [(empty? lop) #false]
        [else
         (if (<= (distance (point-x p) (point-y p)
                           (point-x (first lop)) (point-y (first lop)))
                 HIT-RANGE)
             #true
             (hit? p (rest lop)))]))


;; ListOfMissile -> ListOfPoint
;; convert the list of missile to the list of point

(check-expect (missiles-to-points empty) empty)
(check-expect (missiles-to-points (list (make-missile 100 150) (make-missile 200 250)))
              (list (make-point 100 150) (make-point 200 250)))

; (define (missiles-to-points lom) empty) ; stub

(define (missiles-to-points lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-point (missile-x (first lom)) (missile-y (first lom)))
               (missiles-to-points (rest lom)))]))


;; ListOfInvader -> ListOfPoint
;; convert the list of invader to the list of point

(check-expect (invaders-to-points empty) empty)
(check-expect (invaders-to-points (list (make-invader 100 150 -2) (make-invader 200 250 2)))
              (list (make-point 100 150) (make-point 200 250)))

; (define (invaders-to-points loi) empty) ; stub

(define (invaders-to-points loi)
  (cond [(empty? loi) empty]
        [else
         (cons (make-point (invader-x (first loi)) (invader-y (first loi)))
               (invaders-to-points (rest loi)))]))


;; Natural Natural Natural Natural -> Natural
;; produce a distance between two cooridates in pixel

; (define (distance x1 y1 x2 y2) 0) ; stub

(check-expect (distance 1 1 4 5) 5)

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2))
           (sqr (- y1 y2)))))


;; Natural Natural ListOfInvader -> ListOfInvader
;; add a new invader at the x position if the invade counter reach the INVADE-RATE

(check-expect (add-invader 150 0 empty) empty)
(check-expect (add-invader 150 INVADE-RATE empty) (list (make-invader 150 0 (- INVADER-X-SPEED))))
(check-expect (add-invader 150 0 (list (make-invader 50 0 -2)
                                       (make-invader 100 0 2)))
              (list (make-invader 50 0 -2)
                    (make-invader 100 0 2)))
(check-expect (add-invader 150 INVADE-RATE (list (make-invader 50 0 -2)
                                                 (make-invader 100 0 2)))
              (list(make-invader 150 0 (- INVADER-X-SPEED))
                   (make-invader 50 0 -2)
                   (make-invader 100 0 2)))

; (define (add-invader x invade loi) loi) ; stub

(define (add-invader x invade loi)
  (if (= invade INVADE-RATE)
      (cons (make-invader x 0 (- INVADER-X-SPEED)) loi)
      loi))
      

;; ListOfInvaders -> ListOfInvaders
;; produce a new ListOfInvaders
;; with new x and y position in pixel

(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list (make-invader 100 100 (- INVADER-X-SPEED))
                                   (make-invader 200 200 INVADER-X-SPEED)))
              (list (make-invader (- 100 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) (- INVADER-X-SPEED))
                    (make-invader (+ 200 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED)))

; (define (move-invaders loi) loi) ; stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; Invader -> Invader
;; produce new invader with a new (x, y) position changed in pixel

(check-expect (move-invader (make-invader 100 100 -5)) (make-invader (- 100 5) (+ 100 INVADER-Y-SPEED) -5))
(check-expect (move-invader (make-invader (+ INVADER-WIDTH/2 4) 100 -5)) (make-invader (+ INVADER-WIDTH/2 4) (+ 100 INVADER-Y-SPEED) 5)) ; almost left
(check-expect (move-invader (make-invader (- (- WIDTH INVADER-WIDTH/2) 4) 100 5)) (make-invader (- (- WIDTH INVADER-WIDTH/2) 4) (+ 100 INVADER-Y-SPEED) -5)) ; almost right

; (define (move-invader in) in) ; stub

(define (move-invader in)
  (cond [(or (< (invader-left (next-invader-x in)) INVADER-WIDTH/2)
             (> (invader-right (next-invader-x in)) (- WIDTH INVADER-WIDTH/2)))
         (make-invader (invader-x in)
                       (next-invader-y in)
                       (* -1 (invader-dx in)))]
        [else
         (make-invader (next-invader-x in)
                       (next-invader-y in)
                       (invader-dx in))]))


;; Invader -> Natural
;; produce the next center-x position of the invader

(check-expect (next-invader-x (make-invader 100 200 -5)) (- 100 5))
(check-expect (next-invader-x (make-invader 100 200 5)) (+ 100 5))
(check-expect (next-invader-x (make-invader 100 200 -3)) (- 100 3))
(check-expect (next-invader-x (make-invader 100 200 3)) (+ 100 3))
              
; (define (next-invader-x in) 0) ; stub

(define (next-invader-x in)
  (+ (invader-x in) (invader-dx in)))


;; Invader -> Natural
;; produce the next center-y position of the invader

(check-expect (next-invader-y (make-invader 100 150 -5)) (+ 150 INVADER-Y-SPEED))
(check-expect (next-invader-y (make-invader 100 200 5)) (+ 200 INVADER-Y-SPEED))
              
; (define (next-invader-y in) 0) ; stub

(define (next-invader-y in)
  (+ (invader-y in) INVADER-Y-SPEED))


;; Natural -> Natural
;; produce the left bound of the invader
;; from a given center-x of invader

(check-expect (invader-left 100) (- 100 INVADER-WIDTH/2))
(check-expect (invader-left 200) (- 200 INVADER-WIDTH/2))
(check-expect (invader-left 0) (- 0 INVADER-WIDTH/2))

; (define (invader-left x) 0) ; stub

(define (invader-left x) (- x INVADER-WIDTH/2))


;; Natural -> Natural
;; produce the right bound of the invader
;; from a given center-x of invader

(check-expect (invader-right 100) (+ 100 INVADER-WIDTH/2))
(check-expect (invader-right 200) (+ 200 INVADER-WIDTH/2))
(check-expect (invader-right WIDTH) (+ WIDTH INVADER-WIDTH/2))

(define (invader-right x) (+ x INVADER-WIDTH/2))


;; Natural -> Natural
;; produce the bottom bound of the invader
;; from a given center-y of invader

(check-expect (invader-bottom 100) (+ 100 INVADER-HEIGHT/2))
(check-expect (invader-bottom 200) (+ 200 INVADER-HEIGHT/2))

; (define (invader-bottom y) y) ;stub

(define (invader-bottom y) (+ y INVADER-HEIGHT/2))


;; ListOfMissile -> ListOfMissile
;; produce a new ListOfMissile with new position changed

(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list (make-missile 100 150)
                                   (make-missile 200 250)))
              (list (make-missile 100 (- 150 MISSILE-SPEED))
                    (make-missile 200 (- 250 MISSILE-SPEED))))

; (define (move-missiles lom) lom) ; stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (move-missiles (rest lom)))]))


;; Missile -> Missile
;; produce the next missile with center-y positon changed in pixel

(check-expect (move-missile (make-missile 100 150)) (make-missile 100 (- 150 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 200 250)) (make-missile 200 (- 250 MISSILE-SPEED)))
 
; (define (move-missile m) m) ; stub

(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissile -> ListOfMissile
;; filter out all the missile that is out of screen
;; the missile is out of screen when the bottom of missile is less than 0

(check-expect (remove-missiles empty) empty)
(check-expect (remove-missiles (list (make-missile 100 150)
                                     (make-missile 200 250)))
              (list (make-missile 100 150)
                    (make-missile 200 250)))
(check-expect (remove-missiles (list (make-missile 100 (- (+ MISSILE-HEIGHT/2 1)))
                                     (make-missile 200 250)))
              (list (make-missile 200 250)))

; (define (remove-missiles lom) lom) ;stub

(define (remove-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-bottom (first lom)) 0)
             (cons (first lom) (remove-missiles (rest lom)))
             (remove-missiles (rest lom)))]))


;; Missile -> Natural
;; produce a bottom bound of missile

(check-expect (missile-bottom (make-missile 100 150)) (+ 150 MISSILE-HEIGHT/2))
(check-expect (missile-bottom (make-missile 200 250)) (+ 250 MISSILE-HEIGHT/2))

; (define (missile-bottom m) 0) ; stub

(define (missile-bottom m)
  (+ (missile-y m) MISSILE-HEIGHT/2))


;; Tank -> Tank
;; produce the next tank with new position depend on the tank dir
;; if tank dir is 0, the tank is not moving (the position is not change)
;; if tank dir is -1, the tank is moving to the left (the position is decreased by TANK-SPEED)
;; if tank state is "right", the tank is moving to the right (the position is increased by TANK-SPEED)
(check-expect (move-tank (make-tank 100 0)) (make-tank 100 0))
(check-expect (move-tank (make-tank 100 -1)) (make-tank (- 100 TANK-SPEED) -1))
(check-expect (move-tank (make-tank 100 1)) (make-tank (+ 100 TANK-SPEED) 1))
(check-expect (move-tank (make-tank (+ TANK-WIDTH/2 (- TANK-SPEED 1)) -1)) (make-tank TANK-WIDTH/2 -1))  ; almost left
(check-expect (move-tank (make-tank (- (- WIDTH TANK-WIDTH/2) (- TANK-SPEED 1)) 1)) (make-tank (- WIDTH TANK-WIDTH/2) 1))  ; almost right

; (define (move-tank t) t) ; stub

(define (move-tank t)
  (cond [(= 0 (tank-dir t)) t]
        [(< (tank-left (next-tank-x t)) 0) (make-tank TANK-WIDTH/2 (tank-dir t))]
        [(> (tank-right (next-tank-x t)) WIDTH) (make-tank (- WIDTH TANK-WIDTH/2) (tank-dir t))]
        [else (make-tank (next-tank-x t) (tank-dir t))]))


;; Tank -> Natural
;; produce the next x position of tank

(check-expect (next-tank-x (make-tank 100 1)) (+ 100 TANK-SPEED))
(check-expect (next-tank-x (make-tank 100 -1)) (- 100 TANK-SPEED))

;(define (next-tank-x t) 0) ;stub

(define (next-tank-x t)
  (+ (tank-x t) (* (tank-dir t) TANK-SPEED)))


;; Natural -> Natural
;; produce the left bound of the tank in pixcel
;; from a given center-x tank position

(check-expect (tank-left 100) (- 100 TANK-WIDTH/2))
(check-expect (tank-left 0) (- 0 TANK-WIDTH/2))

(define (tank-left x) (- x TANK-WIDTH/2))


;; Natural -> Natural
;; produce the right bound of the tank in pixcel
;; from a given center-x tank position

(check-expect (tank-right 100) (+ 100 TANK-WIDTH/2))
(check-expect (tank-right WIDTH) (+ WIDTH TANK-WIDTH/2))

(define (tank-right x)
  (+ x TANK-WIDTH/2))



;; Natural -> Natural
;; increase an invade counter by 1
;; if the invadd count reach INVADE-RATE, then it will reset to 0

(check-expect (next-invade 0) 1)
(check-expect (next-invade 1) 2)
(check-expect (next-invade INVADE-RATE) 0)

; (define (next-invade n) n) ; stub

(define (next-invade n)
  (if (= n INVADE-RATE)
      0
      (+ n 1)))


;; Game KeyEvent -> Game
;; update a game state by key
;; !!!

(check-expect (handle-key (make-game empty empty (make-tank 100 0) 0) "left")
              (make-game empty empty (make-tank 100 -1) 0))
(check-expect (handle-key (make-game empty empty (make-tank 100 0) 0) "right")
              (make-game empty empty (make-tank 100 1) 0))
(check-expect (handle-key (make-game empty empty (make-tank 100 0) 0) "a")
              (make-game empty empty (make-tank 100 0) 0))
(check-expect (handle-key (make-game empty empty (make-tank 100 -1) 0) "right")
              (make-game empty empty (make-tank 100 1) 0))
(check-expect (handle-key (make-game empty empty (make-tank 100 1) 0) "left")
              (make-game empty empty (make-tank 100 -1) 0))
(check-expect (handle-key (make-game empty empty (make-tank 100 1) 0) " ")
              (make-game empty (list (make-missile 100 MISSILE-Y-START)) (make-tank 100 1) 0))
(check-expect (handle-key (make-game empty (list (make-missile 100 MISSILE-Y-START)) (make-tank 200 1) 0) " ")
              (make-game empty (list (make-missile 200 MISSILE-Y-START) (make-missile 100 MISSILE-Y-START)) (make-tank 200 1) 0))

; (define (handle-key g ke) g) ; stub

(define (handle-key g ke)
  (cond [(key=? "left" ke) (make-game (game-invaders g)
                                      (game-missiles g)
                                      (make-tank (tank-x (game-tank g)) -1)
                                      (game-invade g))]
        [(key=? "right" ke) (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x (game-tank g)) 1)
                                       (game-invade g))]
        [(key=? " " ke) (make-game (game-invaders g)
                                   (cons (make-missile (tank-x (game-tank g)) MISSILE-Y-START)
                                         (game-missiles g))
                                   (game-tank g)
                                   (game-invade g))]
        [else g]))


;; Game KeyEvent -> Game
;; update a game state when release a key

(check-expect (handle-release (make-game empty empty (make-tank 100 -1) 0) "left")
              (make-game empty empty (make-tank 100 0) 0))
(check-expect (handle-release (make-game empty empty (make-tank 100 -1) 0) "right")
              (make-game empty empty (make-tank 100 -1) 0))
(check-expect (handle-release (make-game empty empty (make-tank 100 1) 0) "right")
              (make-game empty empty (make-tank 100 0) 0))
(check-expect (handle-release (make-game empty empty (make-tank 100 1) 0) "left")
              (make-game empty empty (make-tank 100 1) 0))
(check-expect (handle-release (make-game empty empty (make-tank 100 1) 0) "a")
              (make-game empty empty (make-tank 100 1) 0))
(check-expect (handle-release (make-game empty empty (make-tank 100 -1) 0) "a")
              (make-game empty empty (make-tank 100 -1) 0))

; (define (handle-release g ke) g) ; stub

(define (handle-release g ke)
  (cond [(and (key=? "left" ke) (= (tank-dir (game-tank g)) -1))
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 0)
                    (game-invade g))]
        [(and (key=? "right" ke) (= (tank-dir (game-tank g)) 1))
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 0)
                    (game-invade g))]
        [else g]))


;; Game -> Boolean
;; produce true if the game is in the end state condition

(check-expect (game-over? (make-game (list (make-invader 100 150 -2)
                                           (make-invader 200 250 2))
                                     empty
                                     (make-tank 100 0)
                                     0))
              #false)
(check-expect (game-over? (make-game (list (make-invader 100 150 -2)
                                           (make-invader 200 (- HEIGHT INVADER-HEIGHT/2) 2))
                                     empty
                                     (make-tank 100 0)
                                     0))
              #true)

; (define (game-over? g) #false) ;stub

(define (game-over? g)
  (reach-bottom (game-invaders g)))


;; ListOfInvader -> Boolean
;; return true if any of the invader bottom reach the bottom of the screen

(check-expect (reach-bottom empty) #false)
(check-expect (reach-bottom (list (make-invader 100 150 -2) (make-invader 200 250 2))) #false)
(check-expect (reach-bottom (list (make-invader 200 (- HEIGHT INVADER-HEIGHT/2) -2) (make-invader 200 250 2))) #true)

; (define (reach-bottom loi) #false) ; stub

(define (reach-bottom loi)
  (cond [(empty? loi) #false]
        [else
         (if (>= (invader-bottom (invader-y (first loi))) HEIGHT)
             #true
             (reach-bottom (rest loi)))]))


;; Game -> Image
;; produce the game over scene

(check-expect (end-scene (make-game (list (make-invader 40 50 -2) (make-invader 80 90 2))
                                    (list (make-missile 100 110) (make-missile 200 210))
                                    (make-tank TANK-X-START 0)
                                    0))
              (overlay GAMEOVER
                       (place-image INVADER 40 50
                                    (place-image INVADER 80 90
                                                 (place-image MISSILE 100 110
                                                              (place-image MISSILE 200 210
                                                                           (place-image TANK TANK-X-START (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))))))

; (define (end-scene g) empty-image) ; stub

(define (end-scene g)
  (overlay GAMEOVER (render g)))
