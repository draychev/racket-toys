#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; Constants
(define WIDTH 400)
(define HEIGHT 600)
(define PLAYER-SPEED 10)
(define BULLET-SPEED 15)
(define INVADER-SPEED 5)
(define INVADER-ROWS 3)
(define INVADER-COLS 5)

(define PLAYER-IMG (triangle 20 "solid" "blue"))
(define INVADER-IMG (circle 15 "solid" "red"))
(define BULLET-IMG (rectangle 4 10 "solid" "yellow"))

;; Structs
(struct world (player bullets invaders dir) #:transparent)
(struct bullet (x y) #:transparent)
(struct invader (x y) #:transparent)

;; Initial State
(define (make-invaders)
  (for*/list ([row (in-range INVADER-ROWS)]
              [col (in-range INVADER-COLS)])
    (invader (+ 40 (* 60 col)) (+ 40 (* 40 row)))))

(define INIT (world (vector (/ WIDTH 2) (- HEIGHT 30)) '() (make-invaders) 1))

;; Drawing
(define (draw-world w)
  (define bg (empty-scene WIDTH HEIGHT))
  (define player-x (vector-ref (world-player w) 0))
  (define player-img (place-image PLAYER-IMG player-x (- HEIGHT 30) bg))
  (define bullets-img
    (foldr (λ (b img) (place-image BULLET-IMG (bullet-x b) (bullet-y b) img))
           player-img
           (world-bullets w)))
  (define invaders-img
    (foldr (λ (i img) (place-image INVADER-IMG (invader-x i) (invader-y i) img))
           bullets-img
           (world-invaders w)))
  invaders-img)

;; On-tick
(define (move-bullets bullets)
  (filter (λ (b) (> (bullet-y b) 0))
          (map (λ (b) (bullet (bullet-x b) (- (bullet-y b) BULLET-SPEED))) bullets)))

(define (move-invaders invaders dir)
  (map (λ (i) (invader (+ (invader-x i) (* dir INVADER-SPEED)) (invader-y i))) invaders))

(define (invaders-hit-wall? invaders)
  (ormap (λ (i) (or (< (invader-x i) 20) (> (invader-x i) (- WIDTH 20)))) invaders))

(define (collide bullet invader)
  (and (< (abs (- (bullet-x bullet) (invader-x invader))) 20)
       (< (abs (- (bullet-y bullet) (invader-y invader))) 20)))

(define (remove-collisions bullets invaders)
  (for/fold ([bullets bullets] [invaders invaders]) ([b bullets])
    (define remaining (filter (λ (i) (not (collide b i))) invaders))
    (if (< (length remaining) (length invaders))
        (values (remove b bullets) remaining)
        (values bullets invaders))))


(define (TICK w)
  (define new-bullets (move-bullets (world-bullets w)))
  (define new-invaders (move-invaders (world-invaders w) (world-dir w)))
  (define-values (final-bullets final-invaders)
    (remove-collisions new-bullets new-invaders))
  (define new-dir
    (if (invaders-hit-wall? new-invaders) (* -1 (world-dir w)) (world-dir w)))
  (world (world-player w) final-bullets final-invaders new-dir))

;; Key events
(define (handle-key w key)
  (define x (vector-ref (world-player w) 0))
  (cond
    [(key=? key "left")
     (world (vector (max 0 (- x PLAYER-SPEED)) (- HEIGHT 30))
            (world-bullets w) (world-invaders w) (world-dir w))]
    [(key=? key "right")
     (world (vector (min WIDTH (+ x PLAYER-SPEED)) (- HEIGHT 30))
            (world-bullets w) (world-invaders w) (world-dir w))]
    [(key=? key " ")
     (world (world-player w)
            (cons (bullet x (- HEIGHT 50)) (world-bullets w))
            (world-invaders w) (world-dir w))]
    [else w]))

;; Run the game
(big-bang INIT
  [to-draw draw-world]
  [on-tick TICK 0.05]
  [on-key handle-key])
