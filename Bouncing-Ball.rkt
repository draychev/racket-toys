#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 400)
(define HEIGHT 400)
(define RADIUS 20)
(define GRAVITY 1)
(define BOUNCE -0.8)

(define CIRCLE (circle RADIUS "solid" "blue"))

;; world = (list y velocity)
(define (draw-world world)
  (define y (first world))
  (place-image CIRCLE (/ WIDTH 2) y (empty-scene WIDTH HEIGHT)))

(define (TICK world)
  (define y (first world))
  (define v (second world))
  (define new-v (+ v GRAVITY))
  (define new-y (+ y new-v))
  (if (>= new-y (- HEIGHT RADIUS))
      (list (- HEIGHT RADIUS) (* new-v BOUNCE))
      (list new-y new-v)))

(big-bang (list 100 0)
  [to-draw draw-world]
  [on-tick TICK])
