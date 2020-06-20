#lang racket
(require racket/gui)

;window's height
(define WINDOW-HEIGHT 20)
;window's width
(define WINDOW-WIDTH 20)
;frame size
(define WINDOW-SIZE 10)
;create a frame named "Snake" with pre-defined height & width
(define frame (new frame% 
  [label "Snake"]
  [width (* WINDOW-WIDTH WINDOW-SIZE)]
  [height (* WINDOW-HEIGHT WINDOW-SIZE)]))
;Initial cases
(define snake (list (list 2 1) (list 1 1)));Initial snake position
(define apple (list 5 5));Initial apple position
(define direction 'r);Initial direction of snake's head
(define score 0);Initial score board
;restart case->set all status into initial case
(define restart (lambda()
  (set! snake (list (list 2 1) (list 1 1)))
  (set! apple (list 5 5))
  (set! direction 'r)
  (set! score 0)
))
;failure situation
(define lost-the-game (lambda ()
  (send dc draw-text "Press R to restart!" (- (round (/ (* WINDOW-WIDTH WINDOW-SIZE) 2)) 80) (- (round (/ (* WINDOW-HEIGHT WINDOW-SIZE) 2)) 30))
))
;x&y represent coordinates
(define (move-block x y) 
  (reverse (append (cdr (reverse snake)) (list(list x y)))))
;the coordinates of head
(define (snake-head post-positon lst)
  (list-ref (list-ref lst 0) post-positon))
;paint area
(define (draw-block screen x y color) 
  (send screen set-brush color 'solid)
  (send screen draw-rectangle (* x WINDOW-SIZE) (* y WINDOW-SIZE) WINDOW-SIZE WINDOW-SIZE))
;Map each key-event with key-function
(define (canvas-key frame) (class canvas%
  (define/override (on-char key-event)
    (cond
      [(eq? (send key-event get-key-code) 'left) (set! direction 'l)]
      [(eq? (send key-event get-key-code) 'right) (set! direction 'r)]
      [(eq? (send key-event get-key-code) 'up) (set! direction 'u)]
      [(eq? (send key-event get-key-code) 'down) (set! direction 'd)]
      [(eq? (send key-event get-key-code) '#\r) (restart)]))
  (super-new [parent frame])))
;The functions for key-events
(define (move-snake post-positon)
  (case post-positon
    ['l (set! snake (move-block (- (snake-head 0 snake) 1) (snake-head 1 snake)))]
    ['r (set! snake (move-block (+ (snake-head 0 snake) 1) (snake-head 1 snake)))]
    ['u (set! snake (move-block (snake-head 0 snake) (- (snake-head 1 snake) 1)))]
    ['d (set! snake (move-block (snake-head 0 snake) (+ (snake-head 1 snake) 1)))]))

(define (self-block snake block [i 0] [g 666]) (if (> (length snake) i) (if (and (not (= g i)) (and (eq? (list-ref (list-ref snake i) 0) (list-ref block 0)) (eq? (list-ref (list-ref snake i) 1) (list-ref block 1)))) #t(self-block snake block (+ i 1) g))#f))
;update status
(define update-snake-status (lambda () 
  (draw-block dc (list-ref apple 0) (list-ref apple 1) "red") 
  (cond [(self-block snake apple) (post-update-snake)] [else (move-snake direction)]) ; checa por colisão com a apple
  (send dc draw-text (number->string score) (-(* WINDOW-WIDTH WINDOW-SIZE) 190) 10)
  (for ([block snake]) (
    if (eq? block (car snake)) 
      (draw-block dc (list-ref block 0) (list-ref block 1) "black") 
      (draw-block dc (list-ref block 0) (list-ref block 1) "black")))))
;how snake gets longer
(define post-update-snake (lambda () 
  (define x (car (reverse snake)))
  (set! apple (list (inexact->exact (round (* (random) (- WINDOW-WIDTH 1)))) (inexact->exact (round (* (random) (- WINDOW-HEIGHT 1)))) ))
  (move-snake direction)
  (set! score (add1 score))
  (set! snake (append snake (list x)))))
(define canvas (
  new (canvas-key frame)))

(define dc (send canvas get-dc))

(send dc set-font (make-object font% 10 'modern))
(send dc set-text-foreground "red")

(send frame show #t)
;set start time then run the steps
(define timer (new timer%
  [notify-callback (lambda()
    (send dc clear)
    ;background color
    (send dc set-brush "white" 'solid)        
    (send dc draw-rectangle 0 0 (* WINDOW-WIDTH WINDOW-SIZE) (* WINDOW-HEIGHT WINDOW-SIZE))                 
    (define accident-edge #f);when snake meets edges
    (for ([block snake]
         [j (in-naturals 0)])
      (cond 
            [(or (> (list-ref block 0) WINDOW-WIDTH) (> 0 (list-ref block 0))) (set! accident-edge #t )]
            [(or (> (list-ref block 1) WINDOW-HEIGHT) (> 0 (list-ref block 1))) (set! accident-edge #t)]
            [(eq? #f accident-edge) (set! accident-edge (eq? #t (self-block snake block 0 j)))]))
    (if accident-edge (lost-the-game) (update-snake-status)))]
  [interval #f]))

(send timer start 100)