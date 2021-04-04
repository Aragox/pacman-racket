#lang racket  
(define max-deep 3)

;funcion de poda
(define (cutoff-test deep)
  (if (>= deep max-deep)
      #t
      #f)
  )

;que tan cerca esta un fantasma de pac-man
(define (howCloseGhost gp weigth)
  (define distantGP (first (manhattan-move gp pp "chase")))
  (define value (/ (* weigth (* rows-tot columns-tot)) (+ 1 distantGP)))
  (cond
    [(equal? vulnerable-ghosts #t) (* -1 value)]
    [else value])
  )

(define (howCloseCorner actual-corners weigth)
  (define sum 0)
  (define l '())
  (for ([elem actual-corners])
    (cond
      [(equal? (fourth elem) #t) (set! l (append l elem))]
      ))
  (for ([elem l])
    (define d (first (manhattan-move (list (second elem) (* (first elem) columns-tot)) pp "chase")))
    (define value (/ (* weigth (* rows-tot columns-tot)) (+ 1 d)))
    (set! sum (+ sum value))
    )
  sum
  )

(define (howCloseBalls maze weigth)
  (define sum 0)
  (define l '())
  (for ([elem maze])
    (cond
      [(equal? (fourth elem) #t) (set! l (append l elem))]
      ))
  (for ([elem l])
    (define d (first (manhattan-move (list (second elem) (* (first elem) columns-tot)) pp "chase")))
    (define value (/ (* weigth (* rows-tot columns-tot)) (+ 1 d)))
    (set! sum (+ sum value))
    )
  sum
  )
  
(define (eval balls-count pp g1p g2p actual-corners maze actual-count-balls)
  (define sum 0)
  (define ball-value (/ balls-total actual-count-balls))
  (set! sum (+ sum
               (howCloseGhost g2p -15)
               (howCloseGhost g1p -15)
               (howCloseCorner (* 8 ball-value))
               (howCloseBalls maze (* 2 ball-value))
               )
        )
  sum
  )

;se calcula el valor maximo
(define (max-value a b pp g1p g2p actual-corners maze actual-count-balls deep)
  (define v 0)
  (define poslist '())
  (cond
    [(cutoff-test deep)
      (eval balls-count pp g1p g2p actual-corners maze actual-count-balls)]
    [else
      (set! v -99999999999999999999999)
      (set! poslist (get-positions pp))
      (define temp1 maze)
      (define temp2 actual-corners)
      (define temp3 actual-count-balls)
      (for ([elem poslist])
        (cond ; Ocultar las bolitas en las casillas que pacman ha recorrido
          [(equal? (fourth (list-ref maze (+ (first pp) (second pp)))) #t) ; Si hay bolita en la posición actual de pacman
           (cond ; Si la bolita es grande y está en una esquina
             [(member (list (/ (second pp) columns-tot) (first pp))
                      (map (lambda (elem) (take elem 2)) actual-corners))
              (set! actual-corners (list-set
                                    actual-corners
                                    (+ (first pp)(second pp))
                                    (list-set (list-ref actual-corners (+ (first pp)(second pp))) 3 #f)
                                    ))] ; Hacer vulnerables a los fantasmas
             )
           (set! maze (list-set maze (+
                                      (first pp)
                                      (second pp))
                                (list-set (list-ref maze (+ (first pp)(second pp))) 3 #f)
                                )
                 )
           (set! actual-count-balls (- actual-count-balls 1)) ; Actualizar bolitas
           ]
          )
    
        (set! v (max v (min-value1 a b pp g1p g2p actual-corners maze actual-count-balls deep)))
        (cond
          [(>= v b) v])
        (set! a (max a v))
        (set! maze temp1)
        (set! actual-corners temp2)
        (set! actual-count-balls temp3)
        )
      v]))

(define (min-value1 a b pp g1p g2p actual-corners maze actual-count-balls deep)
  (define v 0)
  (define poslist '())
  (cond
    [(cutoff-test deep)
      (eval balls-count pp g1p g2p actual-corners maze actual-count-balls)]
    [else
      (set! v 99999999999999999999999)
      (set! poslist (second (manhattan-move g1p pp "chase")))
      
      (for ([elem poslist])
    
        (set! v (min v (min-value2 a b pp g1p g2p actual-corners maze actual-count-balls deep)))
        (cond
          [(<= v a) v])
        (set! b (min b v))
        )
      v]))

(define (min-value2 a b pp g1p g2p actual-corners maze actual-count-balls deep)
  (define v 0)
  (define poslist '())
  (cond
    [(cutoff-test deep)
      (eval balls-count pp g1p g2p actual-corners maze actual-count-balls)]
    [else
      (set! v 99999999999999999999999)
      (set! poslist (second (manhattan-move g2p pp "chase")))
      
      (for ([elem poslist])
    
        (set! v (min v (max-value a b pp g1p g2p actual-corners maze actual-count-balls deep)))
        (cond
          [(<= v a) v])
        (set! b (min b v))
        )
      v]))
