#lang racket
; Cantidad de niveles de profundidad del minimax
(define max-deep 3)

;Función que determina si ya no se analizarán jugadas futuras
; Retorna un booleano
(define (cutoff-test deep)
  (if (>= deep max-deep)
      #t
      #f)
  )

;Función que retorna un valor dependiendo de qué tan cerca está un fantasma de pac-man
; Si el fantasma está cerca retorna una magnitud mayor, y retorna una magnitud menor en caso contrario
; También cambia el signo del valor retornado dependiendo de si los fantasmas están vulnerables o no
(define (howCloseGhost gp vulnerable-gh weigth)
  (define distantGP (first (manhattan-move gp pp "chase"))) ; Obtener distancia
  (define value (/ (* weigth (* rows-tot columns-tot)) (+ 1 distantGP)))
  (cond
    [(equal? vulnerable-gh #t) (* -1 value)]
    [else value])
  )

;Función que retorna un valor dependiendo de una suma de qué tan cerca está pac-man de alguna esquina con bolita grande
; Si pacman está cerca de alguna esquina retorna una magnitud mayor, y retorna una magnitud menor en caso contrario
(define (howCloseCorner actual-corners weigth)
  (define sum 0)
  (define l '())
  (for ([elem actual-corners]) ; Trabajar sólo con las esquinas que tienen bolita presente
    (cond
      [(equal? (fourth elem) #t) (set! l (append l elem))]
      ))
  (for ([elem l])
    (define d (first (manhattan-move (list (second elem) (* (first elem) columns-tot)) pp "chase"))) ; Obtener distancia a la bolita
    (define value (/ weigth (+ 1 d)))
    (set! sum (+ sum value)) ; Acumular suma
    )
  sum
  )

;Función que retorna un valor dependiendo de una suma de qué tan cerca está pac-man de alguna bolita 
; Si pacman está cerca de alguna bolita retorna una magnitud mayor, y retorna una magnitud menor en caso contrario
(define (howCloseBalls maze weigth)
  (define sum 0)
  (define l '())
  (for ([elem maze]) ; Trabajar sólo con las casillas que tienen bolita presente
    (cond
      [(equal? (fourth elem) #t) (set! l (append l elem))]
      ))
  (for ([elem l])
    (define d (first (manhattan-move (list (second elem) (* (first elem) columns-tot)) pp "chase")))  ; Obtener distancia a la bolita
    (define value (/ weigth (+ 1 d)))
    (set! sum (+ sum value))  ; Acumular suma
    )
  sum
  )

; Función que retorna un valor numérico dada una función lineal del estado del tablero de juego (laberinto)
(define (eval balls-count pp g1p g2p actual-corners maze actual-count-balls vulnerable-gh)
  (define sum 0)
  (define ball-value (/ balls-total actual-count-balls)) ; Lo que vale cada bolita en este momento
  (set! sum (+ sum     ; sumar valores numéricos de funciones con sus respectivos pesos
               (howCloseGhost g2p vulnerable-gh -15)
               (howCloseGhost g1p vulnerable-gh -15)
               (howCloseCorner (* 8 ball-value))
               (howCloseBalls maze (* 2 ball-value))
               )
        )
  sum
  )

;Función max-value del minimax. Retorna el máximo valor y la posición que debe tomar pacman
(define (max-value a b pp g1p g2p actual-corners maze actual-count-balls deep vulnerable-gh)
  (define v '())
  (define poslist '())
  
  (cond
    [(cutoff-test deep)
      (list (eval balls-count (last pp) g1p g2p actual-corners maze actual-count-balls vulnerable-gh) (second pp))] ; Retornar v
    [else
      (set! v (list -99999999999999999999999 (list 0 0))) ; v es una lista que guarda un valor y una posición
      (set! poslist (get-positions (last pp))) ; Obtener hijos del nodo hoja
      (define temp1 maze)
      (define temp2 actual-corners)
      (define temp3 actual-count-balls)
      
      (for ([elem poslist])
        (cond ; Ocultar las bolitas en las casillas que pacman ha recorrido
          [(equal? (fourth (list-ref maze (+ (first elem) (second elem)))) #t) ; Si hay bolita en la posición actual de pacman
           (cond ; Si la bolita es grande y está en una esquina
             [(member (list (/ (second elem) columns-tot) (first elem))
                      (map (lambda (ele) (take ele 2)) actual-corners))
              (set! actual-corners (list-set ; Actualizar la presencia de bolitas en la esquina respectiva
                                    actual-corners
                                    (+ (first elem)(second elem))
                                    (list-set (list-ref actual-corners (+ (first elem)(second elem))) 3 #f)
                                    ))] ; Hacer vulnerables a los fantasmas
             )
           (set! maze (list-set maze (+  ; Actualizar la presencia de bolitas en la casilla respectiva
                                      (first elem)
                                      (second elem))
                                (list-set (list-ref maze (+ (first elem)(second elem))) 3 #f)
                                )
                 )
           (set! actual-count-balls (- actual-count-balls 1)) ; Actualizar # bolitas
           ]
          )
        (define new-v ; Obtener nuevo v
          (min-value1 a b (append pp (list elem)) g1p g2p actual-corners maze actual-count-balls deep vulnerable-gh) ; Agregar nuevo nodo hoja
        )
        (cond ; Cambiar el v anterior por el nuevo v
          [(> (first new-v) (first v)) (set! v new-v)] 
        )
        (cond
          [(>= (first v) b) v] ; Retornar v
        )
        (set! a (max a (first v))) ; Asignar alfa
        (set! maze temp1)
        (set! actual-corners temp2)
        (set! actual-count-balls temp3)
        )
      v]))

(define (min-value1 a b pp g1p g2p actual-corners maze actual-count-balls deep vulnerable-gh)
  (define v '())
  (define poslist '())
  (cond
    [(cutoff-test deep)
      (list (eval balls-count (last pp) g1p g2p actual-corners maze actual-count-balls vulnerable-gh) (second pp))] ; Retornar v
    [else
      (set! v (list 99999999999999999999999 (list 0 0))) ; v es una lista que guarda un valor y una posición
      
      (cond ; Obtener la única posición a la que se va a mover el fantasma
        [(equal? vulnerable-gh #t) (set! poslist (list (second (manhattan-move g1p pp "run away"))))]
        [(equal? vulnerable-gh #f) (set! poslist (list (second (manhattan-move g1p pp "chase"))))]
      )
      
      (for ([elem poslist])
   
        (define new-v ; Obtener nuevo v
          (min-value2 a b pp elem g2p actual-corners maze actual-count-balls deep vulnerable-gh)
        )
        (cond ; Cambiar el v anterior por el nuevo v
          [(< (first new-v) (first v)) (set! v new-v)] 
        )
        (cond
          [(<= (first v) a) v] ; Retornar v
        ) 
        (set! b (min b (first v))) ; Asignar beta
        )
      v]))

(define (min-value2 a b pp g1p g2p actual-corners maze actual-count-balls deep vulnerable-gh)
  (define v '())
  (define poslist '())
  (cond
    [(cutoff-test deep)
      (list (eval balls-count (last pp) g1p g2p actual-corners maze actual-count-balls vulnerable-gh) (second pp))] ; Retornar v
    [else
      (set! v (list 99999999999999999999999 (list 0 0))) ; v es una lista que guarda un valor y una posición
      
      (cond ; Obtener la única posición a la que se va a mover el fantasma
        [(equal? vulnerable-gh #t) (set! poslist (list (second (manhattan-move g2p pp "run away"))))]
        [(equal? vulnerable-gh #f) (set! poslist (list (second (manhattan-move g2p pp "chase"))))]
      )
      
      (for ([elem poslist])
    
        (define new-v ; Obtener nuevo v
          (max-value a b pp g1p elem actual-corners maze actual-count-balls deep vulnerable-gh)
        )
        (cond ; Cambiar el v anterior por el nuevo v
          [(< (first new-v) (first v)) (set! v new-v)] 
        )        
        (cond
          [(<= (first v) a) v]  ; Retornar v
        )
        (set! b (min b (first v))) ; Asignar beta
        )
      v]))
