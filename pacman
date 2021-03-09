#lang racket/gui
(require racket/gui/base)
 (require table-panel) ;https://github.com/spdegabrielle/table-panel.git


;##################################################################################################
;MENÚ PRINCIPAL
; Crear el menú principal
(define mainmenu (new frame%
                 [label "Pacman"]                      
                 [width 300]
                 [height 370]
                 [stretchable-width #f]	 
   	         [stretchable-height #f]))

; Añadir un panel vertical, de botones centrados
(define title-panel (new horizontal-panel% [parent mainmenu]
                                           [min-width 275]
                                           [min-height 275]
                                           [alignment '(center center)]))

;Dibujar el título del juego
(new canvas% [parent title-panel]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 2.5 2.5)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Pacman AI" 12.5 20))])
 
; Añadir un panel vertical, de botones centrados
(define panel-mainmenu (new vertical-panel% [parent mainmenu]
                                     [alignment '(center center)]))

; Botón de "comenzar"
(new button% [parent panel-mainmenu] [label "Start!"][min-width 180]	 
   	 	[min-height 40] [callback (lambda (button event)
                         (send mainmenu show #f)                 
                         (send board show #t))]) ;Comienza el juego                                             
;##################################################################################################
;TABLERO DE JUEGO

;Tablero del juego puntitos
(define board (new frame%
                   [label "Pacman"]
                   [min-width 600]
                   [min-height 600]
                   [stretchable-width #f]	 
   	           [stretchable-height #f]))


; Hacer mensaje estático que para el tablero 
(define msg-board 
(new canvas% [parent board]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 1.5 1.5)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Level 1" 12.5 0))]))

; Añadir un panel horizontal, de score centrado
(define score-panel (new horizontal-panel% [parent board]
                                    [spacing 10]
                                    [alignment '(center center)]))

;Función que asigna el valor del score
(define (update-score)
  (~a 17) ; Convierte el número a un string
)

; Función que define el score (incluyendo las letras y los números) en la ventana
(define score 
(new canvas% [parent score-panel]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 2.7 2.5)
                (send dc set-text-foreground "blue")
                (send dc draw-text (string-append "SCORE: " (update-score)) 0 0))]))



;Un tablero de dimensiones estándar de 2x2
(define table-panel
  (instantiate table-panel%
    (board)
    (alignment '(center center))
    [min-width 350]	 
    [min-height 350]
    (dimensions '(2 2)))) ;dimensiones estándar de 2x2

;##################################################################################################
; Mostrar ventana principal
(send mainmenu show #t)
