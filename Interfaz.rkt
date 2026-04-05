#lang racket/base

(require 2htdp/image)
(require 2htdp/universe)

; Importamos la lógica

(require "SourceCode.rkt")

; ===================================================================
; CONSTANTES VISUALES 
; ===================================================================
(define TAMANO-CELDA 90)
(define TEXTO-TAMANO 36)

; Función para dar el color de fondo según el número de la celda
(define (color-fondo numero)
  (cond [(= numero 0)    "white"]
        [(= numero 2)    "lightgrey"]
        [(= numero 4)    "bisque"]
        [(= numero 8)    "orange"]
        [(= numero 16)   "tomato"]
        [(= numero 32)   "orangered"]
        [(= numero 64)   "red"]
        [(= numero 128)  "gold"]
        [(= numero 256)  "goldenrod"]
        [(= numero 512)  "darkgoldenrod"]
        [(= numero 1024) "yellowgreen"]
        [(= numero 2048) "forestgreen"]
        [else "black"])) ; Para números mayores a 2048 si llegan

; Función para el color del texto
(define (color-texto numero)
  (if (or (= numero 4) (= numero 2)) "dimgray" "white"))


; ===================================================================
; MOTOR DE DIBUJO (Recursividad pura, cero mutaciones)
; ===================================================================

; 1. Dibuja una celda individual
(define (dibujar-celda numero)
  (overlay 
   (if (= numero 0)
       (text "" TEXTO-TAMANO "white") ; Si es 0, texto vacío en lugar de no poner nada
       (text (number->string numero) TEXTO-TAMANO (color-texto numero)))
   (rectangle TAMANO-CELDA TAMANO-CELDA "outline" "gray") ; Borde
   (rectangle TAMANO-CELDA TAMANO-CELDA "solid" (color-fondo numero))))

; 2. Dibuja una fila completa 
(define (dibujar-fila fila)
  (cond [(null? fila) (empty-scene 0 0)]
        [else (beside (dibujar-celda (car fila))
                      (dibujar-fila (cdr fila)))]))

; 3. Dibuja el tablero completo 
(define (dibujar-tablero tablero)
  (cond [(null? tablero) (empty-scene 0 0)]
        [else (above (dibujar-fila (car tablero))
                     (dibujar-tablero (cdr tablero)))]))

; 4. Pantalla final con el fondo gris oscuro
(define (pantalla-juego tablero)
  (define fondo-gris (rectangle (+ 20 (* (contador-columnas (car tablero)) TAMANO-CELDA)) (+ 20 (* (contador-filas tablero) TAMANO-CELDA)) "solid" "dimgray"))
  (define tablero-dibujado (dibujar-tablero tablero))
  (define fondo-completo (overlay tablero-dibujado fondo-gris))
  
  (cond
    ; Si ganó (tiene un 2048)
    [(won? tablero)
     (overlay
      (rectangle (+ 20 (* (contador-columnas (car tablero)) TAMANO-CELDA)) (+ 20 (* (contador-filas tablero)  TAMANO-CELDA)) "solid" (make-color 0 0 0 40))
      (text "¡VICTORIA!" 50 "green")
      fondo-completo)]
    
    ; Si perdió (sin movimientos posibles)
    [(lost? tablero)
     (overlay
      (rectangle (+ 20 (* (contador-columnas (car tablero)) TAMANO-CELDA)) (+ 20 (* (contador-filas tablero) TAMANO-CELDA)) "solid" (make-color 0 0 0 40))
      (text "GAME OVER" 50 "red")
      fondo-completo)]
    
    ; Juego en curso
    [else
     fondo-completo]))

; ===================================================================
; MANEJADOR DE EVENTOS (Teclado)
; ===================================================================

; Esta función intercepta las teclas. 
; Entradas: 
;  - tablero: La matriz actual (lista de listas)
;  - tecla: Un string con la tecla presionada ("up", "down", "left", "right")
; Salida: 
;  - Una matriz NUEVA con los movimientos ya aplicados.
(define (manejar-teclado tablero tecla)
  (cond 
    ; Si la tecla es la flecha izquierda...
    ; Toma la matriz actual, pásasela a la función 'mover-izquierda' de SourceCode.rkt,
    ; y el resultado que esa función escupa, se convertirá en el nuevo tablero.

     [(key=? tecla "left")  (
                             if (equal? (Derecha-Izquierda tablero) tablero) tablero
                                (agregar-celda-aleatoria (Derecha-Izquierda tablero)
                             ))]

    ;lo mismo en la demas direcciones, pero con sus respectivas funciones de movimiento.
     [(key=? tecla "right") (
                             if (equal? (Izquierda-Derecha tablero) tablero) tablero
                                (agregar-celda-aleatoria (Izquierda-Derecha tablero)
                             ))]
     [(key=? tecla "up")    (
                             if (equal? (Abajo-Arriba tablero) tablero) tablero
                                (agregar-celda-aleatoria (Abajo-Arriba tablero)
                             ))]
     [(key=? tecla "down")  (
                             if (equal? (Arriba-Abajo tablero) tablero) tablero
                                (agregar-celda-aleatoria (Arriba-Abajo tablero)
                             ))]
    
    ; Si el usuario presiona cualquier otra tecla (la letra 'A', espacio, enter...),
    ; la función condicional cae en este 'else'. 
    ; Al retornar 'tablero', le decimos a Racket: "No hagas ningún cambio matemático, 
    ; devuelve la misma matriz exacta que entró".
    
    [else tablero]))

; ===================================================================
; INICIALIZACIÓN (BIG-BANG)
; ===================================================================


#|
Hay errores en el big-bang

Estado inicial-clausulas entre corchetes
Hay que agregar logica para celdas aleatorias y de won y lost

|#
; Función que arranca todo.
(define (iniciar-juego tablero)
  (big-bang tablero
    [to-draw pantalla-juego]
    [on-key manejar-teclado]
    [name "2048 en Racket - TEC"])) ; Titulo de la ventana

;Matriz de prueba para verificar condición de victoria
(define matriz-ganar '((16 8 4 1024)
                 (8 4 0 1024)
                 (4 16 0 0)))

;Matriz de prueba para verificar condición de perder
(define matriz-perder '((16 8 4 128)
                        (8 4 8 32)
                        (4 0 0 0)))


; Función principal
(define (main row col)
        (iniciar-juego (agregar-celdas-iniciales (tablero-mayor-a-1 row col)))
  )

(define (main-prueba tablero)
        (iniciar-juego (agregar-celdas-iniciales tablero))
  )