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
  (cond [(= numero 0)    "lightgray"]
        [(= numero 2)    "white"]
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
  (if (= numero 4) "dimgray" "white"))

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
  (overlay
   (dibujar-tablero tablero)
   (rectangle (+ 20 (* 4 TAMANO-CELDA)) (+ 20 (* 4 TAMANO-CELDA)) "solid" "dimgray")))

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

    ; [(key=? tecla "left")  (mover-izquierda tablero)]

    ;lo mismo en la demas direcciones, pero con sus respectivas funciones de movimiento.
    ; [(key=? tecla "right") (mover-derecha tablero)]
    ; [(key=? tecla "up")    (mover-arriba tablero)]
    ; [(key=? tecla "down")  (mover-abajo tablero)]
    
    ; Si el usuario presiona cualquier otra tecla (la letra 'A', espacio, enter...),
    ; la función condicional cae en este 'else'. 
    ; Al retornar 'tablero', le decimos a Racket: "No hagas ningún cambio matemático, 
    ; devuelve la misma matriz exacta que entró".
    
    [else tablero]))

; ===================================================================
; INICIALIZACIÓN (BIG-BANG)
; ===================================================================

; Función que arranca todo.
(define (iniciar-juego tablero-inicial)
  (big-bang tablero-inicial
    (to-draw pantalla-juego)      
    (on-key manejar-teclado)      ;
    (name "2048 en Racket - TEC"))) ; Titulo de la ventana

; Matriz para hacer pruebas

(define tablero-prueba 
  '((0 2 4 8)
    (16 32 64 128)
    (256 512 1024 2048)
    (0 0 2 2)))