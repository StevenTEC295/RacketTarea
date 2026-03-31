#lang racket/base
;Lógica del juego

;Función auxiliar para crear fila de ceros

(define (filazeros n)
  (if (= n 0) '()
      (cons 0(filazeros (- n 1))
  )
  ))


;Funcion para crear tablero

(define (tablero rows cols)
  ;Agregar verificacion rows>1 and cols>1
  (if (= rows 0) '()
      (cons(filazeros cols)(tablero (- rows 1) cols))
      )
  )


;Funcion auxiliar con verificacion de columnas y filas > 1
(define(tablero-mayor-a-1 rows cols)
  
  (if (and (> rows 1) (> cols 1))
      (tablero rows cols)
      (display "")

  ))
;Funcion para mostrar tablero
(define (mostrar-tablero tablero)

  (if (list? tablero)
      (if (equal? tablero '()) (display "\n")
          (begin (display (car tablero))(newline)(mostrar-tablero (cdr tablero))


  ))(display (string-append "Error:" " " "Las columnas y las filas deben ser mayores a 1"))))
;Obtener numero de filas

(define (contador-filas tablero)
  (if (equal? tablero '()) 0 (+ 1 (contador-filas (cdr tablero))))
)

;como parámetro debe ponerse (car(tablero))
;TODO: encontrar algoritmo para no tener que agregar car en el parámetro
(define (contador-columnas columna)
  (if (equal? columna '()) 0 (+ 1 (contador-columnas (cdr columna))))
)

(define matriz '((2 2 8) (2 2 2) (7 4 5)))


(define (obtener-fila tablero row)
  (list-ref tablero row)
  )
(define (obtener-celda tablero row col)
  (list-ref(list-ref tablero row) col)
)

(define (obtener-celda-fila row col)
  (list-ref row col)
)

(define (set-list lst n valor)
  (if (= n 0) (cons valor (cdr lst))
      (cons (car lst) (set-list (cdr lst) (- n 1) valor))))


#|
valor = 0
n = 2
[1 2 3 2 5]
n = 1
(1 + (2 3 2 5))
n = 0
(1 2 +(3 2 5))
-> (1 2 0 2 5)
|#

(define (set-cell tablero row col valor)
  (set-list tablero row
            (set-list (list-ref tablero row) col valor))

)

(define (remover-ceros row)
  (cond ((equal? row '()) '())
        ((= (car row) 0) (remover-ceros (cdr row)))
        (else (cons (car row) (remover-ceros (cdr row))))
        ))

(define (combinar-auxiliar row)
  (cond ((equal? row '()) '())
        ((equal? (cdr row) '()) (cons (car row) '()))
        ((= (obtener-celda-fila row 0) (obtener-celda-fila row 1))
        (cons (* 2 (obtener-celda-fila row 0)) (combinar-auxiliar (cddr row))))
        (else (cons (obtener-celda-fila row 0) (combinar-auxiliar (cdr row))))
        
    ))

(define (rellenar-ceros row row-size)
  (cond ((>= (contador-columnas row) row-size) row)
        (else (rellenar-ceros (agregar-elemento-row row 0) row-size))
        ))

(define (agregar-elemento-row row value)
  (cond ((equal?  row '()) (cons value '()))
        (else (cons (car row) (agregar-elemento-row (cdr row) value)))))

(define (combinar-row row)
  (rellenar-ceros (combinar-auxiliar row) (contador-columnas row)))

;(combinar-row (obtener-fila matriz 0)) == movimiento izq
;(reverse(combinar-row (reverse (obtener-fila matriz 0)))) == movimiento der

(define (sacar-1f mat)
  (cond( (null? mat) '())
       (else(cons (car (car mat))(sacar-1f (cdr mat))))))

(define (borrar-1f mat)
  (cond((null? mat) '())
      (else (cons (cdr (car mat))(borrar-1f (cdr mat))))))

(define (transpuesta mat )
  (cond ((null? (car mat)) '())
         (else (cons (sacar-1f mat)(transpuesta (borrar-1f mat))))))
#|
Aplicar los cambios (derecha a izq)Hecho - (izquierda a derecha)En proceso - (Arriba a abajo) -(Abajo a arriba)


Arriba abajo:

(2 4 8)                    (2 2 4)              (0 4 4)                   (0 4 0)
(2 16 8)  -> (Transpuesta) (4 16 8) -> (Cambio) (4 16 8) -> (Transpuesta) (4 16 8)
(4 4 8)                    (8 8 8)              (0 8 16)                  (4 8 16)


|#


