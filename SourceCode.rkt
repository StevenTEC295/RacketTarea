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

(define matriz '((2 4 8) (8 16 8) (4 64 8)))


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

Abajo arriba:

(2 4 8)                                (2 2 4)              (0 4 4)                   (0 4 0)
(2 16 8)  -> (Invertir)->(Transpuesta) (4 16 8) -> (Cambio) (4 16 8) -> (Transpuesta) (4 16 8) -> (Invertir)
(4 4 8)                                (8 8 8)              (0 8 16)                  (4 8 16)

Arriba abajo:

(2 4 8)                    (2 2 4)              (0 4 4)                   (0 4 0)
(2 16 8)  -> (Transpuesta) (4 16 8) -> (Cambio) (4 16 8) -> (Transpuesta) (4 16 8)
(4 4 8)                    (8 8 8)              (0 8 16)                  (4 8 16)

Arriba abajo:

(2 4 8)                    (2 2 4)              (0 4 4)                   (0 4 0)
(2 16 8)  -> (Transpuesta) (4 16 8) -> (Cambio) (4 16 8) -> (Transpuesta) (4 16 8)
(4 4 8)                    (8 8 8)              (0 8 16)                  (4 8 16)

Izquierda-Derecha:

(2 4 8)    (8 4 2)    (8 4 2)   (2 4 8)
(2 2 2) -> (2 2 2) -> (0 2 4)-> (4 2 0)
(4 4 8)    (8 4 4)    (0 8 8)   (8 8 0)

|#

(define(Derecha-Izquierda matriz)
                            
                           (cond((equal? matriz '())'())
                                (else(cons(rellenar-ceros (combinar-auxiliar (car matriz)) (contador-columnas (car matriz)))(Derecha-Izquierda (cdr matriz)) 
                                          )))

                           )

(define(Izquierda-Derecha matriz)
                            
                           (cond((equal? matriz '())'())
                                (else(cons(reverse (rellenar-ceros(combinar-auxiliar (reverse(car matriz))) (contador-columnas (car matriz))))(Izquierda-Derecha (cdr matriz)) 
                                          )))

                           )                  
(define(Arriba-Abajo matriz)(
                          transpuesta(Izquierda-Derecha(transpuesta matriz)))
                                     )

(define(Abajo-Arriba matriz)(
                          transpuesta(Derecha-Izquierda(transpuesta matriz)))
                                     )



#|
Izquierda:
-> (funcion-para-aplicar-cadafila matriz)
   -> si matriz = ():
      retornar ()
   -> cambio(car matriz)
   -> retorna funcion(cdr matriz)
derecha:
-> (funcion-para-aplicar-cadafila matriz)
   -> si matriz = ():
      retornar ()
   -> reverse(cambio(reverse(car matriz)))
   -> retorna funcion(cdr matriz)
Abajo:
matriz = Transpuesta(matriz)
-> (funcion-para-aplicar-cadafila matriz)
   -> si matriz = ():
      retornar ()
   -> cambio(car matriz)
   -> retorna funcion(cdr matriz)
matriz = Transpuesta(matriz)

|#
#|
;Funciones de prueba
(define (Test-Arriba-Abajo)
  (mostrar-tablero matriz)
  (mostrar-tablero (Arriba-Abajo matriz))
  )

(define (Test-Abajo-Arriba)
  (mostrar-tablero matriz)
  (mostrar-tablero (Abajo-Arriba matriz))
  )
|#

(define (posiciones-vacias tablero row col)
  (cond ((equal? tablero '()) '())
        ((equal? (car tablero) '()) (posiciones-vacias (cdr tablero) (+ row 1) 0))
        ((= (car (car tablero)) 0) (cons (cons row col)
                                                     (posiciones-vacias (cons (cdr (car tablero)) (cdr tablero))
                                                                        row (+ col 1))))
        (else (posiciones-vacias (cons (cdr (car tablero)) (cdr tablero))
                                                                        row (+ col 1)))))

;Funciones de agregar celda aleatoria
(define(agregar-celda-aleatoria tablero) ;Agrega un valor a una celda entre 1 y 2 si existen campos vacíos
  (define lista-posiciones-vacias (posiciones-vacias tablero 0 0))
  (if (equal? lista-posiciones-vacias '())tablero
      (agregar-celda-aleatoria-aux tablero lista-posiciones-vacias (random(contador-filas lista-posiciones-vacias )))
      ))

(define (agregar-celda-aleatoria-aux tablero pos-vacias index); Se genera el valor random con 90% que sea 2 y 10% que sea 4
  (define pos (list-ref pos-vacias index))
  (define row (car pos))
  (define col (cdr pos))
  (define valor (if (< (random 10) 9) 2 4))
  (set-cell tablero row col valor)
  )
(define (agregar-celdas-iniciales tablero)
  (agregar-celda-aleatoria(agregar-celda-aleatoria tablero))
  )

;Funcionalidades de Perdida y Victoria

;Victoria
(define (won? tablero)
  (cond [(equal? tablero '()) #f] ; si se revisaron todas las filas y no estaba 2048 es falso
        [(2048? (car tablero))#t]
        [else (won? (cdr tablero))]
                               ))
(define (2048? row)
  (cond
    [(equal? row '())#f]; si se revisaron todos los campos y no estaba 2048 retorna falso
    [(= (car row) 2048)#t] 
    [else (2048? (cdr row))]
    )
  )

;Derrota
(define (lost? tablero)
  (and (sin-espacios-vacios? tablero)(sin-movimientos-posibles? tablero))
  )
;Funcion que verifica que no existan espacios vacios
(define (sin-espacios-vacios? tablero)
  (equal? (posiciones-vacias tablero 0 0) '())


  )
;Funcion que verifica que no hayan más movimientos posibles
(define (sin-movimientos-posibles? tablero)
  (and (sin-movimientos-horizontales? tablero)(sin-movimientos-verticales? tablero))
  )
;Funcion que verifica que no hayan movimientos horizontales posibles
(define (sin-movimientos-horizontales? tablero)
  (cond
    [(equal? tablero '())#t]
    [(combinacion-horizontal? (car tablero))#f]
    [else (sin-movimientos-horizontales? (cdr tablero))]
    )
  )

(define (combinacion-horizontal? row)
  (cond
    [(equal? (cdr row) '()) #f]
    [(= (obtener-celda-fila row 0) (obtener-celda-fila row 1)) #t]
    [else(combinacion-horizontal? (cdr row))]
    )

  )
(define (sin-movimientos-verticales? tablero)
  (sin-movimientos-horizontales? (transpuesta tablero))
  )

; Exportar funciones públicas
(provide tablero-mayor-a-1
         agregar-celdas-iniciales
         Derecha-Izquierda
         Izquierda-Derecha
         Arriba-Abajo
         Abajo-Arriba
         agregar-celda-aleatoria
         won?
         lost?
         )

