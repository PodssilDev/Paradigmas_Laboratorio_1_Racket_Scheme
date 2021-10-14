#lang racket
; NOTA: ESTE TDA YA ESTABA CREADO Y FUE DESCARGADO DE UVIRTUAL. SOLO SE HICIERON ALGUNAS PEQUEÑAS MODIFICACIONES

;-----------------------------------TDA FECHA-----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; Este TDA representa una fecha con dia, mes y año. Se guarda en una lista con el mismo orden
; (entero X entero X entero)
; (list dia mes año) 

;-----------------------------------CONSTRUCTORES-------------------------------------------------------------

; Dominio: entero X entero X entero
; Descripción: Permite crear una fecha
; Recorrido: lista
; Tipo de recursion: No se utiliza recursion
(define (date d m a)
  (if (and (integer? d) (integer? m) (integer? a)
           (> d 0) (> m 0) (< m 13) (not (= a 0))
           (<=  d (getDiasDelMes m a)))
      (list d m a)
      null
  )
)

;-----------------------------------FUNCIONES DE PERTENENCIA-----------------------------------------------------------------

; Dominio: Elemento de cualquier tipo
; Recorrido: Un Booleano
; Descripción: Función que permite determinar si un elemento cualquiera es del tipo fecha
;             se implementa a partir del constructor
;             evaluando el retorno
; Tipo de recursion: No se utiliza recursion
(define (date? f)
  (and (list? f) 
       (= (length f) 3)
       (not (null? (date (car f) (cadr f) (caddr f)))))
)

;-----------------------------------SELECTORES-------------------------------------------------------------------------------

; Dominio: fecha
; Recorrido: entero
; Descripción: Función que retorna el día en una fecha
; Tipo de recursion: No se utiliza recursion
(define (getDia f)
   (if (date? f)
       (car f)
       0
   )
 )

; Dominio: fecha
; Recorrido: entero
; Descripción: Función que retorna el mes en una fecha
; Tipo de recursion: No se utiliza recursion
(define (getMes f)
 (if (date? f)
       (cadr f)
       0
   )  
)

; Dominio: fecha
; Recorrido: entero
; Descripción: Función que retorna el año en una fecha
; Tipo de recursion: No se utiliza recursion
(define (getAgno f)
 (if (date? f)
       (caddr f)
       0
   )
 )

;-----------------------------------MODIFICADORES-------------------------------------------------------------------------------

; Dominio: fecha x entero
; Recorrido: fecha
; Descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al día
; Tipo de recursion: No se utiliza recursion
(define (setDia f nd)
  (if (date? f)
      (date nd (getMes f) (getAgno f))
      null
   )
 )

; Dominio: fecha x entero
; Recorrido: fecha
; Descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al mes
; Tipo de recursion: No se utiliza recursion
(define (setMes f nm)
  (if (date? f)
      (date (getDia f) nm (getAgno f))
      null
   )
 )

; Dominio: fecha x entero
; Recorrido: fecha
; Descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al año
; Tipo de recursion: No se utiliza recursion
(define (setAgno f na)
  (if (date? f)
      (date (getDia f) (getMes f) na)
      null
   )
 )

 ;-----------------------------------OTRAS FUNCIONES-------------------------------------------------------------------------------

;descripción: función que transforma una fecha en string
;dom: fecha
;rec: string
(define (date->string f)
  (if (date? f)
      (string-append (number->string (getDia f)) " de " (getMonthName (getMes f)) " de " (number->string (getAgno f)))
      ""
   )
)

;descripción: función que retorna el siguiente año para una fecha
;dom: fecha
;rec: entero
(define (nextAgno f)
  (if (date? f)
      (setAgno f (+ 1 (getAgno f)))
      null
  )
 )

;LAS SIGUIENTES TRES FUNCIONES SON COMPLEMENTARIAS/AUXILIARES AL TDA. NO FORMAN PARTE DEL TDA FECHA, PERO
;EL TDA FECHA LAS EMPLEA PARA PODER REALIZAR SU TRABAJO. ESTAS FUNCIONES DE IGUAL FORMA PUEDEN
;SER EMPLEADAS INDEPENDIENTEMENTE DE LA EXISTENCIA DEL TDA FECHA, POR LO QUE NO EXISTE ACOPLAMIENTO CON EL TDA.
;POR OTRO LADO, EL TDA SEGUN LA IMPLEMENTACION REALIZADA A CONTINUACION, ESTA ACOPLADO A ESTAS FUNCIONES.

;descripción: función para determinar si un año es bisiesto
;dom: entero
;rec: boolean
(define (bisiesto? a)
  (if (and (integer? a) (not (= a 0)))
      (or (= (remainder a 400) 0)
              (and (= (remainder a 4) 0) (not (= (remainder a 100) 0))))
      #f
  )
)

;descripción: función para determinar los días de un mes
;dom: entero X entero
;rec: entero
(define (getDiasDelMes m a)
  (if (and (integer? m) (integer? a) (not (= a 0))
           (> m 0) (< m 13))
           (if (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12))
                31
                (if (= m 2)
                    (if (bisiesto? a)
                        29
                        28
                    )
                    30
                )
            )
           0
   )
 )

;descripción: función que transforma un mes entero a su nombre en string
;dom: entero
;rec: string
(define (getMonthName m)
      (cond ((not (and (integer? m) (> m 0) (< m 13))) "")
            ((= m 1) "Enero")
            ((= m 2) "Febrero")
            ((= m 3) "Marzo")
            ((= m 4) "Abril")
            ((= m 5) "Mayo")
            ((= m 6) "Junio")
            ((= m 7) "Julio")
            ((= m 8) "Agosto")
            ((= m 9) "Septiembre")
            ((= m 10) "Octubre")
            ((= m 11) "Noviembre")
            ((= m 12) "Diciembre")
       )
  )

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
