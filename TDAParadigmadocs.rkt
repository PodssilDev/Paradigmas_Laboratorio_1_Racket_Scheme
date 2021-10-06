#lang racket
; Se requiere el uso del TDA Fecha para elaborar la construccion del TDA ParadigmaDocs
(require "TDAFecha.rkt")

; TDA ParadigmaDocs
; REPRESENTACION
; Este TDA representa corresponde a una plataforma de documentos, que contiene en una lista el nombre de la plataforma
; una fecha valida de creacion de la plataforma, y las funciones encryptFn y decryptFn, las cuales modifican un texto

; CONSTRUCTORES

; Primero, se definen las funcioens encryptFn y decryptFn. Ambas funciones hacen exactamente lo mismo.
; Dominio: Un texto (string)
; Reocrrido: Un texto (string)
; Descripcion: Funcion que recibe un texto y lo modifica. En este caso, la funcion entrega el texto al revez
; Tipo de Recursion: No se utiliza recursion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

; Se crea el constructor de paradigmadocs
; Dominio: Strings para el caso de name, encryptFn y decryptFn y Enteros para el caso de date
; Recorrido: Una lista que contiene a name, date, el texto de encryptFn y el texto de decryptFn
; Descripcion: Corresponde al constructor de paradigmadocs.
; Tipo de recursion: No se utiliza recursion
(define (paradigmadocs name fecha encryptFn decryptFunction)
  (list name fecha encryptFn decryptFunction)
  )

; FUNCIONES DE PERTENENCIA

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Un booleano
; Descripcion: Comprueba si el formato del TDA es correcto (name corresponde a string)
; Tipo de recursion: No se utiliza recursion
(define (isParadigmadocs? docs)
  (if (string? (car docs))
  #t
  #f
  )
  )

; SELECTORES

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: String
; Descripcion: Obtiene el nombre del documento en formato string
; Tipo de recursion: No se utiliza recursion
(define (getNombre docs)
  (if (isParadigmadocs? docs)
      (car docs)
      null)
  )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Lista de Enteros
; Descripcion: Obtiene la fecha de creacion del documento, en formato lista
; Tipo de recursion: No se utiliza recursion
(define (getFecha docs)
  (if (isParadigmadocs? docs)
      (car (cdr docs))
      null)
  )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Un texto (String)
; Descripcion: Obtiene el texto guardado en la posicion de encryptFn
; Tipo de recursion: No se utiliza recursion
(define (getEncrypt docs)
  (if (isParadigmadocs? docs)
      (car (cdr (cdr docs)))
      null)
  )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Un texto (String)
; Descripcion: Obtiene el texto guardado en la posicion de decryptFn
; Tipo de recursion: No se utiliza recursion
(define (getDecrypt docs)
  (if (isParadigmadocs? docs)
      (car (cdr (cdr (cdr docs))))
      null)
  )

; MODIFICADORES

; Dominio: Un documento de tipo paradigmadocs y un string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica el nombre del documento. Si el nombre es invalido, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setNombre docs newNombre)
  (if (and (isParadigmadocs? docs)(string? newNombre))
      (paradigmadocs newNombre (getFecha docs) (getEncrypt docs) (getDecrypt docs))
      docs))

; Dominio: Un doumento de tipo paradigmadocs y una lista de enteros
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica la fecha del documento. Si la fecha es invalida, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setFecha docs newFecha)
  (if (and(and(isParadigmadocs? docs)(list? newFecha))(not(empty? newFecha)))
      (paradigmadocs (getNombre docs) newFecha (getEncrypt docs) (getDecrypt docs))
      docs))

; Dominio: Un documento de tipo paradigmadocs y un string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica el texto en la posicion de encryptFn. Si el texto es invalido, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setEncrypt docs newEncrypt)
  (if (and (isParadigmadocs? docs)(string? newEncrypt))
      (paradigmadocs (getNombre docs) (getFecha docs) newEncrypt (getDecrypt docs))
      docs))

; Dominio: Un documento de tipo paradigmadocs y un string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica el texto en la posicion de decryptFn. Si el texto es invalido, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setDecrypt docs newDecrypt)
  (if (and (isParadigmadocs? docs)(string? newDecrypt))
      (paradigmadocs (getNombre docs) (getFecha docs) (getEncrypt docs) newDecrypt)
      docs))

; OTRAS FUNCIONES

; No se tienen mas funciones extras.

; EJEMPLOS

(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn decryptFn)) 
(define pertenencia1 (isParadigmadocs? emptyGDocs))
(define nombre (getNombre emptyGDocs))
(define fecha (getFecha emptyGDocs))
(define mensaje (getDecrypt emptyGDocs))
(define cambio (setNombre emptyGDocs 1233)) ; Ejemplo erroneo
(define fechacambio (setFecha emptyGDocs (date 'hi' 10 2021)))
(define string(setEncrypt emptyGDocs "Un nuevo mensaje para el dia"))
(define string2(setDecrypt emptyGDocs "Vamos que se puede"))