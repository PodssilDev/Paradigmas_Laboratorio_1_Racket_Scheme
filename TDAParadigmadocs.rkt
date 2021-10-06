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
; MODIFICADORES
; OTRAS FUNCIONES
; EJEMPLOS
(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn encryptFn))
(define pertenencia1 (isParadigmadocs? emptyGDocs))