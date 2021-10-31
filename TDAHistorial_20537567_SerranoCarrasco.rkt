#lang racket
; Se necesita del TDA Fecha para la construccion del TDA Historial
(require "TDAFecha_20537567_SerranoCarrasco.rkt")

; NOTA: No fue necesario construir modificadores para este TDA.
;-----------------------------------TDA HISTORIAL---------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------

; Este TDA corresponde a una version de un documento el cual se guarda en el historial de versiones.
; Se guarda en una lista y una fecha de tipo date, un texto de tipo string y un ID de tipo integer.
; (date X string X ID)

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------

; Dominio: Una fecha de tipo fecha, un contenido de tipo string y un ID de tipo integer
; Recorrido: Una lista de tipo historial
; Descripcion: Crea una version del historial de versiones
; Tipo de recursion: No se utiliza recursion
(define (historial fecha contenido id)
  (list fecha contenido id))

;-----------------------------------FUNCIONES DE PERTENENCIA----------------------------------------------------

; Dominio: Una lista de historial de tipo historial
; Recorrido: Un booleano
; Descripcion: Verifica si el formato de los elementos de la version de historial es correcto
(define (isHistorial? historial)
  (if(and(and(date? (car historial))(string? (car(cdr historial))))(integer? (car(cdr(cdr historial)))))
     #t
     #f))

;-----------------------------------SELECTORES-----------------------------------------------------------------

; Dominio: Una lista de una version de un historial
; Recorrido: Una fecha de tipo fecha 
; Descripcion: Funcion que obtiene la fecha de registro de una version de un historial correspondiente a un documento
; Tipo de recursion: No se utiliza recursion
(define (getFechaHistorial listhistorial)
  (if (isHistorial? listhistorial)
      (car listhistorial)
      null))

; Dominio: Una lista de una version de un historial
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que obtiene el texto de una version de un historial correspondiente a un documento
; Tipo de recursion: No se utiliza recursion
(define (getTextoHistorial listhistorial)
  (if (isHistorial? listhistorial)
      (car (cdr listhistorial))
      null))

; Dominio: Una lista de una version de un historial
; Recorrido: Un ID de tipo integer
; Descripcion: Funcion que obtiene el ID de una version de un historial correspondiente a un documento
; Tipo de recursion: No se utiliza recursion
(define (getIDHistorial listhistorial)
  (if (isHistorial? listhistorial)
      (car (cdr (cdr listhistorial)))
      null))

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Dominio: Una lista de historial de versiones (Contiene sublistas con una fecha, texto y un ID de version) y un ID de version de tipo integer
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que permite obtener al texto correcto que se quiere restaurar. Si el texto no se logra encontrar, se retorna null
; Tipo de Recursion: Recursion de Cola
; Justificacion de Recursion: Permite recorrer toda la lista del historial de versiones
(define(obtenerVersion listHistorial idHist)
  (if(eq? listHistorial null)
     null
     (if (eq? idHist (getIDHistorial(car listHistorial)))
         (getTextoHistorial(car listHistorial))
         (obtenerVersion (cdr listHistorial) idHist))))

; Dominio: Una lista de historial de un documento y un texto de tipo string
; Recorrido: Un booleano
; Descripcion: Funcion que recorre todo el historial de versiones de un documento para encontrar si un texto se encuentra en alguna version
; Tipo de Recursion: Recursion de Cola
; Justificacion de recursion: Permite recorrer toda la lista del historial para encontrar si existe una version que tenga un texto en especifico
(define (encontrarTextoHistorial listhistorial texto )
  (if(null? listhistorial)
     #f
     (if(not( eq? (length(string-split (getTextoHistorial(car listhistorial)) texto)) (length(list (getTextoHistorial(car listhistorial))))))
        #t
        (if (not(eq? (length(string->list(car(string-split(getTextoHistorial(car listhistorial)) texto)))) (length(string->list(getTextoHistorial(car listhistorial))))))
            #t
            (encontrarTextoHistorial (cdr listhistorial) texto)))))

; Dominio: Una lista de historial
; Recorrido: Un string
; Descripcion: Transforma una lista de historial en un string
; Tipo de recursion: No se utiliza recursion
(define (historialToString listhistorial)
  (string-join (list "Version N°"(number->string (getIDHistorial listhistorial))":" "*"(getTextoHistorial listhistorial)"*" "version guardada el dia" (date->string (getFechaHistorial listhistorial))"\n")))

; Dominio: Una lista de una version de un documento, pertenenciente al historial
; Recorrido: Un booleano
; Descripcion: Funcion que se utiliza para filtrar si una version tiene comentarios o no
; Tipo de recursion: No se utiliza recursion
(define (sinComentarios historial)
  (if (string-contains? (getTextoHistorial historial) "&C&")
      #f
      #t))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
