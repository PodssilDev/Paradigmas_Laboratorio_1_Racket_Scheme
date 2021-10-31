#lang racket
; NOTA: No fue necesario crear funciones de pertenencia y modificadores para este TDA.
;-----------------------------------TDA ACCESS------------------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------------

; Este TDA representa a la lista de accesos, donde se tiene una lista de sublistas, donde cada sublista es
; un un permiso

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------------

; Dominio: Listas de string
; Recorrido: Lista de listas (cada lista contiene a un permiso)
; Descripcion: Crea una lista de sublistas, donde cada sublista es un permiso
; Tipo de recursion: No se utiliza recursion
(define (access . accessess)
   accessess)

;-----------------------------------SELECTORES------------------------------------------------------------------------

; Dominio: Una lista que contiene una lista de accesos
; Recorrido: Una lista qud contiene sublistas de accesso
; Descripcion: Funcion que obtiene la verdadera lista de accesses (elimina la lista de sobra que se forma
; al utilizar lambda)
; Tipo de recursion: No se utiliza recursion
(define (getVerdaderaLista listaccesses)
  (car listaccesses))

;-----------------------------------OTRAS FUNCIONES-------------------------------------------------------------------

; Dominio: Dos listas, una de ellas es una lista de sublistas
; Recorrido: Una lista
; Descripcion: Funcion que une la lista de accesos, creando una sola lista de sublistas
; Tipo de recursion: No se utiliza recursion
(define (unionAccesos list1 list2)
  (cons list1 (getVerdaderaLista list2)))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
