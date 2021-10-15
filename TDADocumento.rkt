#lang racket
; Se necesita del TDA Fecha para la construccion del TDA Documento
(require "TDAFecha.rkt")

;-----------------------------------TDA DOCUMENTO---------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------

; Este TDA corresponde a un Documento, donde se guarda el autor del documento, la fecha de creacion del documento, el nombre del documento,
; el contenido del documento, una lista de permisos (de escritura, lectura o comentarios), un historial de versiones del contenido y
; un ID unico de documento, donde el primero siempre parte con ID 0 y asi succesivamente. Todo en una lista con el mismo orden
; (string X fecha X string X string X list X list X integer)

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------

; Dominio: Un nombre de tipo string, una fecha de tipo fecha, un nombre de tipo string, un texto de tipo string y un ID de tipo integer
; Recorrido: Un documento de tipo documento (lista)
; Descripcion: Crea a un documento
; Tipo de recursion: No se utiliza recursion
(define(documento autor fecha nombre_documento contenido_documento ID)
(list autor fecha nombre_documento contenido_documento (list)(list (list contenido_documento 0)) ID))

;-----------------------------------FUNCIONES DE PERTENENCIA----------------------------------------------------

; Dominio: Un documento de tipo documento
; Recorrido: Un booleano
; Descripcion: Verifica si el formato del documento es correcto (si el nombre, autor y contenido son strings y la fecha es de tipo fecha y si el ID es un integer)
; Tipo de recursion: No se utiliza recursion
(define(isDocumento? document)
  (if (and(and(and(and(string? (car document))(date? (car(cdr document))))(string? (car(cdr(cdr document)))))(string? (car(cdr(cdr(cdr document))))))(integer? (car (cdr (cdr (cdr (cdr (cdr (cdr document)))))))))
      #t
      #f))

;-----------------------------------SELECTORES-----------------------------------------------------------------

; Dominio: Un documento de tipo documento
; Recorrido: Un autor de tipo string
; Descripcion: Funcion que obtiene el autor de un documento
; Tipo de recursion: No se utiliza recursion
(define (getAutorDocumento document)
  (if (isDocumento? document)
      (car document)
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Una fecha de tipo fecha
; Descripcion: Funcion que obtiene la fecha de creacion de un documento
; Tipo de recursion: No se utiliza recursion
(define (getFechaDocumento document)
  (if (isDocumento? document)
      (car (cdr document))
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Un nombre de un documento (string)
; Descripcion: Funcion que obtiene el nombre de un documento
; Tipo de recursion: No se utiliza recursion
(define (getNombreDocumento document)
  (if (isDocumento? document)
      (car (cdr (cdr document)))
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que obtiene el contenido (texto) de un documento
; Tipo de recursion: No se utiliza recursion
(define (getContenidoDocumento document)
  (if (isDocumento? document)
      (car (cdr (cdr (cdr document))))
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Una lista de permisos de tipo list
; Descripcion: Funcion que obtiene una lista de permisos de un documento (No incluye al creador, que por defecto tiene todos los permisos)
; Tipo de recursion: No se utiliza recursion
(define (getPermisosDocumento document)
  (if (isDocumento? document)
      (car (cdr (cdr (cdr (cdr document)))))
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Una lista de historial del texto del documento de tipo list
; Descripcion: Funcion que obtiene una lista que contiene todas las versiones del contenido (texto) del documento
; Tipo de recursion: No se utiliza recursion
(define (getHistorialDocumento document)
  (if (isDocumento? document)
      (car (cdr (cdr (cdr (cdr (cdr document))))))
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Un ID de documento de tipo integer
; Descripcion: Funcion que el ID unico de un documento
; Tipo de recursion: No se utiliza recursion
(define (getIDDocumento document)
  (if (isDocumento? document)
      (car (cdr (cdr (cdr (cdr (cdr (cdr document)))))))
      null))

;-----------------------------------MODIFICADORES----------------------------------------------------------------

; Dominio: Un documento de tipo documento y un nuevo autor de tipo string
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el nombre del autor. Si el formato del autor es incorrecto, retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setAutorDocumento document newAutor)
  (if (and(isDocumento? document)(string? newAutor))
      (list newAutor (getFechaDocumento document)(getNombreDocumento document)(getContenidoDocumento document)(getPermisosDocumento document)(getHistorialDocumento document)(getIDDocumento document))
      document))

; Dominio: Un documento de tipo documento y una nueva fecha de tipo fecha (list)
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia la fecha de creacion del documento. Si el formato de la fecha es incorrecto,
; retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setFechaDocumento document newFecha)
  (if (and(and(isDocumento? document)(list? newFecha))(not(empty? newFecha)))
      (list (getAutorDocumento document) newFecha (getNombreDocumento document)(getContenidoDocumento document)(getPermisosDocumento document)(getHistorialDocumento document)(getIDDocumento document))
      document))

; Dominio: Un documento de tipo documento y un nuevo nombre de tipo string
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el nombre del documento. Si el formato del nombre es incorrecto, retorna el
; documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setNombreDocumento document newNombre)
  (if (and(isDocumento? document)(string? newNombre))
      (list (getAutorDocumento document) (getFechaDocumento document)newNombre(getContenidoDocumento document)(getPermisosDocumento document)(getHistorialDocumento document)(getIDDocumento document))
      document))

; Dominio: Un documento de tipo documento y un nuevo contenido (texto) de tipo string
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el contenido (texto) del documento. Si el formato del texto es incorrecto,
; retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setContenidoDocumento document newContenido)
  (if (and(isDocumento? document)(string? newContenido))
      (list (getAutorDocumento document) (getFechaDocumento document)(getNombreDocumento document)newContenido(getPermisosDocumento document)(cons(getContenidoDocumento document)(getHistorialDocumento document))(getIDDocumento document))
      document))

; Dominio: Un documento de tipo documento y una lista que contiene sublistas de permisos (cada sublista tiene un username y el permiso)
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y agregar permisos a este. Si el formato de la lista de permisos es incorrecto,
; retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setPermisosDocumento document listpermisos)
  (if (and(isDocumento? document)(list? listpermisos))
      (list (getAutorDocumento document) (getFechaDocumento document)(getNombreDocumento document)(getContenidoDocumento document)(append listpermisos (getPermisosDocumento document))(getHistorialDocumento document)(getIDDocumento document))
      document))
;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Dominio: Un documento de tipo documento y un ID de tipo integer
; Recorrido: Un booleano (o null en el caso de que document no corresponda a un documento valido)
; Descripcion: Funcion que verifica si un documento tiene un ID en especifico.
; Recursion: No se utiliza recursion
(define (verificarIDs document ID1)
  (if (and(isDocumento? document)(integer? ID1))
      (if(eq? (getIDDocumento document) ID1)
         #t
         #f)
      null))

;-----------------------------------EJEMPLOS DE PRUEBA---------------------------------------------------------------
;Crear un documento
(define Doc0001 (documento "John" (date 12 10 2021) "Mi primer documento" "Este es el contenido del documento para el laboratorio de Paradigmas de Programación" 0))
; Verificar si un documento es correcto
(define esDocu? (isDocumento? Doc0001))


; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
