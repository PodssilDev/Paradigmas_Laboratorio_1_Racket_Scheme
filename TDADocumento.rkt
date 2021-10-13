#lang racket
; Se necesita del TDA Fecha para la construccion del TDA Documento
(require "TDAFecha.rkt")

; TDA Documento
; REPRESENTACION
; Este TDA corresponde a un Documento, donde se guarda el autor del documento, la fecha de creacion del documento, el nombre del documento y
; el contenido del documento, todo en una lista con el mismo orden (string X fecha X string X string)

; CONSTRUCTORES

; Dominio: Un nombre de tipo string, una fecha de tipo fecha, un nombre de tipo string y un texto de tipo string
; Recorrido: Un documento de tipo documento (lista)
; Descripcion: Crea a un documento
; Tipo de recursion: No se utiliza recursion
(define(documento autor fecha nombre_documento contenido_documento)
(list autor fecha nombre_documento contenido_documento (list)(list)))

; FUNCIONES DE PERTENENCIA

; Dominio: Un documento de tipo documento
; Recorrido: Un booleano
; Descripcion: Verifica si el formato del documento es correcto (si el nombre, autor y contenido son strings y la fecha es de tipo fecha)
; Tipo de recursion: No se utiliza recursion
(define(isDocumento? document)
  (if (and(and(and(string? (car document))(date? (car(cdr document))))(string? (car(cdr(cdr document)))))(string? (car(cdr(cdr(cdr document))))))
      #t
      #f))

; SELECTORES

; Dominio: Un documento de tipo documento
; Recorrido: Un autor de tipo string
; Descripcion: Funcion que obtiene un autor de un documento
; Tipo de recursion: No se utiliza recursion
(define (getAutorDocumento document)
  (if (isDocumento? document)
      (car document)
      null))

; Dominio: Un documento de tipo documento
; Recorrido: Una fecha de tipo fecha
; Descripcion: Funcion que obtiene una fecha de creacion de un documento
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

; MODIFICADORES

; Dominio: Un documento de tipo documento y un nuevo autor de tipo string
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el nombre del autor. Si el formato del autor es incorrecto, retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setAutorDocumento document newAutor)
  (if (and(isDocumento? document)(string? newAutor))
      (list newAutor (getFechaDocumento document)(getNombreDocumento document)(getContenidoDocumento document)(getPermisosDocumento document)(getHistorialDocumento document))
      document))

; Dominio: Un documento de tipo documento y una nueva fecha de tipo fecha (list)
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia la fecha de creacion del documento. Si el formato de la fecha es incorrecto,
; retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setFechaDocumento document newFecha)
  (if (and(and(isDocumento? document)(list? newFecha))(not(empty? newFecha)))
      (list (getAutorDocumento document) newFecha (getNombreDocumento document)(getContenidoDocumento document)(getPermisosDocumento document)(getHistorialDocumento document))
      document))

; Dominio: Un documento de tipo documento y un nuevo nombre de tipo string
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el nombre del documento. Si el formato del nombre es incorrecto, retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setNombreDocumento document newNombre)
  (if (and(isDocumento? document)(string? newNombre))
      (list (getAutorDocumento document) (getFechaDocumento document)newNombre(getContenidoDocumento document)(getPermisosDocumento document)(getHistorialDocumento document))
      document))

; Dominio: Un documento de tipo documento y un nuevo contenido (texto) de tipo string
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el contenido (texto) del documento. Si el formato del texto es incorrecto,
; retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setContenidoDocumento document newContenido)
  (if (and(isDocumento? document)(string? newContenido))
      (list (getAutorDocumento document) (getFechaDocumento document)(getNombreDocumento document)newContenido(getPermisosDocumento document)(cons(getContenidoDocumento document)(getHistorialDocumento document)))
      document))

; EJEMPLOS

(define gDocs0001 ( documento "John" (date 12 10 2021) "Mi primer documento" "Este es el contenido del documento para el laboratorio de Paradigmas de Programación"))
(define esdocu (setContenidoDocumento gDocs0001 "Mi nuevo documento"))
(define esdocu2 (setContenidoDocumento esdocu "Otro contenido mas"))
(define elemen (first(getHistorialDocumento esdocu2)))

(provide (all-defined-out))