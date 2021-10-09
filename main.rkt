#lang racket
; Se requiere el uso del TDA Fecha, TDA Paradigmadocs y TDA User para la construcción de las funciones a continuación

(require "TDAFecha.rkt")
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")

; FUNCION REGISTER
; Dominio: Un documento de tipo paradigmadocs, una fecha de tipo fecha, un username de tipo string y un password de tipo string
; (paradigmadogs X fecha X string X string)
; Recorrido: Un documento de tipo paradigmadocs
; Descripcion: Funcion que registra un usuario en paradigmadocs. Si se intenta registrar un usuario ya registrado, no se registra y
; se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion:  Recursion Natural
(define(register paradigmadocs date username password)
  (define usuaro (user date username password)) ; Mala practica?
  (define (yaRegistrado? entrada usuario )
    (if(eq? entrada null)
       #f
       (if(usersIguales?(car entrada)usuario)
          #t
          (yaRegistrado? (cdr entrada)usuario))
       ))
  (if(yaRegistrado? (getUsersPdocs paradigmadocs) usuaro)
     paradigmadocs
     (list (getNombrePdocs paradigmadocs) (getFechaPdocs paradigmadocs)(getEncryptPdocs paradigmadocs)(getDecryptPdocs paradigmadocs) (cons usuaro(getUsersPdocs paradigmadocs)))))

; FUNCION LOGIN


; Ejemplos para la FUNCION REGISTER
(define ejemplo (register emptyGDocs (date 25 3 2020) "user" "pass"))
(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1"  "pass2" ) (date 25 10 2021) "user1"  "pass2" ) (date 25 10 2021) "user3" "pass3"))
