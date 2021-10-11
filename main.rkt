#lang racket
; Se requiere el uso del TDA Fecha, TDA Paradigmadocs y TDA User para la construcción de las funciones a continuación

(require "TDAFecha.rkt")
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")

; FUNCION REGISTER
; Dominio: Un documento de tipo paradigmadocs, una fecha de tipo fecha, un username de tipo string y un password de tipo string
; (paradigmadogs X fecha X string X string)
; Recorrido: Un documento de tipo paradigmadocs actualizado
; Descripcion: Funcion que registra un usuario en paradigmadocs. Si se intenta registrar un usuario ya registrado, no se registra y
; se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion:  Recursion Natural (Llamado a la funcion recursiva RevisarUsuarioPdocs)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado.
(define(register paradigmadocs date username password)
  (if(revisarUsuarioPdocs (getUsersPdocs paradigmadocs)(user date username password)) ; Llamado a funcion recursiva de TDA Paradigmadocs
     (setUserPdocs paradigmadocs (user date username password ))
     paradigmadocs))

; FUNCION LOGIN

(define(login paradigmadocs username password)
  (if(revisarUserActivoPdocs (getUsersPdocs paradigmadocs) username password) ; Llamado a funcion recursiva de TDA Paradigmadocs
     (setUseractivosPdocs paradigmadocs username password)
     paradigmadocs))


; Ejemplos para la FUNCION REGISTER
(define gDocs0(register emptyGDocs (date 25 3 2020) "user" "pass")) ; Ejemplo base
(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user2" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3")) ;Un usuario ya esta registrado
(define gDocs2 (register emptyGDocs (date 25 10 2021) 1233434 2222)) ; Ejemplo erroneo
(define gDocs3(register(register emptyGDocs (date 09 10 2021) "user0" "pass0")(date 09 10 2021) "user1" "pass1")) ; Registro de dos usuarios diferentes

(define gDocs4 (login(login gDocs3  "user0" "pass0")"user1" "pass1"))