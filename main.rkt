#lang racket
; Se requiere el uso del TDA Fecha, TDA Paradigmadocs, TDA User y TDA Documento para la construcción de las funciones a continuación

(require "TDAFecha.rkt")
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")
(require "TDADocumento.rkt")

; FUNCION REGISTER
; Dominio: Una plataforma de tipo paradigmadocs, una fecha de tipo fecha, un username de tipo string y un password de tipo string
; (paradigmadogs X fecha X string X string)
; Recorrido: Una plataforma de tipo paradigmadocs actualizado
; Descripcion: Funcion que registra un usuario en paradigmadocs. Si se intenta registrar un usuario ya registrado, no se registra y
; se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion:  Recursion Natural (Llamado a la funcion recursiva revisarUsuarioPdocs)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado.
(define(register paradigmadocs date username password)
  (if(revisarUsuarioPdocs (getUsersPdocs paradigmadocs)(user date username password)) ; Llamado a funcion recursiva de TDA Paradigmadocs
     (setUserPdocs paradigmadocs (user date username password ))
     paradigmadocs)
  )

; FUNCION LOGIN
; Dominio: Una plataforma de tipo paradigmadocs, un username de tipo string, un password de tipo string y una funcion de tipo function
; Recorrido: Una plataforma de tipo paradigmadocs actualizado o una funcion de tipo function
; Descripcion: Funcion que loguea a un usuario que ya esta registrado y lo registra en paradigmadocs. Llama a una funcion
; Tipo de recursion: Recursion Natural (Llamado a la funcion recursiva revisarUserActivoPdocs)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado y si el password coincide correctamente
(define(login paradigmadocs username password operation)
  (if(revisarUserActivoPdocs (getUsersPdocs paradigmadocs) username password) ; Llamado a funcion recursiva de TDA Paradigmadocs
     (cond
       [(eq? operation create) (lambda(date nombre contenido)(create (setUseractivosPdocs paradigmadocs username) date nombre contenido))]
       )
     (cond
       [(eq? operation create) (lambda(date nombre contenido)(create paradigmadocs date nombre contenido))]
       )
     )
  )

; FUNCION CREATE
; Dominio: Una plataforma de tipo paradigmadocs, una fecha de tipo fecha, un nombre de tipo string y un texto de tipo string
; Retorno: Una plataforma de tipo paradigmadocs actualizada (O sin cambios si el usuario no esta logueado)
; Descripcion: Funcion que crea un documento. Un documento contiene el autor, fecha de creacion del documento, un nombre, un contenido (texto),
; permisos y un historial de versiones. Guarda el documento en Paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define(create paradigmadocs date nombre contenido)
  (if (null? (getUsersactivosPdocs paradigmadocs))
       paradigmadocs
      (setDocumentoPdocs paradigmadocs (documento (car (getUsersactivosPdocs paradigmadocs)) date nombre contenido))))


; Ejemplos para la FUNCION REGISTER
 ; Ejemplo base
(define gDocs1
(register(register (register emptyGDocs (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user3" "pass3")) ;Un usuario ya esta registrado
(define gDocs2 (register emptyGDocs (date 25 10 2021) 1233434 2222)) ; Ejemplo erroneo
(define gDocs3(register(register emptyGDocs (date 09 10 2021) "user0" "pass0")(date 09 10 2021) "user1" "pass1")) ; Registro de dos usuarios diferentes

;ejemplos para la FUNCION LOGIN

(define gDocs0(register emptyGDocs (date 25 3 2020) "user" "pass"))
(define gDocs5 (login gDocs0 "user" "pass" create))

(define gDocs4 (login gDocs0 "user21" "pass" create))

