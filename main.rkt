#lang racket
; Se requiere el uso del TDA Fecha, TDA Paradigmadocs, TDA User y TDA Documento para la construcción de las funciones a continuación

(require "TDAFecha.rkt")
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")
(require "TDADocumento.rkt")

;-----------------------------------FUNCION REGISTER-----------------------------------------------------------------

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

;-----------------------------------FUNCION LOGIN---------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un username de tipo string, un password de tipo string y una funcion de tipo function
; Recorrido: Una plataforma de tipo paradigmadocs actualizado o una funcion de tipo function
; Descripcion: Funcion que loguea a un usuario que ya esta registrado y lo registra en paradigmadocs. Llama a una funcion.
; Si el usuario no se puede loguear (password erroneo o no esta registrado) de igual forma llama a la funcion, pero en paradigmadocs
; no se registra al user como activo.
; Tipo de recursion: Recursion Natural (Llamado a la funcion recursiva revisarUserActivoPdocs)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado y si el password coincide correctamente
(define(login paradigmadocs username password operation)
  (if(revisarUserActivoPdocs (getUsersPdocs paradigmadocs) username password) ; Llamado a funcion recursiva de TDA Paradigmadocs
     (cond
       [(eq? operation create) (lambda(date nombre contenido)(operation (setUseractivosPdocs paradigmadocs username) date nombre contenido))]
       [(eq? operation share) (lambda(idDoc access . accesses)(operation (setUseractivosPdocs paradigmadocs username) idDoc access accesses))]
       [else (paradigmadocs)]
       )
     (cond
       [(eq? operation create) (lambda(date nombre contenido)(operation paradigmadocs date nombre contenido))]
       [(eq? operation share) (lambda(idDoc access . accesses)(operation paradigmadocs idDoc access accesses))]
       [else (paradigmadocs)]
       )
     )
  )

;-----------------------------------FUNCION CREATE-----------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, una fecha de tipo fecha, un nombre de tipo string y un texto de tipo string
; Retorno: Una plataforma de tipo paradigmadocs actualizada (O sin cambios si el usuario no esta logueado)
; Descripcion: Funcion que crea un documento. Un documento contiene el autor, fecha de creacion del documento, un nombre, un contenido (texto),
; una lista de permisos, un historial de versiones y un ID de documento. Guarda el documento en Paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define(create paradigmadocs date nombre contenido)
  (if (null? (getUsersactivosPdocs paradigmadocs))
       paradigmadocs
      (setDocumentoPdocs paradigmadocs (documento (car (getUsersactivosPdocs paradigmadocs)) date nombre contenido (definirID paradigmadocs)))))

;-----------------------------------FUNCION SHARE------------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer y una lista de accesos (funcion access)
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada en el caso de que se pueda dar permisos correctamente
; Descripcion: Funcion que permite dar permisos de escritura, lectura o comentarios de un documento a uno o mas usuarios que ya estan
; registrados y guarda los permisos dentro de paradigmadocs. Solo el propietario de un documento puede dar permisos. Si se intenta
; dar permisos sin estar logueado o si el user que esta intentando dar permisos no es el autor del documento, se retorna a paradigmadocs
; sin modificaciones
; Tipo de recursion: Recursion Natural (Funcion encontrarIDs)
; Justificacion de Recursion: Permite encontrar al documento correcto a traves de su ID
(define(share paradigmadocs idDoc access . accesses)
  (if (or(null? (getUsersactivosPdocs paradigmadocs))(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
      paradigmadocs
      (if(eq? (car(getUsersactivosPdocs paradigmadocs))(getAutorDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc)))
         (setDocumentoPermisosPdocs paradigmadocs (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc) idDoc (filtrarPermisosPdocs (getUsersPdocs paradigmadocs) null (cons  access (car accesses))))
         paradigmadocs)))

;-----------------------------------FUNCION ADD---------------------------------------------------------------------------

;-----------------------------------EJEMPLOS PARA LAS FUNCIONES-----------------------------------------------------------

;-----------------------------------EJEMPLOS PARA LA FUNCION REGISTER-----------------------------------------------------

; Nota: emptyGDocs es una plataforma de tipo paradigmadocs. Fue creado dentro de TDA Paradigmadocs.

; Ejemplo del enunciado del laboratorio
(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))

; Ejemplo erroneo: Intentando registrar a un user cuyo username y password es de formato incorrecto
(define gDocs1000(register emptyGDocs (date 25 10 2021) 1233434 2222))

; Registro de dos usuarios diferentes
(define gDocs1001 (register(register emptyGDocs (date 09 10 2021) "user0" "pass0")(date 09 10 2021) "user1" "pass1"))

; Registro de tres usuarios, donde se intenta registrar a un user que ya fue registrado antes
(define gDocs1002
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user1" "pass2") (date 25 10 2021) "user3" "pass3"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y CREATE-----------------------------------------------------

; Se loguea a un user y se crea un documento correctamente
(define gDocs2 ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc0" "contenido doc0"))

; Ejemplo erroneo: Se intenta crear un documento pero el login es incorrecto
(define gDocs2000 ((login gDocs1 "user1" "pass2" create) (date 30 08 2021) "doc1" "contenido doc1"))

; Ejemplo erroneo: Se intenta crear otro documento pero se intenta loguear a un usuario que no esta registrado
(define gDocs3 ((login gDocs2 "user5" "pass5" create) (date 30 08 2021) "doc2" "contenido doc2"))

; Ejempplo erroneo: Se loguea correctamente a un user pero se intenta crear otro documento con formato incorrecto (integers en ves de strings)
(define gDocs3000 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) 22222 12334343))

; Se loguea al user2 correctamente y se crea un segundo documento
(define gDocs4 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc1" "contenido doc1"))

; Se loguea al user1 correctamente y user1 crea un tercer documento
(define gDocs4000 ((login gDocs4 "user1" "pass1" create) (date 30 08 2021) "doc2" "contenido doc2"))
;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y SHARE-----------------------------------------------------

; User 1 se loguea correctamente y le da acceso de lectura al user 2 para el documento 0
(define gDocs5 ((login gDocs4000 "user1" "pass1" share) 0 (access "user2" #\r)))

; User 2 se loguea correctamente y le da acceso de lectura a user 1 y a acceso de escritura a user 3 para el documento 1
(define gDocs6 ((login gDocs5 "user2" "pass2" share) 1 (access "user1" #\r) (access "user3" #\w)))

; Ejemplo erroneo:  User 3 se loguea correctamente pero intenta darse permisos en un documento que no es de su propiedad
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 0 (access "user3" #\c)))

; Ejemplo erroneo: User 1 se loguea correctamente, pero intenta dar permisos a un User que no esta registrado, aunque se le dan
; permisos correctamente a User 3
(define gDocs7000 ((login gDocs6 "user1" "pass1" share) 2 (access "user4" #\r)(access "user3" #\c)))

; Ejemplo erroneo: Se intenta usar share sin loguear y se intenta dar acceso a un documento que no existe
(define gDocs7001 (share gDocs7000 3 (access "user2" #\c)))

; Ejemplo erroneo: User 1 se loguea correctamente pero intenta dar permisos a un documento que no existe
(define gDocs7002 ((login gDocs7000 "user1" "pass1" share) 3 (access "user2" #\r)(access "user3" #\c)))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y ADD--------------------------------------------------------
