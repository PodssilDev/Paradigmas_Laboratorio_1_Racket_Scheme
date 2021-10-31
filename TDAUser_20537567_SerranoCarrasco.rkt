#lang racket
; Se requiere el uso del TDA Fecha para elaborar la construcción del TDA User
(require "TDAFecha_20537567_SerranoCarrasco.rkt")

; NOTA: No fue necesario crear modificadores para este TDA.
;-----------------------------------TDA USER-----------------------------------------------------------------

;-----------------------------------REPRESENTACION-----------------------------------------------------------

; Este TDA representa a un usuario del documento, que contiene una lista donde se guarda la fecha de registro
; el nombre del usuario y su contraseña en ese mismo orden.
; (fecha X string X string)

;-----------------------------------CONSTRUCTORES------------------------------------------------------------

; Dominio: Una fecha de tipo de dato fecha y strings para username y password (fecha x string x string)
; Recorrido: Una lista con la fecha, username y password del usuario
; Descripcion: Toma la fecha de registro del usuario, su username y su password y los guarda en una lista
; Tipo de recursion: No se utiliza recursion
(define(user date username password)
  (list date username password)
  )

;-----------------------------------FUNCIONES DE PERTENENCIA-------------------------------------------------

; Dominio: Recibe un usuario de tipo de dato user
; Recorrido: Un booleano
; Descripcion: Comprueba si el formato del usuario es correcto (Si el username y password son strings)
; Tipo de recursion: No se utiliza recursion
(define (isUser? usuario)
  (if(and(string? (car(cdr usuario)))(string? (car(cdr(cdr usuario)))))
     #t
     #f)
  )

;-----------------------------------SELECTORES---------------------------------------------------------------

; Dominio: Un usuario de tipo de dato user
; Recorrido: Una fecha (lista)
; Descripcion: Obtiene la fecha de registro del usuario
; Tipo de recursion: No se utiliza recursion 
(define(getFechaUser usuario)
  (if(isUser? usuario)
     (car usuario)
     null)
  )

; Dominio: Un usuario de tipo de dato user
; Recorrido: Devuelve un nombre de usuario (string)
; Descripcion: Obtiene el username del usuario
; Tipo de recursion: No se utiliza recursion
(define (getUsernameUser usuario)
  (if(isUser? usuario)
     (car(cdr usuario))
         null)
  )

; Dominio: Un usuario de tipo de dato user
; Recorrido: Devuelve una contraseña de un usuario (string)
; Descripcion: Obtiene la contraseña de un usuario
; Tipo de recursion: No se utiliza recursion
(define(getPasswordUser usuario)
  (if(isUser? usuario)
     (car(cdr(cdr usuario)))
     null)
  )

; Dominio: Una lista que contiene a usuarios (user X user X user... X user)
; Recorrido: Un usuario de tipo user
; Descripcion: Funcion que obtiene al primer usuario de la lista de users
; Tipo de recursion: No se utiliza recursion
(define (getPrimeroListUser listUsuarios)
  (if (list? listUsuarios)
      (car listUsuarios)
      null))

; Dominio: Una lista que contiene a usuarios (user X user X user... X user)
; Recorrido: Una lista de usuarios de tipo user
; Descripcion: Funcion que obtiene a los siguientes elementos de la lista (ignorando el primero)
; Tipo de recursion: No se utiliza recursion
(define (getSiguientesListUser listUsuarios)
  (if(list? listUsuarios)
     (cdr listUsuarios)
     null))

;-----------------------------------OTRAS FUNCIONES-----------------------------------------------------------------

; Dominio: Una lista de usuarios, un usuario de tipo user y un booleano
; Recorrido: Una lista de usuarios
; Descripcion: Funcion que de manera recursiva y dependiendo si esta en la lista o no, agrega a un user a la lista
; Tipo de recursion: Recursion Natural
; Justificacion de Recursion: Permite recorrer toda la lista y no agregar dos users con el mismo username
(define (registerNatural lista usuario flag)
  (if(null? lista)
     (if(equal? flag #t)
        null
        (cons usuario null))
     (if(not(equal? (getUsernameUser usuario) (getUsernameUser (getPrimeroListUser lista))))
        (cons (getPrimeroListUser lista) (registerNatural (getSiguientesListUser lista) usuario flag))
        (cons (getPrimeroListUser lista) (registerNatural (getSiguientesListUser lista) usuario #t)))))

; Dominio: Una lista de usuarios de tipo list y un usuario de tipo user
; Recorrido: Booleano
; Descripcion: Verifica si un usuario ya esta registrado, revisando toda la lista de usuarios
; Tipo de recursion: Recursion de Cola
; Justificacion de Recursion: Sirve para verificar toda la lista de usuarios y comprobar que el user a registrar no este registrado.
(define(revisarUsuarioPdocs listUser nameuser)
  (if (null? listUser)
      #t
      (if(not(usersIguales?(getPrimeroListUser listUser) nameuser))
         (revisarUsuarioPdocs(getSiguientesListUser listUser) nameuser)
         #f)
      )
  )

; Dominio: Dos usuarios de tipo user
; Recorrido: Un booleano
; Descripcion: Compara si dos usuarios tienen el mismo nombre de usuario
; Tipo de recursion: No se utiliza recursion
(define (usersIguales? usuario1 usuario2)
  (if (and(isUser? usuario1)(isUser? usuario2))
      (if (eq? (getUsernameUser usuario1)(getUsernameUser usuario2))
          #t
          #f)
      null))

; Dominio: Dos usuarios de tipo user
; Recorrido: Un booleano
; Descripcion: Compara si dos usuarios tienen el mismo nombre de usuario y el mismo password
; Tipo de recursion: No se utiliza recursion
(define(verificarUsersUser usuario1 usuarioname2 usuariopassw2)
  (if (and(and(isUser? usuario1)(string? usuarioname2))(string? usuariopassw2))
      (if (and(eq? (getUsernameUser usuario1) usuarioname2)(equal?(getPasswordUser usuario1)usuariopassw2))
          #t
          #f)
      null))

; Dominio: Un usuario de tipo user y un nombre de usuario de tipo string
; Recorrido: Un booleano
; Descripcion: Compara si un usuario tiene el mismo nombre de usuario del string a comparar
; Tipo de recursion: No se utiliza recursion
(define (usernamesIguales? usuario1 usern2 autor)
  (if (and(and(isUser? usuario1)(string? usern2))(string? autor))
      (if (or(not(eq? (getUsernameUser usuario1)usern2))(eq? usern2 autor))
          #t
          #f)
      null))

; Dominio: Un usuario de tipo user
; Recorrido: Un string
; Descripcion: Funcion que transforma toda la informacion de un usuario (username, password y fecha de registro) en un string
; Tipo de recursion: No se utiliza recursion
(define (userToString usuario)
  (string-join (list "---------\n"  "Username:" (getUsernameUser usuario)"\n" "Password:" (getPasswordUser usuario) "\n" "Fecha de registro:" (date->string(getFechaUser usuario))"\n")))

;-----------------------------------EJEMPLOS DE PRUEBA--------------------------------------------------------------

(define user000 (user(date 07 10 2021) "MrDoopliss" "Test01"))
(define obtener_pass (getPasswordUser user000))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
