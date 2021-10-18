#lang racket
; Se requiere el uso del TDA Fecha para elaborar la construcción del TDA User
(require "TDAFecha_20537567_SerranoCarrasco.rkt")

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

; Dominio: Funcion que recibe un usuario de tipo de dato user
; Recorrido: Una fecha (lista)
; Descripcion: Obtiene la fecha de registro del usuario
; Tipo de recursion: No se utiliza recursion 
(define(getFechaUser usuario)
  (if(isUser? usuario)
     (car usuario)
     null)
  )

; Dominio: Funcion que recibe un usuario de tipo de dato user
; Recorrido: Devuelve un nombre de usuario (string)
; Descripcion: Obtiene el username del usuario
; Tipo de recursion: No se utiliza recursion
(define (getUsernameUser usuario)
  (if(isUser? usuario)
     (car(cdr usuario))
         null)
  )

; Dominio: Funcion que recibe un usuario de tipo de dato user
; Recorrido: Devuelve una contraseña de un usuario (string)
; Descripcion: Obtiene la contraseña de un usuario
; Tipo de recursion: No se utiliza recursion
(define(getPasswordUser usuario)
  (if(isUser? usuario)
     (car(cdr(cdr usuario)))
     null)
  )

;-----------------------------------MODIFICADORES-------------------------------------------------------------

; Dominio: Recibe un usuario de tipo user y una fecha de tipo fecha
; Recorrido: Un usuario de tipo user (lista)
; Descripcion: Modifica la fecha. Si la fecha es erronea, devuelve al user sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setFechaUser usuario newFecha)
  (if (and(and(isUser? usuario)(list? newFecha))(not(empty? newFecha)))
      (user newFecha  (getUsernameUser usuario) (getPasswordUser usuario))
      usuario))

; Dominio: Recibe un usuario de tipo user y un nombre de usuario de tipo string
; Recorrido: Un usuario de tipo user (lista)
; Descripcion: Modifica el username. Si el username no es de tipo string, devuelve al user sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setUsernameUser usuario newUser)
  (if (and (isUser? usuario)(string? newUser))
      (user (getFechaUser usuario) newUser (getPasswordUser usuario))
      usuario))

; Dominio: Recibe un usuario de tipo user y un password de usuario de tipo string
; Recorrido: Un usuario de tipo user (lista)
; Descripcion: Modifica el password del user. Si el password no es de tipo string, devuelve al user sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setPasswordUser usuario newPassword)
  (if (and (isUser? usuario)(string? newPassword))
      (user (getFechaUser usuario) (getUsernameUser usuario) newPassword)
      usuario))

;-----------------------------------OTRAS FUNCIONES-----------------------------------------------------------------

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
(define user2 (user (date 07 10 2021) "MrDoopliss" "Test01"))
(define obtener_pass (getPasswordUser user2))
(define cambiopass(setPasswordUser user2 "pikachu0709"))
(define compareusersnames (verificarUsersUser user000 "MrDoopliss" "Test01"))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
