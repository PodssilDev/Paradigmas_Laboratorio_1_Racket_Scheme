#lang racket
; NOTA: No fue necesario crear Modificadores para este TDA.
;-----------------------------------TDA PERMISO----------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------

; Este TDA representa a un permiso, el cual consiste en una lista que contiene a un nombre de un user,
; el cual es a quien se le asigna el permiso, y el permiso en si, que consiste en un caracter con una
; letra en especifico. (string X character)

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------

; Dominio: Un nombre de tipo string y un permiso de tipo caracter
; Recorrido: Una lista
; Descripcion: Funcion que crea un permiso
; Tipo de recursion: No se utiliza recursion
(define (permiso nombre perm)
  (list nombre perm))

;-----------------------------------FUNCIONES DE PERTENENCIA----------------------------------------------------

; Dominio: Un permiso
; Recorrido: Un booleano
; Descripcion: Funcion que verifica si un permiso es de formato correcto
; Tipo de recursion: No se utiliza recursion
(define (isPermiso? Permiso)
  (if(list? Permiso)
     (if(and(string? (car Permiso)) (char-alphabetic? (car(cdr Permiso))))
        #t
        #f)
     #f))

;-----------------------------------SELECTORES-----------------------------------------------------------------

; Dominio: Una lista de permiso
; Recorrido: Un nombre, de tipo string
; Descripcion: Funcion que obtiene el nombre asociado a una lista de permisos
; Tipo de recursion: No se utiliza recursion
(define(getNamePermiso listpermiso)
  (if(isPermiso? listpermiso)
     (car listpermiso)
     null))

; Dominio: Una lista de permiso
; Recorrido: Un permiso, de tipo caracter
; Descripcion: Funcion que obtiene el permiso asociado a una lista de permisos
; Tipo de recursion: No se utiliza recursion
(define (getPermisoPermiso listpermiso)
  (if(isPermiso? listpermiso)
     (car (cdr listpermiso))
     null))

; Dominio: Una lista que contiene sublistas de permiso (permiso X permiso X permiso ... X Permiso)
; Recorrido: Una lista de permiso (string X caracter)
; Descripcion: Funcion que obtiene el primer permiso de una lista que contiene muchos permisos
; Tipo de recursion: No se utiliza recursion
(define (getPrimeroListPermiso listaDePermisos)
  (if(list? listaDePermisos)
     (car listaDePermisos)
     null))

; Dominio: Una lista que contiene sublistas de permiso (permiso X permiso X permiso ... X Permiso)
; Recorrido: Una lista de permiso (string X caracter)
; Descripcion: Funcion que obtiene los siguientes permisos de una lista que contiene muchos permisos
; (se ignora el primero)
; Tipo de recursion: No se utiliza recursion
(define (getSiguientesListPermiso listaDePermisos)
  (if(list? listaDePermisos)
     (cdr listaDePermisos)
     null))

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Dominio: Un permiso de tipo caracter
; Recorrido: Un booleano
; Descripcion: Retorna True si el permiso es valido (Un permiso valido puede ser lectura, comentarios o escritura)
; Tipo de recursion: No se utiliza recursion
(define (verificarPermiso permiso)
  (if(not(char-alphabetic? permiso))
     #f
     (if (or(or(eq? permiso #\w)(eq? permiso #\r))(eq? permiso #\c))
         #t
         #f)
     )
  )

; Dominio: Una lista (inicialmente vacia) y un username de tipo string
; Recorrido: Un booleano
; Descripcion: Funcion que retorna True si el user no esta en la lista final de permisos
; Tipo de Recursion: Recursion de Cola
; Justificacion de Recursion: Permite recorrer toda la lista final para poder verificar si el user ya ha sido agregado anteriormente con otro permiso
(define (noEstaPermiso listfinal userpermiso)
  (if (eq? listfinal null)
      #t
      (if (equal? (getNamePermiso (getPrimeroListPermiso listfinal))userpermiso)
          #f
          (noEstaPermiso (getSiguientesListPermiso listfinal) userpermiso))))

; Dominio: Una lista  (inicialmente vacia) y una lista de permisos
; Recorrido: Una lista de permisos filtrada
; Descripcion: Funcion que filtra la lista de permisos para que un User no aparezca mas de una vez en la lista de permisos
; Tipo de recursion: Recursion de Cola
; Justificacion de recursion: Permite recorrer toda la lista de permisos y filtrarla correctamente
(define (filtrarPermisosUnicos listfinal listpermisos)
 (if (eq? listpermisos null)
     listfinal
     (if (noEstaPermiso listfinal (getNamePermiso(getPrimeroListPermiso listpermisos)))
         (filtrarPermisosUnicos (cons (getPrimeroListPermiso listpermisos) listfinal) (getSiguientesListPermiso listpermisos))
         (filtrarPermisosUnicos listfinal (getSiguientesListPermiso listpermisos)))))

; Dominio: Una lista de permiso, que contiene un username y un permiso
; Recorrido: Un booleano
; Descripcion: Funcion que retorna True si el user en especifico tiene el permiso de escritura
; Tipo de Recursion: No se utiliza recursion
(define (puedeEscribir listperm)
  (if (eq? listperm null)
      #f
      (if(eq? (getPermisoPermiso listperm) #\w)
         #t
         #f)))

; Dominio: Una lista de permisos y un username de tipo string
; Recorrido: null o una lista que contiene al user y un permiso
; Descripcion: Funcion que busca si un user tiene un permiso y retorna la sublista con su permiso
; Tipo de Recursion: Recursion de Cola
; Justificacion de Recursion: Permite recorrer toda la lista final para poder verificar si el user tiene un permiso o no.
(define (TienePermiso? listpermisos userpermiso)
  (if (eq? listpermisos null)
      null
      (if (eq? (getNamePermiso (getPrimeroListPermiso listpermisos))userpermiso)
          (getPrimeroListPermiso listpermisos)
          (TienePermiso? (getSiguientesListPermiso listpermisos) userpermiso))))

; Dominio: Una lista de permiso
; Recorrido: Un booleano
; Descripcion: Funcion que retorna True si el user en especifico tiene permiso de escritura o comentarios
; Tipo de recursion: No se utiliza recursion
(define (puedeComentar listperm)
  (if (eq? listperm null)
      #f
      (if(or(eq? (getPermisoPermiso listperm) #\w)(eq? (getPermisoPermiso listperm)#\c))
         #t
         #f)))

; Dominio: Una lista de permisos
; Recorrido: Un booleano
; Descripcion: Funcion que retorna True si una lista de un user en especifico tiene el permiso de escribir o leer el documento
; Recursion: No se utiliza recursion 
(define (puedeEscribirLeer listperm)
  (if (eq? listperm null)
      #f
      (if(or(eq? (getPermisoPermiso listperm) #\w)(eq? (getPermisoPermiso listperm) #\r))
         #t
         #f)))

; Dominio: Una lista de permisos
; Recorrido: Un string
; Descripcion: Transforma una lista de permisos en un string
; Tipo de recursion: No se utiliza recursion
(define (permisoToString listperm)
  (if(eq? (getPermisoPermiso listperm) #\w)
     (string-join (list (getNamePermiso listperm)"tiene permiso de escritura""\n "))
     (if (eq? (getPermisoPermiso listperm) #\r)
         (string-join (list (getNamePermiso listperm)"tiene permiso de lectura""\n "))
         (string-join (list (getNamePermiso listperm)"tiene permiso de comentarios" "\n ")))))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))