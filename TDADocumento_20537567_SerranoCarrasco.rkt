#lang racket
; Se necesita del TDA Fecha para la construccion del TDA Documento
(require "TDAFecha_20537567_SerranoCarrasco.rkt")

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
(list autor fecha nombre_documento contenido_documento (list)(list (list fecha contenido_documento 0)) ID))

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

; Dominio: Una lista de permiso
; Recorrido: Un nombre, de tipo string
; Descripcion: Funcion que obtiene el nombre asociado a una lista de permisos
; Tipo de recursion: No se utiliza recursion
(define(getNamePermiso listpermiso)
  (car listpermiso))

; Dominio: Una lista de permiso
; Recorrido: Un permiso, de tipo caracter
; Descripcion: Funcion que obtiene el permiso asociado a una lista de permisos
; Tipo de recursion: No se utiliza recursion
(define (getPermisoPermiso listpermiso)
  (car (cdr listpermiso)))

; Dominio: Un documento de tipo documento
; Recorrido: Una lista de historial del texto del documento de tipo list
; Descripcion: Funcion que obtiene una lista que contiene todas las versiones del contenido (texto) del documento
; Tipo de recursion: No se utiliza recursion
(define (getHistorialDocumento document)
  (if (isDocumento? document)
      (car (cdr (cdr (cdr (cdr (cdr document))))))
      null))

; Dominio: Una lista de una version de un historial
; Recorrido: Una fecha de tipo fecha 
; Descripcion: Funcion que obtiene la fecha de registro de una version de un historial correspondiente a un documento
; Tipo de recursion: No se utiliza recursion
(define (getFechaHistorial listhistorial)
  (car listhistorial))

; Dominio: Una lista de una version de un historial
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que obtiene el texto de una version de un historial correspondiente a un documento
; Tipo de recursion: No se utiliza recursion
(define (getTextoHistorial listhistorial)
  (car (cdr listhistorial)))

; Dominio: Una lista de una version de un historial
; Recorrido: Un ID de tipo integer
; Descripcion: Funcion que obtiene el ID de una version de un historial correspondiente a un documento
; Tipo de recursion: No se utiliza recursion
(define (getIDHistorial listhistorial)
  (car (cdr(cdr listhistorial))))

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

; Dominio: Un documento de tipo documento, un nuevo contenido (texto) de tipo string, una nueva fecha de tipo fecha
; y un valor booleano
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y cambia el contenido (texto) del documento. Si el formato del texto es incorrecto,
; retorna el documento sin modificaciones. Dependiendo de flag, si este es True, se concatena el texto nuevo por el texto actual. Si
; flag es False, se reemplaza el texto.
; Tipo de recursion: No se utiliza recursion
(define(setContenidoDocumento document newContenido fecha  flag)
  (if (and(and(and(isDocumento? document)(string? newContenido))(date? fecha))(boolean? flag))
      (if (eq? flag #t)
       (list (getAutorDocumento document) (getFechaDocumento document)(getNombreDocumento document)(string-join (list newContenido (getContenidoDocumento document)))(getPermisosDocumento document)(cons(list fecha (string-join (list newContenido (getContenidoDocumento document))) (obtenerIDHistorial document))(getHistorialDocumento document))(getIDDocumento document))
       (list (getAutorDocumento document) (getFechaDocumento document)(getNombreDocumento document)newContenido(getPermisosDocumento document)(cons(list fecha newContenido (obtenerIDHistorial document))(getHistorialDocumento document))(getIDDocumento document)))
      document))

; Dominio: Un documento de tipo documento y una lista que contiene sublistas de permisos (cada sublista tiene un username y el permiso)
; Recorrido: Un documento de tipo documento (list)
; Descripcion: Funcion que modifica el documento y agregar permisos a este. Si el formato de la lista de permisos es incorrecto,
; retorna el documento sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setPermisosDocumento document listpermisos)
  (if (and(isDocumento? document)(list? listpermisos))
      (list (getAutorDocumento document) (getFechaDocumento document)(getNombreDocumento document)(getContenidoDocumento document)(filtrarPermisosUnicos null (append listpermisos (getPermisosDocumento document)))(getHistorialDocumento document)(getIDDocumento document))
      document))

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Dominio: Un documento de tipo documento y un ID de tipo integer
; Recorrido: Un booleano (o null en el caso de que document no corresponda a un documento valido)
; Descripcion: Funcion que verifica si un documento tiene un ID en especifico.
; Tipo de Recursion: No se utiliza recursion
(define (verificarIDs document ID1)
  (if (and(isDocumento? document)(integer? ID1))
      (if(eq? (getIDDocumento document) ID1)
         #t
         #f)
      null))

; Dominio: Un documento de tipo dcumento
; Recorrido: Un ID de tipo integer
; Descripcion: Obtiene el ID de un texto para el historial de versiones
; Tipo de Recursion: No se utiliza recursion
(define (obtenerIDHistorial document)
  (if (isDocumento? document)
      (length (getHistorialDocumento document))
      null))

; Dominio: Un permiso de tipo caracter
; Recorrido: Un booleano
; Descripcion: Retorna True si el permiso es valido (Un permiso valido puede ser lectura, comentarios o escritura)
; Tipo de recursion: No se utiliza recursion
(define (verificarPermiso permiso)
  (if (or(or(eq? permiso #\w)(eq? permiso #\r))(eq? permiso #\c))
      #t
      #f)
  )

; Dominio: Una lista (inicialmente vacia) y un username de tipo string
; Recorrido: Un booleano
; Descripcion: Funcion que retorna True si el user no esta en la lista final de permisos
; Tipo de Recursion: Recursion Natural
; Justificacion de Recursion: Permite recorrer toda la lista final para poder verificar si el user ya ha sido agregado anteriormente con otro permiso
(define (noEstaPermiso listfinal userpermiso)
  (if (eq? listfinal null)
      #t
      (if (equal? (getNamePermiso (car listfinal))userpermiso)
          #f
          (noEstaPermiso (cdr listfinal) userpermiso))))

; Dominio: Una lista  (inicialmente vacia) y una lista de permisos
; Recorrido: Una lista de permisos filtrada
; Descripcion: Funcion que filtra la lista de permisos para que un User no aparezca mas de una vez en la lista de permisos
; Tipo de recursion: Recursion Natural
; Justificacion de recursion: Permite recorrer toda la lista de permisos y filtrarla correctamente
(define (filtrarPermisosUnicos listfinal listpermisos)
 (if (eq? listpermisos null)
     listfinal
     (if (noEstaPermiso listfinal (getNamePermiso(car listpermisos)))
         (filtrarPermisosUnicos (cons (car listpermisos) listfinal) (cdr listpermisos))
         (filtrarPermisosUnicos listfinal (cdr listpermisos)))))

; Dominio: Una lista de permisos y un username de tipo string
; Recorrido: null o una lista que contiene al user y un permiso
; Descripcion: Funcion que busca si un user tiene un permiso y retorna la sublista con su permiso
; Tipo de Recursion: Recursion Natural
; Justificacion de Recursion: Permite recorrer toda la lista final para poder verificar si el user tiene un permiso o no.
(define (TienePermiso? listpermisos userpermiso)
  (if (eq? listpermisos null)
      null
      (if (eq? (getNamePermiso (car listpermisos))userpermiso)
          (car listpermisos)
          (TienePermiso? (cdr listpermisos) userpermiso))))

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

; Dominio: Una lista de historial de versiones (Contiene sublistas con una fecha, texto y un ID de version) y un ID de version de tipo integer
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que permite obtener al texto correcto que se quiere restaurar. Si el texto no se logra encontrar, se retorna null
; Tipo de Recursion: Recursion Natural
; Justificacion de Recursion: Permite recorrer toda la lista del historial de versiones
(define(obtenerVersion listHistorial idHist)
  (if(eq? listHistorial null)
     null
     (if (eq? idHist (getIDHistorial(car listHistorial)))
         (getTextoHistorial(car listHistorial))
         (obtenerVersion (cdr listHistorial) idHist))))

; Dominio: Un documento de tipo document y un ID de tipo integer
; Recorrido: Un documento (lista)
; Descripcion: Funcion que permite colocar el texto desde el historial como texto activo
; Tipo de Recursion: No se utiliza recursion
(define (restaurarVer document id)
  (if (eq? null (obtenerVersion (getHistorialDocumento document) id))
      document
      (list (getAutorDocumento document)(getFechaDocumento document)(getNombreDocumento document) (obtenerVersion (getHistorialDocumento document) id) (getPermisosDocumento document) (getHistorialDocumento document)(getIDDocumento document))))

; Dominio: Un documento de tipo documento
; Recorrido: Un documento de tipo documento (lista)
; Descripcion: Funcion que elimina los permisos de un documento
; Tipo de Recursion: No se utiliza recursion
(define (eliminarPermisos document)
  (list (getAutorDocumento document) (getFechaDocumento document) (getNombreDocumento document)(getContenidoDocumento document) null (getHistorialDocumento document)(getIDDocumento document)))

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

; Dominio: Una lista (inicialmente vacia), una lissta de documentos y un texto de tipo string
; Recorrido: Una lista final donde cada sublista contiene al texto como su elemento final
; Descripcion: Funcion que agrega un texto a una parte final de una lista. Retorna una lista grande que contiene a todas las sublistas
; Tipo de Recursion: Recursion Natural
; Justificacion de recursion: Permite recorrer toda la lista de listdocs para agregar el texto a cada sublista
(define (addTextoSearch listfinal listdocs text)
  (if (null? listdocs)
      listfinal
      (addTextoSearch (cons (append(car listdocs) (list text)) listfinal) (cdr listdocs) text)))

; Dominio: Una lista de historial de un documento y un texto de tipo string
; Recorrido: Un booleano
; Descripcion: Funcion que recorre todo el historial de versiones de un documento para encontrar si un texto se encuentra en alguna version
; Tipo de Recursion: Recursion Natural
; Justificacion de recursion: Permite recorrer toda la lista del historial para encontrar si existe una version que tenga un texto en especifico
(define (encontrarTextoHistorial listhistorial texto )
  (if(null? listhistorial)
     #f
     (if(not( eq? (length(string-split (getTextoHistorial(car listhistorial)) texto)) (length(list (getTextoHistorial(car listhistorial))))))
        #t
        (if (not(eq? (length(string->list(car(string-split(getTextoHistorial(car listhistorial)) texto)))) (length(string->list(getTextoHistorial(car listhistorial))))))
            #t
            (encontrarTextoHistorial (cdr listhistorial) texto)))))

; Dominio: Un documento de tipo documento
; Recorrido: Un booleano
; Descripcion: Funcion que busca la presencia de un texto en especifico en su texto activo.
; Tipo de Recursion:  No se utiliza recursion
(define (encontrarTexto document)
  (if(not( eq? (length(string-split (getContenidoDocumento document)(last document))) (length(list (getContenidoDocumento document)))))
     #t
     (if (not(eq? (length(string->list(car(string-split (getContenidoDocumento document)(last document)))))(length(string->list(getContenidoDocumento document)))))
         #t
         (encontrarTextoHistorial (getHistorialDocumento document)(last document)))))

; Dominio: Un documento de tipo documento
; Recorrido: Un documento actualizado
; Descripcion: Funcion que elimina el "rastro" de la funcion search; elimina el texto a buscar que se encuentra en la ultima posicion del documento
; Tipo de recursion: No se utiliza recursion
(define (eliminarRastro documento)
  (remove (last documento) documento))

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

; Dominio: Una lista de historial
; Recorrido: Un string
; Descripcion: Transforma una lista de historial en un string
; Tipo de recursion: No se utiliza recursion
(define (historialToString listhistorial)
  (string-join (list "Version N°"(number->string (getIDHistorial listhistorial))":" "*"(getTextoHistorial listhistorial)"*" "version guardada el dia" (date->string (getFechaHistorial listhistorial))"\n")))

; Dominio: Un documento
; Recorrido: Un string
; Descripcion: Transforma todo un documento (es decir, autor, nombre, fecha de creacion, contenido, permisos, historial) en un string
; Tipo de recursion: No se utiliza su recursion
(define (documentoToString document)
  (if(null? (getPermisosDocumento document))
     (string-join (list "---------\n" "Autor:" (getAutorDocumento document)"\n" "Fecha de creacion:"(date->string(getFechaDocumento document))"\n" "Nombre de Documento:"(getNombreDocumento document) "\n" "Contenido Documento:"(getContenidoDocumento document)"\n" "Permisos: No se ha dado permisos a otros usuarios\n" "Historial de versiones:" "\n"(string-join(map historialToString (getHistorialDocumento document)))))
     (string-join (list "---------\n" "Autor:" (getAutorDocumento document)"\n" "Fecha de creacion:"(date->string(getFechaDocumento document))"\n" "Nombre de Documento:"(getNombreDocumento document) "\n" "Contenido Documento:"(getContenidoDocumento document)"\n" "Permisos:" "\n" (string-join(map permisoToString (getPermisosDocumento document)))"Historial de versiones:" "\n"(string-join(map historialToString (getHistorialDocumento document)))))))

; Dominio: Una lista (inicialmente vacia), una lista que contiene caracteres de un texto y un numero de caracteres totales
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que elimina una cantidad de caracteres, a traves de recorrer una lista y dejar solo los caracteres
; tal que el largo de la lista final equivalga al numero de caracteres totales. Si el numero de caracteres totales es mayor
; al largo de la lista de caracteres inicial, se retorna un string vacio.
; Tipo de recursion: Recursion Natural
; Justificacion de recursion: Permite recorrer toda la lista hasta que se cumpla la condicion
(define (deleteCharsDoc listfinal texto numbersFinal)
  (if ( > 0 numbersFinal)
      ""
      (if(eq? (length listfinal) numbersFinal)
          (list->string listfinal)
         (deleteCharsDoc (append listfinal (list (car texto) )) (cdr texto) numbersFinal))))

; Dominio: Un elemento de tipo caracter
; Recorrido: Una elemento de tipo caracter
; Descripcion: Funcion que toma un elemento de tipo caracter y lo cambia por otro
; Tipo de recursion: No se utiliza recursion
(define (mezclarLetras letra)
  (cond
    [(equal? letra #\A) #\Z]
    [(equal? letra #\a) #\z]
    [(equal? letra #\B) #\Y]
    [(equal? letra #\b) #\y]
    [(equal? letra #\C) #\X]
    [(equal? letra #\c) #\x]
    [(equal? letra #\D) #\W]
    [(equal? letra #\d) #\w]
    [(equal? letra #\E) #\V]
    [(equal? letra #\e) #\v]
    [(equal? letra #\F) #\U]
    [(equal? letra #\f) #\u]
    [(equal? letra #\G) #\T]
    [(equal? letra #\g) #\t]
    [(equal? letra #\H) #\S]
    [(equal? letra #\h) #\s]
    [(equal? letra #\I) #\R]
    [(equal? letra #\i) #\r]
    [(equal? letra #\J) #\Q]
    [(equal? letra #\j) #\q]
    [(equal? letra #\K) #\P]
    [(equal? letra #\k) #\p]
    [(equal? letra #\L) #\O]
    [(equal? letra #\l) #\o]
    [(equal? letra #\M) #\N]
    [(equal? letra #\m) #\n]
    [(equal? letra #\N) #\M]
    [(equal? letra #\n) #\m]
    [(equal? letra #\O) #\L]
    [(equal? letra #\o) #\l]
    [(equal? letra #\P) #\K]
    [(equal? letra #\p) #\k]
    [(equal? letra #\Q) #\J]
    [(equal? letra #\q) #\j]
    [(equal? letra #\R) #\I]
    [(equal? letra #\r) #\i]
    [(equal? letra #\S) #\H]
    [(equal? letra #\s) #\h]
    [(equal? letra #\T) #\G]
    [(equal? letra #\t) #\g]
    [(equal? letra #\U) #\F]
    [(equal? letra #\u) #\f]
    [(equal? letra #\V) #\E]
    [(equal? letra #\v) #\e]
    [(equal? letra #\W) #\D]
    [(equal? letra #\w) #\d]
    [(equal? letra #\X) #\C]
    [(equal? letra #\x) #\c]
    [(equal? letra #\Y) #\B]
    [(equal? letra #\y) #\b]
    [(equal? letra #\Z) #\A]
    [(equal? letra #\z) #\a]
    [(equal? letra #\0) #\9]
    [(equal? letra #\1) #\8]
    [(equal? letra #\2) #\7]
    [(equal? letra #\3) #\6]
    [(equal? letra #\4) #\5]
    [(equal? letra #\5) #\4]
    [(equal? letra #\6) #\3]
    [(equal? letra #\7) #\2]
    [(equal? letra #\8) #\1]
    [(equal? letra #\9) #\0]
    [(equal? letra #\-) #\_]
    [(equal? letra #\_) #\-]
    [(equal? letra #\/) #\.]
    [(equal? letra #\.) #\/]
    [(equal? letra #\¡) #\¿]
    [(equal? letra #\¿) #\¡]
    [(equal? letra #\?) #\!]
    [(equal? letra #\!) #\?]
    [else letra]))

;-----------------------------------EJEMPLOS DE PRUEBA---------------------------------------------------------------
;Crear un documento
(define Doc0001 (documento "John" (date 12 10 2021) "Mi primer documento" "Paradigmas de Programación" 0))
; Verificar si un documento es correcto
(define esDocu? (isDocumento? Doc0001))

;(define newcontent (setContenidoDocumento Doc0001 "Este es un texto" (date 14 10 2021)#t))

(define elimin (eliminarPermisos Doc0001))
; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
