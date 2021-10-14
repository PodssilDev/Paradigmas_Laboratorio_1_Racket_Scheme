#lang racket
; Se requiere el uso de los TDA Fecha, TDA User y TDA Documento para elaborar la construccion del TDA ParadigmaDocs
(require "TDAFecha.rkt")
(require "TDAUser.rkt")
(require "TDADocumento.rkt")

;-----------------------------------TDA PARADIGMADOCS-----------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------------

; Este TDA representa corresponde a una plataforma de documentos, que contiene en una lista el nombre de la plataforma
; una fecha valida de creacion de la plataforma, las funciones encryptFn y decryptFn, las cuales modifican un texto,
; una lista con los usuarios registrados, cada uno con su username, password y fecha de registro, una lista de users activos
; donde se guarda el username del user y una lista de documentos, donde se tiene el autor del documento, la fecha de creacion
; de este, el nombre del documento, el contenido (texto) del documento, los permisos adicionales y el historial de versiones
; del documento.

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------------

; Primero, se definen las funcioens encryptFn y decryptFn. Ambas funciones hacen encryptan y decryptan texto.

; Dominio: Un texto (string)
; Reocrrido: Un texto (string)
; Descripcion: Funcion que recibe un texto y lo modifica. En este caso, la funcion entrega el texto al revez
; Tipo de Recursion: No se utiliza recursion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

; Se crea el constructor de paradigmadocs

; Dominio: Strings para el caso de name, encryptFn y decryptFn y una fecha valida para el caso de fecha
; Recorrido: Una lista que contiene a name, fecha, el texto de encryptFn y el texto de decryptFn, una lista de usuarios
; registrados, una lista de usuarios activos y una lista de documentos creados, cada uno con su informacion importante.
; Descripcion: Corresponde al constructor de la plataforma paradigmadocs.
; Tipo de recursion: No se utiliza recursion
(define (paradigmadocs name fecha encryptFn decryptFunction)
  (list name fecha encryptFn decryptFunction (list) (list) (list))
  )

;-----------------------------------FUNCIONES DE PERTENENCIA----------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un booleano
; Descripcion: Comprueba si el formato de paradigmadocs es correcto (name corresponde a string y fecha esta correcta)
; Tipo de recursion: No se utiliza recursion
(define (isParadigmadocs? docs)
  (if(and(string? (car docs))(date? (car(cdr docs))))
  #t
  #f))

;-----------------------------------SELECTORES-----------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un nombre de tipo string
; Descripcion: Obtiene el nombre de la plataforma en formato string
; Tipo de recursion: No se utiliza recursion
(define (getNombrePdocs docs)
  (if (isParadigmadocs? docs)
      (car docs)
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Lista de Enteros de tipo list
; Descripcion: Obtiene la fecha de creacion de la plataforma, en formato lista
; Tipo de recursion: No se utiliza recursion
(define (getFechaPdocs docs)
  (if (isParadigmadocs? docs)
      (car (cdr docs))
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un texto (String) o function
; Descripcion: Obtiene el texto guardado en la posicion de encryptFn (o la funcion, en caso de que no haya texto)
; Tipo de recursion: No se utiliza recursion
(define (getEncryptPdocs docs)
  (if (isParadigmadocs? docs)
      (car (cdr (cdr docs)))
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un texto (String) o function
; Descripcion: Obtiene el texto guardado en la posicion de decryptFn (o la funcion, en caso de que no haya texto)
; Tipo de recursion: No se utiliza recursion
(define (getDecryptPdocs docs)
  (if (isParadigmadocs? docs)
      (car (cdr (cdr (cdr docs))))
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de usuarios registrados que contiene una fecha, un username y un password(date x string x string)
; Descripcion: Obtiene la lista de users registrados guardada en paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define(getUsersPdocs docs)
  (if (isParadigmadocs? docs)
      (car(cdr(cdr(cdr (cdr docs)))))
      null)
 )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de usuarios activos que contiene el nombre del user activo (user logueado correctamente)
; Descripcion: Obtiene la lista de usernames de los usuarios activos en Paradigmadocs (Users logueados mediante login)
; Tipo de recursion: No se utiliza recursion
(define(getUsersactivosPdocs docs)
  (if (isParadigmadocs? docs)
      (car(cdr(cdr(cdr (cdr (cdr docs))))))
      null)
 )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de documentos donde cada elemento tiene el autor de un documento, la fecha de creacion, el nombre y el contenido
; Descripcion: Obtiene la lista de documentos creados, donde cada documento tiene su informacion importante
; Tipo de recursion: No se utiliza recursion
(define(getDocumentosPdocs docs)
  (if (isParadigmadocs? docs)
      (car(cdr(cdr(cdr (cdr (cdr (cdr docs)))))))
      null)
  )

;-----------------------------------MODIFICADORES-----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs y un nombre de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se modifica el nombre de la plataforma. Si el nombre es invalido, se entrega la plataforma sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setNombrePdocs docs newNombre)
  (if (and (isParadigmadocs? docs)(string? newNombre))
      (list newNombre (getFechaPdocs docs) (getEncryptPdocs docs) (getDecryptPdocs docs)(getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Una plataforma de tipo paradigmadocs y una fecha de tipo fecha
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se modifica la fecha de creacion de la plataforma. Si la fecha es invalida, se entrega la plataforma sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setFechaPdocs docs newFecha)
  (if (and(and(isParadigmadocs? docs)(list? newFecha))(not(empty? newFecha)))
      (list (getNombrePdocs docs) newFecha (getEncryptPdocs docs) (getDecryptPdocs docs)(getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Una plataforma de tipo paradigmadocs y un texto de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se modifica el texto en la posicion de encryptFn. Si el texto es invalido, se entrega la plataforma sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setEncryptPdocs docs newEncrypt)
  (if (and (isParadigmadocs? docs)(string? newEncrypt))
      (list (getNombrePdocs docs) (getFechaPdocs docs) newEncrypt (getDecryptPdocs docs)(getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Una plataforma de tipo paradigmadocs y un texto de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se modifica el texto en la posicion de decryptFn. Si el texto es invalido, se entrega la plataforma sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setDecryptPdocs docs newDecrypt)
  (if (and (isParadigmadocs? docs)(string? newDecrypt))
      (list (getNombrePdocs docs) (getFechaPdocs docs) (getEncryptPdocs docs) newDecrypt (getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Una plataforma de tipo paradigmadocs y un usuario de tipo user
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se agrega un usuario registrado correctamente a paradigmadocs. Si se intenta registrar incorrectamente, se retorna Paradigmadocs
; sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setUserPdocs docs user1)
  (if(and(isParadigmadocs? docs)(isUser? user1))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(cons user1(getUsersPdocs docs))(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
     docs))

; Dominio: Una plataforma de tipo paradigmadocs y un username de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se agrega un usuario activo (logueado) a paradigmadocs. Si el usuario no se ha registrado o el formato es incorrecto, se retorna
; a paradigmadocs sin modificaciones.
; Tipo de recursion: No se utiliza recursion
(define(setUseractivosPdocs docs usern)
  (if(and(isParadigmadocs? docs)(string? usern))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(getUsersPdocs docs)(cons usern(getUsersactivosPdocs docs))(getDocumentosPdocs docs))
     docs))

; Dominio: Una plataforma de tipo paradigmadocs y un documento de tipo documento
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Desloguea a un user activo y agrega un documento a paradigmadocs. Si el formato del documento es incorrecto, se retorna a paradigmadocs
; sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setDocumentoPdocs docs document)
  (if(and (and(isParadigmadocs? docs)(isDocumento? document))(not (eq? (getUsersactivosPdocs docs) null)))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(getUsersPdocs docs)(remove(first(getUsersactivosPdocs docs))(getUsersactivosPdocs docs))(cons document(getDocumentosPdocs docs)))
     docs))

; Dominio: Una plataforma de tipo paradigmadocs, un documento de tipo documento, un ID de tipo integer y una lista de permisos de tipo list
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Desloguea a un user activo y agrega permisos a un documento de paradigmadocs. Si el formato del documento, del ID o de la lista de permisos
; no corresponde, entonces se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion: Recursion Natural (Funcion encontrarIDs)
; Justificacion de recursion: Sirve para encontrar al documento correcto de acuerdo a su ID
(define (setDocumentoPermisosPdocs docs document ID listpermisos)
  (if(and(and(isParadigmadocs? docs)(isDocumento? document))(integer? ID))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(getUsersPdocs docs)(remove(first(getUsersactivosPdocs docs))(getUsersactivosPdocs docs))(cons (setPermisosDocumento document listpermisos)(remove (encontrarIDs (getDocumentosPdocs docs)ID)(getDocumentosPdocs docs))))
     docs))

;-----------------------------------OTRAS FUNCIONES-----------------------------------------------------------------

; Dominio: Una lista de usuarios de tipo list y un usuario de tipo user
; Recorrido: Booleano
; Descripcion: Verifica si un usuario ya esta registrado, revisando toda la lista de usuarios
; Tipo de recursion: Recursion Natural
; Justificacion de Recursion: Sirve para verificar toda la lista de usuarios y comprobar que el user a registrar no este registrado.
(define(revisarUsuarioPdocs listUser nameuser)
  (if (null? listUser)
      #t
      (if(not(usersIguales?(car listUser) nameuser))
         (revisarUsuarioPdocs(cdr listUser) nameuser)
         #f)
      )
  )

; Dominio: Una lista de usuarios registrados de tipo list, un username de tipo string y un password de tipo string
; Recorrido: Booleano
; Descripcion: Verifica si un usuario esta registrado y si su password coincide, lo que le permite loguearse correctamente. 
; Tipo de recursion: Recursion Natural
; Justificacion de Recursion: Sirve para verificar toda la lista de usuarios registrados y comprobar si el usuario puede loguearse.
(define(revisarUserActivoPdocs listUseract nameuseract passwact)
  (if (null? listUseract)
      #f
      (if(verificarUsersUser(car listUseract) nameuseract passwact)
         #t
         (revisarUserActivoPdocs(cdr listUseract) nameuseract passwact)
      )
  ))

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un numero de ID (Integer)
; Dscripcion: Obtiene el ID correspondiente a un documento y se lo agrega a un documento
; Tipo de recursion: No se utiliza recursion
(define(definirID docs)
  (if(eq? (getDocumentosPdocs docs) null)
     0
     (length(getDocumentosPdocs docs))))

; Dominio: Listas de string
; Recorrido: Lista de listas (cada lista contiene a un permiso)
; Descripcion: Crea una lista de sublistas, donde cada sublista es un permiso
; Tipo de recursion: No se utiliza recursion
(define (access . accessess)
   accessess)

; Dominio: Una lista de documentos de tipo list y un ID de tipo integer
; Recorrido: Un documento de tipo document
; Descripcion: Encuentra a un documento guardado en Paradigmadocs de acuerdo a su ID. Si no lo encuentra, retorna null
; Tipo de recursion: Recursion Natural
; Justificacion de recursion: Sirve para poder recorrer toda la lista de documentos de paradigmadocs y asi poder encontrar al documento correcto.
(define (encontrarIDs listdocs ID1)
  (if (null? listdocs)
      null
      (if (verificarIDs (car listdocs) ID1)
          (car listdocs)
          (encontrarIDs (cdr listdocs) ID1))))

; Dominio: Una lista de usuarios registrados y un nombre de tipo string
; Recorrido: Un booleano
; Descripcion: Verifica si un nombre de usuario ya esta registrado correctamente
; Tipo de recursion: Recursion Natural
; Justificacion de Recursion: Permite recorrer toda la lista de usuarios registrados
(define(revisarUsernPdocs listUser nameuser)
  (if (null? listUser)
      #t
      (if(not(usernamesIguales?(car listUser) nameuser))
         (revisarUsernPdocs(cdr listUser) nameuser)
         #f)
      )
  )

; Dominio: Una lista de usuarios, una lista vacia y una lista de permisos
; Recorrido: Una lista de permisos actualizada
; Descripcion: Funcion que filtra la lista de permisos para dejar los permisos de solo aquellos usuarios que estan registrados en paradigmadocs
; Tipo de recursion: Recursion Natural (Funcion revisarUsernPdocs)
; Justificacion de recursion: Permite recorrer toda la lista de usuarios registrados
(define (filtrarPermisosPdocs listUser listfinal listpermisos)
  (if (null? listpermisos)
      listfinal
      (if (revisarUsernPdocs listUser (first(car listpermisos)))
          (filtrarPermisosPdocs listUser listfinal (cdr listpermisos))
          (filtrarPermisosPdocs listUser (cons (car listpermisos) listfinal) (cdr listpermisos)))))

;-----------------------------------EJEMPLOS DE PRUEBA-----------------------------------------------------------------

(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn decryptFn) )
(define pertenencia1 (isParadigmadocs? emptyGDocs))
(define nombre (getNombrePdocs emptyGDocs))
(define fecha (getFechaPdocs emptyGDocs))
(define mensaje (getDecryptPdocs emptyGDocs))
(define cambio (setNombrePdocs emptyGDocs 1233)) ; Ejemplo erroneo
(define fechacambio (setFechaPdocs emptyGDocs (date 'hi' 10 2021)))
(define string(setEncryptPdocs emptyGDocs "Un nuevo mensaje para el dia"))
(define string2(setDecryptPdocs emptyGDocs "Vamos que se puede"))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
