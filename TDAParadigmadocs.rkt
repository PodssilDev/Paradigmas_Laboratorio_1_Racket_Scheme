#lang racket
; Se requiere el uso de los TDA Fecha, TDA User y TDA Documento para elaborar la construccion del TDA ParadigmaDocs
(require "TDAFecha.rkt")
(require "TDAUser.rkt")
(require "TDADocumento.rkt")

; TDA ParadigmaDocs
; REPRESENTACION
; Este TDA representa corresponde a una plataforma de documentos, que contiene en una lista el nombre de la plataforma
; una fecha valida de creacion de la plataforma, las funciones encryptFn y decryptFn, las cuales modifican un texto,
; una lista con los usuarios registrados, cada uno con su username, password y fecha de registro, una lista de users activos
; donde se guarda el username del user y una lista de documentos, donde se tiene el autor del documento, la fecha de creacion
; de este, el nombre del documento, el contenido (texto) del documento, los permisos adicionales y el historial de versiones
; del documento.

; CONSTRUCTORES

; Primero, se definen las funcioens encryptFn y decryptFn. Ambas funciones hacen exactamente lo mismo.
; Dominio: Un texto (string)
; Reocrrido: Un texto (string)
; Descripcion: Funcion que recibe un texto y lo modifica. En este caso, la funcion entrega el texto al revez
; Tipo de Recursion: No se utiliza recursion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

; Se crea el constructor de paradigmadocs
; Dominio: Strings para el caso de name, encryptFn y decryptFn y Enteros para el caso de date
; Recorrido: Una lista que contiene a name, date, el texto de encryptFn y el texto de decryptFn, una lista de usuarios
; registrados, una lista de usuarios activos y una lista de documentos creados, cada uno con su informacion importante.
; Descripcion: Corresponde al constructor de paradigmadocs.
; Tipo de recursion: No se utiliza recursion
(define (paradigmadocs name fecha encryptFn decryptFunction)
  (list name fecha encryptFn decryptFunction (list) (list) (list)))

; FUNCIONES DE PERTENENCIA

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Un booleano
; Descripcion: Comprueba si el formato del TDA es correcto (name corresponde a string y fecha esta correcta)
; Tipo de recursion: No se utiliza recursion
(define (isParadigmadocs? docs)
  (if(and(string? (car docs))(date? (car(cdr docs))))
  #t
  #f
  )
  )

; SELECTORES

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: String
; Descripcion: Obtiene el nombre del documento en formato string
; Tipo de recursion: No se utiliza recursion
(define (getNombrePdocs docs)
  (if (isParadigmadocs? docs)
      (car docs)
      null)
  )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Lista de Enteros
; Descripcion: Obtiene la fecha de creacion del documento, en formato lista
; Tipo de recursion: No se utiliza recursion
(define (getFechaPdocs docs)
  (if (isParadigmadocs? docs)
      (car (cdr docs))
      null)
  )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Un texto (String)
; Descripcion: Obtiene el texto guardado en la posicion de encryptFn
; Tipo de recursion: No se utiliza recursion
(define (getEncryptPdocs docs)
  (if (isParadigmadocs? docs)
      (car (cdr (cdr docs)))
      null)
  )


; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Un texto (String)
; Descripcion: Obtiene el texto guardado en la posicion de decryptFn
; Tipo de recursion: No se utiliza recursion
(define (getDecryptPdocs docs)
  (if (isParadigmadocs? docs)
      (car (cdr (cdr (cdr docs))))
      null)
  )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Una lista de usuarios que contiene una fecha, un username y un password(date x string x string)
; Descripcion: Obtiene la lista de usernames guardada en Paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define(getUsersPdocs docs)
  (if (isParadigmadocs? docs)
      (car(cdr(cdr(cdr (cdr docs)))))
      null)
 )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Una lista de usuarios que contiene el nombre del user activo
; Descripcion: Obtiene la lista de usernames de los usuarios activos en Paradigmadocs (Users logueados mediante login)
; Tipo de recursion: No se utiliza recursion
(define(getUsersactivosPdocs docs)
  (if (isParadigmadocs? docs)
      (car(cdr(cdr(cdr (cdr (cdr docs))))))
      null)
 )

; Dominio: Un documento de tipo paradigmadocs
; Recorrido: Una lista de documentos donde cada elemento tiene el autor de un documento, la fecha de creacion, el nombre y el contenido
; Descripcion: Obtiene la lista de documentos creados
; Tipo de recursion: No se utiliza recursion
(define(getDocumentosPdocs docs)
  (if (isParadigmadocs? docs)
      (car(cdr(cdr(cdr (cdr (cdr (cdr docs)))))))
      null)
  )
; MODIFICADORES

; Dominio: Un documento de tipo paradigmadocs y un string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica el nombre del documento. Si el nombre es invalido, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setNombrePdocs docs newNombre)
  (if (and (isParadigmadocs? docs)(string? newNombre))
      (list newNombre (getFechaPdocs docs) (getEncryptPdocs docs) (getDecryptPdocs docs)(getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Un doumento de tipo paradigmadocs y una lista de enteros
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica la fecha del documento. Si la fecha es invalida, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setFechaPdocs docs newFecha)
  (if (and(and(isParadigmadocs? docs)(list? newFecha))(not(empty? newFecha)))
      (list (getNombrePdocs docs) newFecha (getEncryptPdocs docs) (getDecryptPdocs docs)(getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Un documento de tipo paradigmadocs y un string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica el texto en la posicion de encryptFn. Si el texto es invalido, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setEncryptPdocs docs newEncrypt)
  (if (and (isParadigmadocs? docs)(string? newEncrypt))
      (list (getNombrePdocs docs) (getFechaPdocs docs) newEncrypt (getDecryptPdocs docs)(getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Un documento de tipo paradigmadocs y un string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se modifica el texto en la posicion de decryptFn. Si el texto es invalido, se entrega la lista sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define (setDecryptPdocs docs newDecrypt)
  (if (and (isParadigmadocs? docs)(string? newDecrypt))
      (list (getNombrePdocs docs) (getFechaPdocs docs) (getEncryptPdocs docs) newDecrypt (getUsersPdocs docs )(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
      docs))

; Dominio: Un documento de tipo paradigmadocs y un usuario de tipo user
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se agrega un usuario registrado correctamente a paradigmadocs. Si se intenta registrar incorrectamente, se retorna Paradigmadocs
; sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setUserPdocs docs user1)
  (if(and(isParadigmadocs? docs)(isUser? user1))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(cons user1(getUsersPdocs docs))(getUsersactivosPdocs docs)(getDocumentosPdocs docs))
     docs))

; Dominio: Un documento de tipo paradigmadocs y un username de tipo string
; Recorrido: Una lista (de tipo paradigmadocs)
; Descripcion: Se agrega un usuario activo (logueado) a paradigmadocs. Si el usuario no se ha registrado o el formato es incorrecto, se retorna
; a paradigmadocs sin modificaciones.
; Tipo de recursion: No se utiliza recursion
(define(setUseractivosPdocs docs usern)
  (if(and(isParadigmadocs? docs)(string? usern))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(getUsersPdocs docs)(cons usern(getUsersactivosPdocs docs))(getDocumentosPdocs docs))
     docs))

(define (setDocumentoPdocs docs document)
  (if(and (and(isParadigmadocs? docs)(isDocumento? document))(not (eq? (getUsersactivosPdocs docs) null)))
     (list (getNombrePdocs docs)(getFechaPdocs docs)(getEncryptPdocs docs)(getDecryptPdocs docs)(getUsersPdocs docs)(remove(first(getUsersactivosPdocs docs))(getUsersactivosPdocs docs))(cons document(getDocumentosPdocs docs)))
     docs))
; OTRAS FUNCIONES

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
; Descripcion: Verifica si un usuario esta registrado y por lo tanto, puede loguearse. 
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


; EJEMPLOS

(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn decryptFn) )
(define pertenencia1 (isParadigmadocs? emptyGDocs))
(define nombre (getNombrePdocs emptyGDocs))
(define fecha (getFechaPdocs emptyGDocs))
(define mensaje (getDecryptPdocs emptyGDocs))
(define cambio (setNombrePdocs emptyGDocs 1233)) ; Ejemplo erroneo
(define fechacambio (setFechaPdocs emptyGDocs (date 'hi' 10 2021)))
(define string(setEncryptPdocs emptyGDocs "Un nuevo mensaje para el dia"))
(define string2(setDecryptPdocs emptyGDocs "Vamos que se puede"))

(define getnamedocs (getUsersactivosPdocs emptyGDocs))

(provide (all-defined-out))