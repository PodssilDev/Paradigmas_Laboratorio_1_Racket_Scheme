#lang racket
; Se requiere el uso de los TDA Fecha, TDA Access TDA User, TDA Historial, TDA Permiso y TDA Documento para elaborar la construccion del TDA ParadigmaDocs
(require "TDAFecha_20537567_SerranoCarrasco.rkt")
(require "TDAUser_20537567_SerranoCarrasco.rkt")
(require "TDAHistorial_20537567_SerranoCarrasco.rkt")
(require "TDAPermiso_20537567_SerranoCarrasco.rkt")
(require "TDADocumento_20537567_SerranoCarrasco.rkt")
(require "TDAAccess_20537567_SerranoCarrasco.rkt")

;-----------------------------------TDA PARADIGMADOCS-----------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------------

; Este TDA representa corresponde a una plataforma de documentos, que contiene en una lista el nombre de la plataforma
; una fecha valida de creacion de la plataforma, las funciones encryptFn y decryptFn, las cuales modifican un texto,
; una lista con los usuarios registrados, cada uno con su username, password y fecha de registro, una lista de users activos
; donde se guarda el username del user activo y una lista de documentos, donde se tiene el autor del documento, la fecha de creacion
; de este, el nombre del documento, el contenido (texto) del documento, los permisos adicionales y el historial de versiones
; del documento.
; (string X date X encryptFunction X decryptFunction X list X list X list)

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------------

; Primero, se definen las funcioens encryptFunction y decryptFunction. Ambas funciones hacen encryptan y decryptan texto.

; Dominio: Un texto (string)
; Reocrrido: Un texto (string)
; Descripcion: Funcion que recibe un texto y lo modifica. En este caso, la funcion entrega el texto al revez
; Tipo de Recursion: No se utiliza recursion
(define encryptFunction (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFunction (lambda (s) (list->string (reverse (string->list s)))))

; Se crea el constructor de paradigmadocs

; Dominio: Strings para el caso de name, encryptFn y decryptFn y una fecha valida para el caso de fecha
; Recorrido: Una lista que contiene a name, fecha, el texto de encryptFn y el texto de decryptFn, una lista de usuarios
; registrados, una lista de usuarios activos y una lista de documentos creados, cada uno con su informacion importante.
; Descripcion: Corresponde al constructor de la plataforma paradigmadocs.
; Tipo de recursion: No se utiliza recursion
(define (paradigmadocs name date encryptFunction decryptFunction)
  (list name date encryptFunction decryptFunction (list) (list) (list))
  )

;-----------------------------------FUNCIONES DE PERTENENCIA----------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un booleano
; Descripcion: Comprueba si el formato de paradigmadocs es correcto (name corresponde a string, fecha esta correcta, y estan Encrypt y Decrypt functions)
; Tipo de recursion: No se utiliza recursion
(define (isParadigmadocs? docs)
  (if(list? docs)
     (if(and(and(and(string? (car docs))(date? (car(cdr docs))))(procedure? (car(cdr(cdr docs)))))(procedure? (car(cdr(cdr(cdr docs))))))
        #t
        #f)
     #f))

;-----------------------------------SELECTORES-----------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un nombre de tipo string
; Descripcion: Obtiene el nombre de la plataforma en formato string
; Tipo de recursion: No se utiliza recursion
(define (getNombrePdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car pdocs)
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Lista de Enteros de tipo list
; Descripcion: Obtiene la fecha de creacion de la plataforma, en formato lista
; Tipo de recursion: No se utiliza recursion
(define (getFechaPdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car (cdr pdocs))
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una function
; Descripcion: Obtiene la funcion EncryptFunction guardada en Paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define (getEncryptPdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car (cdr (cdr pdocs)))
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una function
; Descripcion: Obtiene la funcion DecryptFunction guardada en Paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define (getDecryptPdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car (cdr (cdr (cdr pdocs))))
      null)
  )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de usuarios registrados que contiene una fecha, un username y un password(date x string x string)
; Descripcion: Obtiene la lista de users registrados guardada en paradigmadocs
; Tipo de recursion: No se utiliza recursion
(define(getUsersPdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car(cdr(cdr(cdr (cdr pdocs)))))
      null)
 )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de usuarios activos que contiene el nombre del user activo (user logueado correctamente)
; Descripcion: Obtiene la lista de usernames de los usuarios activos en Paradigmadocs (Users logueados mediante login)
; Tipo de recursion: No se utiliza recursion
(define(getUsersactivosPdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car(cdr(cdr(cdr (cdr (cdr pdocs))))))
      null)
 )

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de usuarios activos que contine al user activo
; Descripcion: Obtiene al usuario activo de la lista de usuarios activos
; Tipo de recursion: No se utiliza recursion
(define (obtenerActivoPdocs pdocs)
  (if(isParadigmadocs? pdocs)
     (if(null? (getUsersactivosPdocs pdocs))
        null
        (car(getUsersactivosPdocs pdocs)))
     null)
  )
; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una lista de documentos donde cada elemento tiene el autor de un documento, la fecha de creacion, el nombre y el contenido
; Descripcion: Obtiene la lista de documentos creados, donde cada documento tiene su informacion importante
; Tipo de recursion: No se utiliza recursion
(define(getDocumentosPdocs pdocs)
  (if (isParadigmadocs? pdocs)
      (car(cdr(cdr(cdr (cdr (cdr (cdr pdocs)))))))
      null)
  )

;-----------------------------------MODIFICADORES-----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs y un usuario de tipo user
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se agrega un usuario registrado correctamente a paradigmadocs. Si se intenta registrar incorrectamente, se retorna Paradigmadocs
; sin modificaciones
; Tipo de recursion: No se utiliza recursion
(define(setUserPdocs pdocs lista1)
  (if(and(isParadigmadocs? pdocs)(list? lista1))
     (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs) lista1 (getUsersactivosPdocs pdocs)(getDocumentosPdocs pdocs))
     pdocs))

; Dominio: Una plataforma de tipo paradigmadocs y un username de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Se agrega un usuario activo (logueado) a paradigmadocs. Si el usuario no se ha registrado o el formato es incorrecto, se retorna
; a paradigmadocs sin modificaciones.
; Tipo de recursion: No se utiliza recursion
(define(setUseractivosPdocs pdocs usern)
  (if(and(isParadigmadocs? pdocs)(string? usern))
     (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(cons usern(getUsersactivosPdocs pdocs))(getDocumentosPdocs pdocs))
     pdocs))

; Dominio: Una plataforma de tipo paradigmadocs y un documento de tipo documento
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Desloguea a un user activo y agrega un documento a paradigmadocs. Si el formato del documento es incorrecto, se retorna a paradigmadocs
; sin modificaciones. También verifica si un documento con el mismo ID ya habia sido agregado (para el caso de querer agregarlo actualizado)
; Tipo de recursion: No se utiliza recursion
(define (setDocumentoPdocs pdocs document)
  (if(and (and(isParadigmadocs? pdocs)(isDocumento? document))(not (eq? (getUsersactivosPdocs pdocs) null)))
     (if(eq? null (encontrarIDs (getDocumentosPdocs pdocs) (getIDDocumento document)))
        (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(remove(obtenerActivoPdocs pdocs) (getUsersactivosPdocs pdocs))(cons document(getDocumentosPdocs pdocs)))
        (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(remove(obtenerActivoPdocs pdocs) (getUsersactivosPdocs pdocs))
              (cons document(remove (encontrarIDs (getDocumentosPdocs pdocs) (getIDDocumento document)) (getDocumentosPdocs pdocs)))))
     pdocs))

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Desloguea a un user activo 
; Tipo de recursion: No se utiliza recursion
(define (setRemoverActivoPdocs pdocs)
  (if(isParadigmadocs? pdocs)
     (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(remove(obtenerActivoPdocs pdocs) (getUsersactivosPdocs pdocs))(getDocumentosPdocs pdocs))
     pdocs))

; Dominio: Una plataforma de tipo paradigmadocs, un documento de tipo documento, un ID de tipo integer y una lista de permisos de tipo list
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Desloguea a un user activo y agrega permisos a un documento de paradigmadocs. Si el formato del documento, del ID o de la lista de permisos
; no corresponde, entonces se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion: Recursion de Cola (Funcion encontrarIDs)
; Justificacion de recursion: Sirve para encontrar al documento correcto de acuerdo a su ID
(define (setDocumentoPermisosPdocs pdocs document ID listpermisos)
  (if(and(and(isParadigmadocs? pdocs)(isDocumento? document))(integer? ID))
     (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(remove(obtenerActivoPdocs pdocs) (getUsersactivosPdocs pdocs)) (cons (setPermisosDocumento document listpermisos)(remove (encontrarIDs (getDocumentosPdocs pdocs)ID)(getDocumentosPdocs pdocs))))
     pdocs))

; Dominio: Una plataforma de tipo paradigmadocs y un documento de tipo documento
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Funcion alternativa de setDocumento, solo que esta no quita al usuario de la seccion activa
; Tipo de recursion: No se utiliza recursion
(define (setDocumentoAlternPdocs pdocs document)
  (if(and (and(isParadigmadocs? pdocs)(isDocumento? document))(not (eq? (getUsersactivosPdocs pdocs) null)))
     (if(eq? null (encontrarIDs (getDocumentosPdocs pdocs) (getIDDocumento document)))
        (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(getUsersactivosPdocs pdocs) (cons document(getDocumentosPdocs pdocs)))
        (list (getNombrePdocs pdocs)(getFechaPdocs pdocs)(getEncryptPdocs pdocs)(getDecryptPdocs pdocs)(getUsersPdocs pdocs)(getUsersactivosPdocs pdocs) (cons document(remove (encontrarIDs (getDocumentosPdocs pdocs) (getIDDocumento document)) (getDocumentosPdocs pdocs)))))
     pdocs))

; Dominio: Una plataforma de tipo paradigmadocs y una lista de documentos de tipo list
; Recorrido: Una plataforma de tipo paradigmadocs actualizada
; Descripcion: Funcion que permite agregar toda la lista de documentos a paradigmadocs
; Tipo de recursion: Recursion de Cola
; Justificacion de recursion: Permite que todos los documentos sean agregados
(define (setListaDocumentosPdocs pdocs listdocumentos)
  (if (eq? null listdocumentos)
      pdocs
      (if (eq? (length listdocumentos) 1)
          (setListaDocumentosPdocs (setDocumentoPdocs pdocs (getPrimeroListDocumento listdocumentos)) (getSiguientesListDocumento listdocumentos))
          (setListaDocumentosPdocs (setDocumentoAlternPdocs pdocs (getPrimeroListDocumento listdocumentos)) (getSiguientesListDocumento listdocumentos)))))

;-----------------------------------OTRAS FUNCIONES-----------------------------------------------------------------

; Dominio: Una lista de usuarios registrados de tipo list, un username de tipo string y un password de tipo string
; Recorrido: Booleano
; Descripcion: Verifica si un usuario esta registrado y si su password coincide, lo que le permite loguearse correctamente. 
; Tipo de recursion: Recursion de Cola
; Justificacion de Recursion: Sirve para verificar toda la lista de usuarios registrados y comprobar si el usuario puede loguearse.
(define(revisarUserActivoPdocs pdocs listUseract nameuseract passwact)
  (if (or(or(null? listUseract)(not(string? nameuseract)))(not(string? passwact)))
      #f
      (if(verificarUsersUser(getPrimeroListUser listUseract) nameuseract ((getEncryptPdocs pdocs) passwact))
         #t
         (revisarUserActivoPdocs pdocs (getSiguientesListUser listUseract) nameuseract passwact)
      )
  ))

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un numero de ID (Integer)
; Dscripcion: Obtiene el ID correspondiente a un documento y se lo agrega a un documento
; Tipo de recursion: No se utiliza recursion
(define(definirID pdocs)
  (if(eq? (getDocumentosPdocs pdocs) null)
     0
     (length(getDocumentosPdocs pdocs))))

; Dominio: Una lista de usuarios registrados, un nombre de tipo string y un autor de tipo string
; Recorrido: Un booleano
; Descripcion: Verifica si un nombre de usuario ya esta registrado correctamente o si este coincide al nombre del autor de un documento
; Tipo de recursion: Recursion de Cola
; Justificacion de Recursion: Permite recorrer toda la lista de usuarios registrados
(define(revisarUsernPdocs listUser nameuser autor)
  (if (null? listUser)
      #t
      (if(not(usernamesIguales?(getPrimeroListUser listUser) nameuser autor))
         #f
         (revisarUsernPdocs(getSiguientesListUser listUser) nameuser autor)
      )
  ))

; Dominio: Una lista de usuarios, una lista vacia y una lista de permisos
; Recorrido: Una lista de permisos actualizada
; Descripcion: Funcion que filtra la lista de permisos para dejar los permisos de solo aquellos usuarios que estan registrados en paradigmadocs
; y aquellos permisos que sean validos (Son validos los permisos de lectura, escritura y comentarios)
; Tipo de recursion: Recursion de Cola (Funcion revisarUsernPdocs)
; Justificacion de recursion: Permite recorrer toda la lista de usuarios registrados y verificar los permisos correctos
(define (filtrarPermisosPdocs listUser listfinal listpermisos autor)
  (if (null? listpermisos)
      listfinal
      (if (or(revisarUsernPdocs listUser (getNamePermiso(getPrimeroListPermiso listpermisos)) autor)(not(verificarPermiso (getPermisoPermiso(getPrimeroListPermiso listpermisos)))))
          (filtrarPermisosPdocs listUser listfinal (getSiguientesListPermiso listpermisos) autor)
          (filtrarPermisosPdocs listUser (cons (getPrimeroListPermiso listpermisos) listfinal) (getSiguientesListPermiso listpermisos) autor))))

; Dominio: Una lista vacia, una lista de documentos de tipo list y un nombre de usuario de tipo string
; Recorrido: Una lista con documentos, donde todos los documentos tienen al mismo autor
; Descripcion: Funcion que obtiene a los documentos de un autor en especifico
; Tipo de recursion: Recursion de Cola
; Justificacion de recursion: Permite recorrer toda la lista de documentos de paradigmadocs
(define (obtenerDocumentosAutor listfinal listdocument user)
  (if (null? listdocument)
      listfinal
      (if(eq? (getAutorDocumento (getPrimeroListDocumento listdocument)) user)
         (obtenerDocumentosAutor (cons (getPrimeroListDocumento listdocument)listfinal) (getSiguientesListDocumento listdocument) user)
         (obtenerDocumentosAutor listfinal (getSiguientesListDocumento listdocument) user))))

; Dominio: Una lista (inicialmente vacia), una lista de documento y un user de tipo string
; Recorrido: Una lista con documentos, donde se tienen documentos cuyo autor no es el user
; Descripcion: Funcion que obtiene los documentos que no son de un autor en especifico
; Tipo de recursion: Recursion de Cola
; Justificacion de recursion: Permite recorrer toda la lista de documentos de paradigmadocs
(define (obtenerDocumentosUser listfinal listdocument user)
  (if (null? listdocument)
      listfinal
      (if(eq? (getAutorDocumento (getPrimeroListDocumento listdocument)) user)
         (obtenerDocumentosUser listfinal (getSiguientesListDocumento listdocument) user)
         (obtenerDocumentosUser (cons (getPrimeroListDocumento listdocument)listfinal) (getSiguientesListDocumento listdocument) user))))

; Dominio: Una lista (inicialmente vacia), una lista de documento y un user de tipo string
; Recorrido: Una lista con documentos, donde se tienen los documentos donde el user tiene permisos
; Descripcion: Funcion que obtiene los documentos donde el user no es el autor, pero tiene permiso de escritura o lectura
; Tipo de recursion: Recursion de Cola
; Justificacion de recursion: Permite recorrer toda la lista de documentos de paradigmadocs
(define (filtrarPorPermisos listfinal listdocument user)
  (if (eq? listdocument null)
      listfinal
      (if (puedeEscribirLeer (TienePermiso? (getPermisosDocumento (getPrimeroListDocumento listdocument)) user))
          (filtrarPorPermisos (cons (getPrimeroListDocumento listdocument) listfinal) (getSiguientesListDocumento listdocument) user)
          (filtrarPorPermisos listfinal (getSiguientesListDocumento listdocument) user))))

; Dominio: Una lista de usuarios de tipo list y un usuario de tipo user
; Recorrido: Un string
; Descripcion: Encuentra y tranforma la informacion de un usuario guardada en paradigmadocs(excepto por su
; password) en un string
; Tipo de recursion: Recursion de Cola
; Justificacion de Recursion: Sirve para verificar toda la lista de usuarios y encontrar la informacion correcta
(define(encontrarDatosUsuarioPdocs listUser nameuser)
  (if(eq?(getUsernameUser(getPrimeroListUser listUser)) nameuser)
     (string-join (list " Username:"(second(cons (date->string (getUsernameUser(getPrimeroListUser listUser))) (remove (last(remove(getUsernameUser(getPrimeroListUser listUser)) (getPrimeroListUser listUser))) (remove(getFechaUser(getPrimeroListUser listUser)) (getPrimeroListUser listUser)))))
                        "\n" "Fecha de registro:" (first(cons (date->string (getFechaUser(getPrimeroListUser listUser))) (remove (last(remove(getFechaUser(getPrimeroListUser listUser)) (getPrimeroListUser listUser))) (remove(getFechaUser(getPrimeroListUser listUser)) (getPrimeroListUser listUser))))) "\n" ))
     (encontrarDatosUsuarioPdocs(getSiguientesListUser listUser) nameuser)))

; Dominio: Una lista (inicialmente vacia), una lista de documentos y un nombre de usuario
; Recorrido: Una lista que contiene un acceso a un documento
; Descripcion: Funcion que encuentra si un usuarion tiene un permiso (independiente de cual sea) en un documento
; Tipo de recursion: Recursion de Cola
; Justificacion de Recursion: Permite recorrer toda la lista de permisos de los documentos
(define (filtrarPorAccesos listfinal listdocument user)
  (if (eq? listdocument null)
      listfinal
      (if (not(null?(TienePermiso? (getPermisosDocumento (getPrimeroListDocumento listdocument)) user)))
          (filtrarPorPermisos (cons (getPrimeroListDocumento listdocument) listfinal) (getSiguientesListDocumento listdocument) user)
          (filtrarPorPermisos listfinal (getSiguientesListDocumento listdocument) user))))

; Dominio: Una lista que contiene sublistas
; Recorrido: Una lista, pero cuyo texto ahora esta desencryptado
; Descripcion: Funcion que desencrypta un texto del historial de un documento
; Tipo de recursion: No se utiliza recursion
; NOTA: Debido a la naturaleza de map, no puedo obtener decryptFunction directamente desde Paradigmadocs
(define (desencryptarHistorial listhist)
  (if(isHistorial? listhist)
     (list (getFechaHistorial listhist) (decryptFunction (getTextoHistorial listhist)) (getIDHistorial listhist))
     null))

; Dominio: Un documento
; Recorrido: Un documento, con todo su texto desencryptado (version activa e historial)
; Descripcion: Funcion que desencrypta todo el texto de un documento, este como activo como en el historial
; Tipo de recursion: No se utiliza recursion
; NOTA: Debido a la naturaleza de map, no puedo obtener decryptFunction directamente desde Paradigmadocs
(define (desencryptarDocs listdoc)
  (if(isDocumento? listdoc)
     (list (getAutorDocumento listdoc) (getFechaDocumento listdoc)(getNombreDocumento listdoc) (decryptFunction(getContenidoDocumento listdoc)) (getPermisosDocumento listdoc) (map desencryptarHistorial (getHistorialDocumento listdoc)) (getIDDocumento listdoc))
     null))

; Dominio: Un texto de tipo string y una lista de estilos
; Recorrido: Un texto de tipo string actualizado
; Descripcion: Funcion que aplica los estilos de la lista de estilos al texto
; Tipo de recursion: Recursion de Cola
(define (aplicarEstilos textoElegido listestilos)
  (if (null? listestilos)
      textoElegido
      (if(and(equal? (car listestilos)#\u)(not(string-contains? textoElegido "#\\u")))
         (aplicarEstilos (string-append " #\\u " textoElegido " #\\u ") (cdr listestilos))
         (if(and(equal? (car listestilos)#\b)(not(string-contains? textoElegido "#\\b")))
            (aplicarEstilos(string-append " #\\b " textoElegido " #\\b ")(cdr listestilos))
            (if(and(equal? (car listestilos)#\i)(not(string-contains? textoElegido "#\\i")))
               (aplicarEstilos(string-append " #\\i " textoElegido " #\\i ") (cdr listestilos))
               (aplicarEstilos textoElegido (cdr listestilos)))))))

;-----------------------------------EJEMPLOS DE PRUEBA-----------------------------------------------------------------

(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFunction decryptFunction) )
(define pertenencia1 (isParadigmadocs? emptyGDocs))
(define pdocsErroneo1 (paradigmadocs 2 3 1 4))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))
