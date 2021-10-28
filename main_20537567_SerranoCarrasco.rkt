#lang racket
; Se requiere el uso del TDA Fecha, TDA Access, TDA Paradigmadocs, TDA User y TDA Documento para la construcción de las funciones a continuación

(require "TDAFecha_20537567_SerranoCarrasco.rkt")
(require "TDAAccess_20537567_SerranoCarrasco.rkt")
(require "TDAParadigmadocs_20537567_SerranoCarrasco.rkt")
(require "TDAUser_20537567_SerranoCarrasco.rkt")
(require "TDADocumento_20537567_SerranoCarrasco.rkt")

;-----------------------------------FUNCION REGISTER-----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, una fecha de tipo fecha, un username de tipo string y un password de tipo string
; (paradigmadogs X fecha X string X string)
; Recorrido: Una plataforma de tipo paradigmadocs actualizado
; Descripcion: Funcion que registra un usuario en paradigmadocs. Si se intenta registrar un usuario ya registrado, no se registra y
; se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion:  Recursion Natural (Llamado a la funcion recursiva registerNatural)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado.
(define(register paradigmadocs date username password)
  (if (or(or(or(not(isParadigmadocs? paradigmadocs))(not(date? date)))(not(string? username)))(not(string? password)))
      paradigmadocs
      (setUserPdocs paradigmadocs (registerNatural (getUsersPdocs paradigmadocs) (user date username ((getEncryptPdocs paradigmadocs) password)) #f))))

;-----------------------------------FUNCION LOGIN---------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un username de tipo string, un password de tipo string y una funcion de tipo function
; Recorrido: Una plataforma de tipo paradigmadocs actualizado o una funcion de tipo function
; Descripcion: Funcion que loguea a un usuario que ya esta registrado y lo registra en paradigmadocs. Llama a una funcion.
; Si el usuario no se puede loguear (password erroneo o no esta registrado) de igual forma llama a la funcion, pero en paradigmadocs
; no se registra al user como activo.
; Tipo de recursion: Recursion de Cola (Llamado a la funcion recursiva revisarUserActivoPdocs)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado y si el password coincide correctamente
(define(login paradigmadocs username password operation)
  (if(or(not(string? username))(not(string? password)))
     paradigmadocs
     (if(revisarUserActivoPdocs (getUsersPdocs paradigmadocs) username password) ; Llamado a funcion recursiva de TDA Paradigmadocs
        (cond
          [(eq? operation create) (lambda(date nombre contenido)(operation (setUseractivosPdocs paradigmadocs username) date nombre contenido))]
          [(eq? operation share) (lambda(idDoc access . accesses)(operation (setUseractivosPdocs paradigmadocs username) idDoc access accesses))]
          [(eq? operation add) (lambda(idDoc date contenidoTexto)(operation (setUseractivosPdocs paradigmadocs username) idDoc date contenidoTexto))]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion)(operation(setUseractivosPdocs paradigmadocs username)idDoc idVersion))]
          [(eq? operation revokeAllAccesses)  (operation(setUseractivosPdocs paradigmadocs username))]
          [(eq? operation search) (lambda(searchText)(operation(setUseractivosPdocs paradigmadocs username) searchText))]
          [(eq? operation paradigmadocs->string) (operation(setUseractivosPdocs paradigmadocs username))]
          [(eq? operation delete) (lambda(id date numberOfCharacters)(operation(setUseractivosPdocs paradigmadocs username) id date numberOfCharacters))]
          [(eq? operation searchAndReplace) (lambda(id date searchText replaceText)(operation (setUseractivosPdocs paradigmadocs username) id date searchText replaceText))]
          [(eq? operation applyStyles) (lambda(idDoc date searchText . styles)(operation (setUseractivosPdocs paradigmadocs username) idDoc date searchText styles))]
          [(eq? operation comment) (lambda(idDoc date selectedText commenText) (operation (setUseractivosPdocs paradigmadocs username) idDoc date selectedText commenText))]
          [else (paradigmadocs)]
          )
        (cond
          [(eq? operation create) (lambda(date nombre contenido)(operation paradigmadocs date nombre contenido))]
          [(eq? operation share) (lambda(idDoc access . accesses)(operation paradigmadocs idDoc access accesses))]
          [(eq? operation add) (lambda(idDoc date contenidoTexto)(operation paradigmadocs idDoc date contenidoTexto))]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion)(operation paradigmadocs idDoc idVersion))]
          [(eq? operation revokeAllAccesses) (operation paradigmadocs)]
          [(eq? operation search)(lambda(searchText)(operation paradigmadocs searchText))]
          [(eq? operation paradigmadocs->string) (operation paradigmadocs)]
          [(eq? operation delete) (lambda(id date numberOfCharacters)(operation paradigmadocs id date numberOfCharacters))]
          [(eq? operation searchAndReplace) (lambda(id date searchText replaceText)(operation paradigmadocs id searchText replaceText))]
          [(eq? operation applyStyles) (lambda(idDoc date searchText . styles) (operation paradigmadocs idDoc date searchText styles))]
          [(eq? operation comment) (lambda(idDoc date selectedText commenText) (operation paradigmadocs idDoc date selectedText commenText))]
          [else (paradigmadocs)]
          )
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
      (if (and(and(date? date)(string? nombre))(string? contenido))
          (setDocumentoPdocs paradigmadocs (documento (obtenerActivoPdocs paradigmadocs) date nombre (encryptFunction contenido) (definirID paradigmadocs)))
          (setRemoverActivoPdocs paradigmadocs))))

;-----------------------------------FUNCION SHARE------------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer y una lista de accesos (funcion access)
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada en el caso de que se pueda dar permisos correctamente
; Descripcion: Funcion que permite dar permisos de escritura, lectura o comentarios para un documento a un usuario que esta registrado. Solo se pueden dar los
; permisos anteriormente mencionados, el autor de un documento no puede darse permisos a si mismo (ya que ya tiene permisos) y los permisos de los usuarios son
; actualizables (pueden cambiar y no puede aparecer el user mas de una vez con permisos distintos en la lista de permisos del documento). Si se intenta dar
; permisos a un usuario que no esta registrado, retorna a Paradigmadocs sin modificaciones.
; Tipo de recursion: Recursion de Cola (Funcion encontrarIDs, Funcion filtrarPermisosUnicos)
; Justificacion de Recursion: Permite encontrar al documento correcto a traves de su ID y también filtrar la lista de permisos para que no hayan usuarios repetidos
(define(share paradigmadocs idDoc access . accesses)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc))
          (setRemoverActivoPdocs paradigmadocs)
          (if(eq? (obtenerActivoPdocs paradigmadocs)(getAutorDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc)))
             (setDocumentoPermisosPdocs paradigmadocs (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc) idDoc (filtrarPermisosPdocs (getUsersPdocs paradigmadocs) null (unionAccesos access accesses) (getAutorDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc))))
             (setRemoverActivoPdocs paradigmadocs)))))

;-----------------------------------FUNCION ADD---------------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer, una fecha de tipo fecha y un texto de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada en el caso de que se pudo agregar texto a un documento correctamente
; Descripcion: Funcion que agrega texto a un documento. El texto se agrega al final y se encrypta mediante la funcion EncryptFn. El texto
; anterior se queda solo en el historial y el nuevo texto pasa a ser el "texto activo". En el caso de que no se haya logueado correctamente,
; se intente agregar texto a un documento que no existe o donde no se tiene el permiso de escritura, se retorna a paradigmadocs sin modificaciones.
; Tipo de recursion: Recursion de Cola (Funcion encontrarIDs, Funcion TienePermiso?)
; Justificacion de Recursion: Permite encontrar el documento correcto a traves de su ID y tambien permite verificar la lista de permisos para ver si
; el usuario logueado puede escribir en el documento obtenido a traves del ID.
(define (add paradigmadocs idDoc date contenidoTexto)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (not(date? date))) (not(integer? idDoc))) (not(string? contenidoTexto)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFunction contenidoTexto) date #t))
            (if(eq? (puedeEscribir(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFunction contenidoTexto) date #t))
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCION RESTOREVERSION----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer y un ID de version de texto de tipo integer
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada si es que se logró restaurar una version de un documento exitosamente
; Descripcion: Restaura la version de un documento desde el historial. Si el user intenta restaurar un documento que no es de su propiedad,
; o si intenta restaurar una versión que no existe o restaurar de un documento que no existe, se retorna a Paradigmadocs sin modificaciones
; Tipo de recursion: Recursion de Cola (Funcion encontrarIDs y restaurarVer)
; Justificacion de Recursion: Permite encontrar al documento correcto de acuerdo a su ID y a la version correcta del texto desde el historial
(define (restoreVersion paradigmadocs idDoc idVersion)
  (if(null? (getUsersactivosPdocs paradigmadocs))
     paradigmadocs
     (if(or(or(null? (encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))(not(integer? idDoc)))(not(integer? idVersion)))
        (setRemoverActivoPdocs paradigmadocs)
        (if (eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
            (setDocumentoPdocs paradigmadocs (restaurarVer (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc)idVersion))
            (setRemoverActivoPdocs paradigmadocs)))))

;-----------------------------------FUNCION REVOKEALLACCESSES----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada si es que se logra quitar permisos exitosamente
; Descripcion: Funcion que elimina los permisos de todos los documentos de un usuario. Si el user no tiene documentos, retorna a
; Paradigmadocs sin modificaciones
; Tipo de recursion: Recursion de Cola (setListaDocumentosPdocs)
; Justificacion de recursion: Permite modificar a todos los documentos de un usuario en especifico
(define (revokeAllAccesses paradigmadocs)
  (if(null? (getUsersactivosPdocs paradigmadocs))
     paradigmadocs
     (if(eq? null (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)))
        (setRemoverActivoPdocs paradigmadocs)
        (setListaDocumentosPdocs paradigmadocs (map eliminarPermisos (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)) )))))

;-----------------------------------FUNCION SEARCH-----------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs y un texto de tipo string
; Recorrido: Una lista de documentos
; Descripcion: Funcion que encuentra todos los documentos que contienen al texto de entrada, lo busca tanto en el historial como en la version activa
; y retorna una lista que contiene a todos los documentos que cumplen ese criterio. Solo el dueño del documento y aquellos usuarios con permiso de escritura
; o lectura pueden realizar una busqueda dentro de ese documento. Si no se encuentra ningun documento o el user intenta buscar incorrectamente, se retorna null
; Tipo de recursion: Recursion de Cola (obtenerDocumentosUser, obtenerDocumentosAutor, encontrarTexto)
; Justificacion de recursion: Permite encontrar a todos los documentos correctos (de acuerdo al user logueado) y a recorrer todo el historial
(define(search paradigmadocs searchText)
  (if(or(null? (getUsersactivosPdocs paradigmadocs))(not(string? searchText)))
     null
     (if(null? (append (filtrarPorPermisos null (obtenerDocumentosUser null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)) (obtenerActivoPdocs paradigmadocs)) (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs))))
        null
        (map eliminarRastro(filter encontrarTexto (addTextoSearch null (append (filtrarPorPermisos null (obtenerDocumentosUser null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)) (obtenerActivoPdocs paradigmadocs) ) (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs) )) (encryptFunction searchText)))))))

;-----------------------------------FUNCION PARADIGMADOCS->STRING---------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs
; Recorrido: Un string que contiene informacion
; Descripcion: Funcion que retorna un string, pero este difiere dependiendo de si un Usuario se loguea correctamente o no. Para el primer caso,
; retorna un string que contiene toda la informacion al usuario, es decir, su username, fecha de registro, sus documentos propios y a los que tiene acceso,
; junto con toda la informacion de aquellos documentos. Para el segundo caso, retorna todo el contenido de Paradigmadocs, es decir, nombre de la plataforma,
; fecha de creacion de plataforma, usuarios registrado (cada user tiene una fecha de registro propia, un username y un password encryptado) y todos los documentos
; creados en la plataforma, los cuales contienen toda su informacion.
; Tipo de recursion: No se utiliza recursion
(define (paradigmadocs->string paradigmadocs)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      (string-join (list " Nombre de la plataforma:" (getNombrePdocs paradigmadocs)"\n" "Fecha de creacion:" (date->string (getFechaPdocs paradigmadocs))"\n" "Usuarios registrados en la plataforma:\n" (string-join(map userToString (getUsersPdocs paradigmadocs))) "\n" "Documentos creados en la plataforma:\n" (string-join(map documentoToString (map desencryptarDocs (getDocumentosPdocs paradigmadocs))))))
      (string-join (list (encontrarDatosUsuarioPdocs (getUsersPdocs paradigmadocs)(obtenerActivoPdocs paradigmadocs))"Los documentos propios del usuario o los cuales puede acceder son:\n"
                         (string-join(map documentoToString (map desencryptarDocs (append (filtrarPorAccesos null (obtenerDocumentosUser null (getDocumentosPdocs paradigmadocs)(obtenerActivoPdocs paradigmadocs))
                         (obtenerActivoPdocs paradigmadocs)) (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)    )))))
                         )
                   )
      )
  )

;-----------------------------------FUNCION DELETE (OPCIONAL)-------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de tipo integer, una fecha de tipo fecha y un numero de caracteres de tipo integer
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada en el caso de que la operacion se realice con exito
; Descripcion: Funcion que elimina una cantidad de caracteres al texto de un documento en especifico. Si no se encuentra el documento, se retorna
; a paradigmadocs sin modificaciones. Si la cantidad de caracteres a eliminar es mayor al largo del texto, se elimina todo el texto. Se desencrypta
; el texto para poder eliminar las palabras y luego se vuelve a encryptar, quedando registrado como texto activo y en el historial de versiones.
; Tipo de recursion: Recursion de Cola (Funcion encontrarIDs)
; Justificacion de recursion: Permite encontrar al documento correcto, recorriendo toda la lista de documentos
(define (delete paradigmadocs id date numberOfCharacters)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (not(date? date))) (not(integer? id))) (not(integer? numberOfCharacters)))(<= numberOfCharacters 0))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) id)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id) ((getEncryptPdocs paradigmadocs)(deleteCharsDoc null (string->list(encryptFunction (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id)))) ( -(length(string->list(encryptFunction (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id))))) numberOfCharacters) )) date #f))
            (if(eq?(puedeEscribir(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (car(getUsersactivosPdocs paradigmadocs))))#t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) ((getEncryptPdocs paradigmadocs)(deleteCharsDoc null (string->list(encryptFunction (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id)))) ( -(length(string->list(encryptFunction (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id))))) numberOfCharacters))) date #f)
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCION SEARCHANDREPLACE (OPCIONAL)---------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de tipo integer, una fecha de tipo fecha, y dos strings
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada
; Descripcion: Funcion que busca un texto en especifico en un documento y lo reemplaza si es que la busqueda fue exitosa.
; Si se intenta buscar un documento que no existe o no se reemplaza el texto, se retorna a paradigmadocs sin modificaciones
; Tipo de recursion: Recursion de Cola
(define (searchAndReplace paradigmadocs id date searchText replaceText)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (not(date? date))) (not(integer? id))) (not(string? searchText)))(not(string? replaceText)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) id)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id) (encryptFunction (string-replace (decryptFunction(getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) id))) searchText replaceText) ) date #f))
            (if(eq? (puedeEscribir(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id) (encryptFunction (string-replace (decryptFunction(getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) id))) searchText replaceText)) date #f))
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCION APPLYSTYLES (OPCIONAL)--------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de tipo integer, una fecha de tipo fecha, un texto de tipo string y
; una lista de estilos especificos
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada
; Descripcion: Funcion que aplica una cantidad de determinada de estilos a un texto. Se omiten los estilos no validos
; y si se intenta aplicar a un texto que no existe o a un documento que no existe, se retorna a paradigmadocs sin modificaciones
; Tipo de recursion: Recursion de Cola
(define (applyStyles paradigmadocs idDoc date searchText . styles)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (not(date? date))) (not(integer? idDoc))) (not(string? searchText)))(not(list? styles)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFunction (string-replace ((getDecryptPdocs paradigmadocs) (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))) searchText (aplicarEstilos searchText (applylist styles)) )) date #f))
            (if(eq? (puedeComentar(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFunction (string-replace ((getDecryptPdocs paradigmadocs) (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))) searchText (aplicarEstilos searchText (applylist styles)) )) date #f))
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCION COMMENT (OPCIONAL)------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de tipo integer, una fecha de tipo fecha y dos textos de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada
; Descripcion: Funcion que comenta un texto en especifico de un documento en especifico. Si no se encuentra el texto o
; el documento, se retorna a paradigmadocs sin modificaciones
; Tipo de recursion: Recursion de Cola
(define (comment paradigmadocs idDoc date selectedText commenText)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (not(date? date))) (not(integer? idDoc))) (not(string? selectedText)))(not(string? commenText)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) ((getEncryptPdocs paradigmadocs)(string-replace (decryptFunction(obtenerSinComentario(encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))) selectedText (string-append selectedText "&C&(" commenText ")&C&")) ) date #f))
            (if(eq? (puedeComentar(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) ( (getEncryptPdocs paradigmadocs)(string-replace (decryptFunction(obtenerSinComentario (encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))) selectedText (string-append selectedText "&C&(" commenText ")&C&"))) date #f))
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCIONES ENCRYPT Y DECRYPT (OPCIONALES)-------------------------------------------------

; Dominio: Un texto de tipo string
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que encripta un texto
; Tipo de recursion: No se utiliza recursion
(define (encryptFn textoEncrypt)
  (if (string? textoEncrypt)
      (list->string (map mezclarLetras (string->list textoEncrypt)))
      ("")))

; Dominio: Un texto de tipo string
; Recorrido: Un texto de tipo string
; Descripcion: Funcion que decripta un texto
; Tipo de recursion: No se utiliza recursion
(define (decryptFn textoEncrypt)
  (if (string? textoEncrypt)
      (list->string (map mezclarLetras (string->list textoEncrypt)))
      ("")))

;-----------------------------------EJEMPLOS PARA LA FUNCION REGISTER-----------------------------------------------------

; Nota: emptyGDocs es una plataforma de tipo paradigmadocs. Fue creado dentro de TDA Paradigmadocs.

; Contra Ejemplo: Intentando registrar a un user cuyo username y password es de formato incorrecto
(define gDocs1a(register emptyGDocs "fecha" 1233434 2222))

; Ejemplo similar enunciado del laboratorio
(define gDocs1b
(register (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3") (date 25 10 2021) "user4" "pass4"))

; Registro de un quinto usuario
(define gDocs1c (register gDocs1b (date 26 10 2021) "user5" "pass5"))

; Registro de dos usuarios que ya estan registrados
(define gDocs1d (register(register gDocs1c (date 09 10 2021) "user1" "nuevopass")(date 09 10 2021) "user2" "passdistinto"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y CREATE-----------------------------------------------------

; Contra ejemplo: Se intenta crear un documento pero el login es incorrecto (Se retorna a Pdocs sin modificaciones)
(define gDocs2a ((login gDocs1d "user1" "pass2" create) (date 26 10 2021) "doc1" "cont doc1"))

; Contra ejemplo: Se intenta crear un documento pero se intenta loguear a un usuario que no esta registrado
(define gDocs2b ((login gDocs2a "user6" "pass6" create) (date 26 10 2021) "doc1" "cont doc1"))

; Contra ejemplo: Se loguea correctamente a un user pero se intenta crear un documento con formato incorrecto (integers en ves de strings)
(define gDocs2c ((login gDocs2b "user2" "pass2" create) (date 32 13 1992) 12334343 251331))

;Se loguea a un user y se crea un documento correctamente
(define gDocs2d ((login gDocs2c "user1" "pass1" create) (date 25 10 2021) "doc0" "doc0"))

; Se loguea al user2 correctamente y se crea un segundo documento
(define gDocs2e ((login gDocs2d "user2" "pass2" create) (date 26 10 2021) "doc1" "doc1"))

; Se loguea al user1 correctamente y user1 crea un tercer documento
(define gDocs2f ((login gDocs2e "user1" "pass1" create) (date 26 10 2021) "doc2" "doc2"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y SHARE-----------------------------------------------------

; Contra ejemplo erroneo:  User 3 se loguea correctamente pero intenta darse permisos en un documento que no es de su propiedad
(define gDocs3a ((login gDocs2f "user3" "pass3" share) 0 (access "user3" #\c)))

; Contra ejemplo: Se intenta usar share sin loguear y se intenta dar acceso a un documento que no existe
(define gDocs3b (share gDocs3a 3 (access "user2" #\c)))

; Mitad Contra Ejemplo: User 1 se loguea correctamente, pero intenta dar permisos a un User que no esta registrado, aunque se le dan
; permisos correctamente a User 3, luego intenta darse permisos a si mismo e intenta dar un permiso que no existe a user 3
(define gDocs3c ((login gDocs3b "user1" "pass1" share) 2 (access "user6" #\r)(access "user1" #\c) (access "user3" #\w) (access "user3" #\b)))

; User 1 se loguea correctamente y le da acceso de lectura al user 2 para el documento 0
(define gDocs3d ((login gDocs3c "user1" "pass1" share) 0 (access "user2" #\r)))

; User 2 se loguea correctamente y le da acceso de lectura a user 1 y a acceso de escritura a user 3 para el documento 1
(define gDocs3e ((login gDocs3d "user2" "pass2" share) 1 (access "user1" #\r) (access "user3" #\w)))

; User 1 se loguea correctamente y le cambia los permisos a User 3. Este se queda con el ultimo (lectura en este caso)
(define gDocs3f ((login gDocs3e "user1" "pass1" share) 2 (access "user3" #\w)(access "user3" #\c) (access "user3" #\r)))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y ADD--------------------------------------------------------

; Contra ejemplo: User 1 se loguea pero intenta agregar texto de formato incorrecto a uno de sus documentos
(define gDocs4a ((login gDocs3e "user1" "pass1" add) 0 (date 16 10 2021) 123313))

; Contra ejemplo: User 1 se loguea correctamente pero intenta escribir en un documento que no existe
(define gDocs4b ((login gDocs4a "user1" "pass1" add) 3  (date 27 10 2021) "Paradigmas"))

; User 1 se loguea correctamente y agrega contenido a su propio documento. 
(define gDocs4c ((login gDocs4b "user1" "pass1"  add) 0 (date 27 10 2021) "t"))

; User 3 se loguea correctamente y agrega contenido a un documento donde tiene permisos de escritura
(define gDocs4d ((login gDocs4c "user3" "pass3" add) 1 (date 27 10 2021) "h"))

; User 3 se loguea correctamente pero intenta escribir en un documento donde no tiene permisos
(define gDocs4e ((login gDocs4d "user3" "pass3" add) 0 (date 16 10 2021) "Nuevo"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y RESTOREVERSION----------------------------------------

; Contra ejemplo: User 2 intenta restaurar una version de un documento que no es de su propiedad
(define gDocs5a ((login gDocs4e "user2" "pass2" restoreVersion) 0 0))

; Contra ejemplo: User 1 intenta restaurar una version que no existe uno de sus documentos
(define gDocs5b ((login gDocs5a "user1" "pass1" restoreVersion) 0 5))

; Contra ejemplo: User 1 intenta restaurar una version de un documento que no existe
(define gDocs5c ((login gDocs5b "user1" "pass1" restoreVersion) 5 0))

; User 1 logra restaurar la version inicial del documento 0
(define gDocs5d ((login gDocs5c "user1" "pass1" restoreVersion) 0 0))

; User 2 logra restaurar la version inicial del documento 1
(define gDocs5e ((login gDocs5d "user2" "pass2" restoreVersion) 1 0))
;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y REVOKEALLACCESSES-----------------------------

; User 1 se loguea y retira el permiso de todos sus documentos
(define gDocs6a (login gDocs5e "user1" "pass1" revokeAllAccesses))

; User 2 se loguea y retira el permiso de todos sus documentos
(define gDocs6b (login gDocs6a "user2" "pass2" revokeAllAccesses))

; User 3 se loguea e intenta quitar permisos, pero no tiene documentos
(define gDocs6c (login gDocs6a "user3" "pass3" revokeAllAccesses))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y SEARCH----------------------------------------

; Contra ejemplo: User 1 realiza una busqueda cuyo formato es incorrecto
(define gDocs7a ((login gDocs5e "user1" "pass1" search) 323232))

; User 1 realiza la busqueda de "o"
(define gDocs7b ((login gDocs5e "user1"  "pass1" search) "o"))

; User 5 realiza una busqueda, pero no es autor de ningun documento ni tiene permisos
(define gDocs7c ((login gDocs5e "user5" "pass5" search) "d"))

; User 3 realiza una busqueda, pero no es autor de ningun documento, aunque tiene permisos en otros
(define gDocs7d ((login gDocs5e "user3" "pass3" search) "doc"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y PARADIGMADOCS->STRING-------------------------

; User 1 utiliza paradigmadocs->string y se obtiene el string con toda su informacion
(define gDocs8a (login gDocs5e "user1" "pass1" paradigmadocs->string))

; User 2 utiliza paradigmadocs->string y se obtiene el string con toda su informacion
(define gDocs8b (login gDocs5e "user2" "pass2" paradigmadocs->string))

; User 3 utiliza paradigmadocs->string y se obtiene el string con toda su informacion
(define gDocs8c (login gDocs5e "user3" "pass3" paradigmadocs->string))

; User 4 utiliza paradigmadocs->string y se obtiene el string con toda su informacion
(define gDocs8d (login gDocs5e "user4" "pass4" paradigmadocs->string))

; User 5 se loguea incorrectamente,  utiliza paradigmadocs->string y este muestra toda la informacion de la plataforma
(define gDocs8e (login gDocs5e "user5" "pas5s" paradigmadocs->string))

; Nadie se loguea y se utiliza paradigmadocs->string
(define gDocs8f (paradigmadocs->string gDocs5e))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y DELETE------------------------------------

; Contra ejemplo: User 1 se loguea, pero decide borrar 0 caracteres del documento 0
(define gDocs9a ((login gDocs5e "user1" "pass1" delete) 0 (date 27 10 2021) 0))

; User 1 se loguea y borra 2 caracteres del documento 0
(define gDocs9b ((login gDocs5e "user1" "pass1" delete) 0 (date 27 10 2021) 2))

; User 2 se loguea y decide borrar todos los caracteres del documento 1
(define gDocs9c ((login gDocs5e "user2" "pass2" delete) 1 (date 27 10 2021) 100))

; User 2 se loguea e intenta borrar todo el contenido del documento 0, pero solo tiene permiso de lectura
(define gDocs9d ((login gDocs5e "user2" "pass2" delete) 0 (date 27 10 2021) 199))

; User 1 se loguea y borra 3 caracteres del contenido del documento 2
(define gDocs9e ((login gDocs5e "user1" "pass1" delete) 2 (date 27 10 2021) 3))

; User 5 se loguea e intenta borrar documentos que no existen
(define gDocs9f ((login gDocs5e "user5" "pass5" delete) 6 (date 27 10 2021) 1000))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN SEARCH AND REPLACE----------------------------

; Contra ejemplo: User 1 se loguea pero intenta reemplazar de manera incorrecta
(define gDocs10a ((login gDocs5e "user1" "pass1" searchAndReplace) 0 (date 24 10 2021) 2323232 "john"))

; User 1 se loguea correctamente y reemplaza "oc" por "ac" en el documento 0
(define gDocs10b ((login gDocs5e "user1" "pass1" searchAndReplace) 0 (date 27 10 2021) "oc" "ac"))

; User 2 se loguea y reemplaza "doc" por "document" en el documento 1
(define gDocs10c ((login gDocs5e "user2" "pass2" searchAndReplace) 1 (date 27 10 2021) "doc1" "document"))

; User 3 se loguea y gracias a su permiso en el documento 1, cambia "document" por "origin"
(define gDocs10d ((login gDocs10c "user3" "pass3" searchAndReplace) 1 (date 27 10 2021) "document" "origin"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y APPLYSTYLES----------------------------

; Contra ejemplo: User 4 se loguea e intenta aplicar estilos en un documento donde no tiene permisos
(define gDocs11a ((login gDocs5e "user4" "pass4" applyStyles) 0 (date 30 11 2021) "doc" #\b #\u))

; Contra ejemplo: User 1 se loguea  e intenta aplicar permisos a un texto que no esta en el documento
(define gDocs11b ((login gDocs5e "user1" "pass1" applyStyles) 0 (date 30 11 2021) "documento" #\b #\u))

; Mitad Contra ejemplo: User 1 se loguea y aplica todos los estilos a un texto, pero también intenta aplicar
; estilos que no existen
(define gDocs11c ((login gDocs5e "user1" "pass1" applyStyles) 0 (date 28 10 2021) "doc" #\b #\u #\i #\a #\e))

; User 3 se loguea y aplica estilos en un documento donde tiene permisos
(define gDocs11d ((login gDocs5e "user3" "pass3" applyStyles) 1 (date 28 10 2021) "d" #\u #\i))

; User 2 se loguea y aplica estilos en su documento al texto "doc"
(define gDocs11e ((login gDocs11c "user2" "pass2" applyStyles) 1 (date 28 10 2021) "doc" #\b))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y COMMENT-------------------------------

; Contra ejemplo: User2 intenta comentar, pero no tiene permisos para hacerlo
(define gDocs12a((login gDocs5e "user2"  "pass2" comment) 0 (date 28 10 2021) "doc" "comment"))

; User 1 se loguea, comenta en el documento 0,el texto "doc"
(define gDocs12b((login gDocs5e "user1"  "pass1" comment) 0 (date 28 10 2021) "doc" "1st comment"))

; User 1 nuevamente repite la operacion, pero esta vez usa otro comentario. La version activa antigua
; se queda en el historial, junto con el comentario anterior
(define gDocs12c((login gDocs12b "user1"  "pass1" comment) 0 (date 28 10 2021) "doc" "2nd comment"))

; User 3 se loguea y comenta en un documento donde tiene permisos
(define gDocs12d((login gDocs12c "user3"  "pass3" comment) 1 (date 28 10 2021) "doc" "3rd comment"))

;-----------------------------------EJEMPLOS PARA LA FUNCIONES ENCRYPT2 Y DECRYPT2 (OPCIONALES)------------

(define gDocs13ae (encryptFn "hello"))
(define gDocs13ad (decryptFn "svool"))

(define gDocs13be (encryptFn "PaRaDiGmAs"))
(define gDocs13bd (decryptFn "KzIzWrTnZh"))

(define gDocs13ce (encryptFn "Pr0g1MaC9o8-.?"))
(define gDocs13cd (decryptFn "Ki9t8NzX0l1_/!"))
