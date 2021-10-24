#lang racket
; Se requiere el uso del TDA Fecha, TDA Paradigmadocs, TDA User y TDA Documento para la construcción de las funciones a continuación

(require "TDAFecha_20537567_SerranoCarrasco.rkt")
(require "TDAParadigmadocs_20537567_SerranoCarrasco.rkt")
(require "TDAUser_20537567_SerranoCarrasco.rkt")
(require "TDADocumento_20537567_SerranoCarrasco.rkt")

;-----------------------------------FUNCION REGISTER-----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, una fecha de tipo fecha, un username de tipo string y un password de tipo string
; (paradigmadogs X fecha X string X string)
; Recorrido: Una plataforma de tipo paradigmadocs actualizado
; Descripcion: Funcion que registra un usuario en paradigmadocs. Si se intenta registrar un usuario ya registrado, no se registra y
; se devuelve a paradigmadocs sin modificaciones
; Tipo de recursion:  Recursion Natural (Llamado a la funcion recursiva revisarUsuarioPdocs)
; Justificacion de recursion: Es necesario para verificar si un usuario ya esta registrado.
(define(register paradigmadocs date username password)
  (if (or(or(or(not(isParadigmadocs? paradigmadocs))(not(date? date)))(not(string? username)))(not(string? password)))
      paradigmadocs
      (if(revisarUsuarioPdocs (getUsersPdocs paradigmadocs)(user date username (encryptFn password))) ; Llamado a funcion recursiva de TDA Paradigmadocs
         (setUserPdocs paradigmadocs (user date username (encryptFn password)
                       ))
         paradigmadocs)
      )
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
          [(eq? operation comment) (lambda(idDoc date selectedText commenText) (operation paradigmadocs idDoc selectedText commenText))]
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
          (setDocumentoPdocs paradigmadocs (documento (obtenerActivoPdocs paradigmadocs) date nombre (encryptFn contenido) (definirID paradigmadocs)))
          (setRemoverActivoPdocs paradigmadocs))))

;-----------------------------------FUNCION SHARE------------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer y una lista de accesos (funcion access)
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada en el caso de que se pueda dar permisos correctamente
; Descripcion: Funcion que permite dar permisos de escritura, lectura o comentarios para un documento a un usuario que esta registrado. Solo se pueden dar los
; permisos anteriormente mencionados, el autor de un documento no puede darse permisos a si mismo (ya que ya tiene permisos) y los permisos de los usuarios son
; actualizables (pueden cambiar y no puede aparecer el user mas de una vez con permisos distintos en la lista de permisos del documento). Si se intenta dar
; permisos a un usuario que no esta registrado, retorna a Paradigmadocs sin modificaciones.
; Tipo de recursion: Recursion Natural (Funcion encontrarIDs, Funcion filtrarPermisosUnicos)
; Justificacion de Recursion: Permite encontrar al documento correcto a traves de su ID y también filtrar la lista de permisos para que no hayan usuarios repetidos
(define(share paradigmadocs idDoc access . accesses)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc))
          (setRemoverActivoPdocs paradigmadocs)
          (if(eq? (obtenerActivoPdocs paradigmadocs)(getAutorDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc)))
             (setDocumentoPermisosPdocs paradigmadocs (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc) idDoc (filtrarPermisosPdocs (getUsersPdocs paradigmadocs) null (cons  access (car accesses))(getAutorDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs)idDoc))))
             (setRemoverActivoPdocs paradigmadocs)))))

;-----------------------------------FUNCION ADD---------------------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer, una fecha de tipo fecha y un texto de tipo string
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada en el caso de que se pudo agregar texto a un documento correctamente
; Descripcion: Funcion que agrega texto a un documento. El texto se agrega al final y se encrypta mediante la funcion EncryptFn. El texto
; anterior se queda solo en el historial y el nuevo texto pasa a ser el "texto activo". En el caso de que no se haya logueado correctamente,
; se intente agregar texto a un documento que no existe o donde no se tiene el permiso de escritura, se retorna a paradigmadocs sin modificaciones.
; Tipo de recursion: Recursion Natural (Funcion encontrarIDs, Funcion TienePermiso?)
; Justificacion de Recursion: Permite encontrar el documento correcto a traves de su ID y tambien permite verificar la lista de permisos para ver si
; el usuario logueado puede escribir en el documento obtenido a traves del ID.
(define (add paradigmadocs idDoc date contenidoTexto)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (not(date? date))) (not(integer? idDoc))) (not(string? contenidoTexto)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFn contenidoTexto) date #t))
            (if(eq? (puedeEscribir(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFn contenidoTexto) date #t))
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCION RESTOREVERSION----------------------------------------------------------------

; Dominio: Una plataforma de tipo paradigmadocs, un ID de documento de tipo integer y un ID de version de texto de tipo integer
; Recorrido: Una plataforma de tipo paradigmadocs, actualizada si es que se logró restaurar una version de un documento exitosamente
; Descripcion: Restaura la version de un documento desde el historial. Si el user intenta restaurar un documento que no es de su propiedad,
; o si intenta restaurar una versión que no existe o restaurar de un documento que no existe, se retorna a Paradigmadocs sin modificaciones
; Tipo de recursion: Recursion Natural (Funcion encontrarIDs y restaurarVer)
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
; Tipo de recursion: Recursion Natural (setListaDocumentosPdocs)
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
; Tipo de recursion: Recursion Natural (obtenerDocumentosUser, obtenerDocumentosAutor, encontrarTexto)
; Justificacion de recursion: Permite encontrar a todos los documentos correctos (de acuerdo al user logueado) y a recorrer todo el historial
(define(search paradigmadocs searchText)
  (if(or(null? (getUsersactivosPdocs paradigmadocs))(not(string? searchText)))
     null
     (if(null? (append (filtrarPorPermisos null (obtenerDocumentosUser null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)) (obtenerActivoPdocs paradigmadocs)) (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs))))
        null
        (map eliminarRastro(filter encontrarTexto (addTextoSearch null (append (filtrarPorPermisos null (obtenerDocumentosUser null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs)) (obtenerActivoPdocs paradigmadocs) ) (obtenerDocumentosAutor null (getDocumentosPdocs paradigmadocs) (obtenerActivoPdocs paradigmadocs) )) (encryptFn searchText)))))))

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
; Tipo de recursion: Recursion Natural (Funcion encontrarIDs)
; Justificacion de recursion: Permite encontrar al documento correcto, recorriendo toda la lista de documentos
(define (delete paradigmadocs id date numberOfCharacters)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (not(date? date))) (not(integer? id))) (not(integer? numberOfCharacters)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) id)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id) ((getEncryptPdocs paradigmadocs)(deleteCharsDoc null (string->list(encryptFn (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id)))) ( -(length(string->list(encryptFn (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id))))) numberOfCharacters) )) date #f))
            (if(eq? (puedeEscribir(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (car(getUsersactivosPdocs paradigmadocs))) #t))
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) ((getEncryptPdocs paradigmadocs)(deleteCharsDoc null (string->list(encryptFn (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id)))) ( -(length(string->list(encryptFn (getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs)id))))) numberOfCharacters))) date #f)
               (setRemoverActivoPdocs paradigmadocs))))))

;-----------------------------------FUNCION SEARCHANDREPLACE (OPCIONAL)---------------------------------------------------

(define (searchAndReplace paradigmadocs id date searchText replaceText)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (not(date? date))) (not(integer? id))) (not(string? searchText)))(not(string? replaceText)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) id)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id) (encryptFn (string-replace (decryptFn(getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) id))) searchText replaceText) ) date #f))
            (if(eq? (puedeEscribir(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) id) (encryptFn (string-replace (decryptFn(getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) id))) searchText replaceText)) date #f))
               (setRemoverActivoPdocs paradigmadocs))))))
;-----------------------------------EJEMPLOS PARA LAS FUNCIONES-----------------------------------------------------------

(define (comment paradigmadocs idDoc date selectedText commenText)
  (if (null? (getUsersactivosPdocs paradigmadocs))
      paradigmadocs
      (if(or(or(or(or(null? (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (not(date? date))) (not(integer? idDoc))) (not(string? selectedText)))(not(string? commenText)))
         (setRemoverActivoPdocs paradigmadocs)
         (if(eq? (obtenerActivoPdocs paradigmadocs) (getAutorDocumento(encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)))
            (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFn (string-replace (decryptFn(getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))) selectedText (string-append selectedText "&C&(" commenText ")&C&")) ) date #f))
            (if(eq? (puedeComentar(TienePermiso? (getPermisosDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc)) (obtenerActivoPdocs paradigmadocs) ) ) #t)
               (setDocumentoPdocs paradigmadocs(setContenidoDocumento (encontrarIDs (getDocumentosPdocs paradigmadocs) idDoc) (encryptFn (string-replace (decryptFn(getContenidoDocumento (encontrarIDs(getDocumentosPdocs paradigmadocs) idDoc))) selectedText (string-append selectedText "&C&(" commenText ")&C&"))) date #f))
               (setRemoverActivoPdocs paradigmadocs))))))
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
(define gDocs2000 ((login gDocs2 "user1" "pass2" create) (date 30 08 2021) "doc1" "contenido doc1"))

; Ejemplo erroneo: Se intenta crear otro documento pero se intenta loguear a un usuario que no esta registrado
(define gDocs3 ((login gDocs2 "user5" "pass5" create) (date 30 08 2021) "doc2" "contenido doc2"))

; Ejempplo erroneo: Se loguea correctamente a un user pero se intenta crear otro documento con formato incorrecto (integers en ves de strings)
(define gDocs3000 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) 22222 12334343))

; Se loguea al user2 correctamente y se crea un segundo documento
(define gDocs4 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc1" "contenido doc1"))

; Se loguea al user1 correctamente y user1 crea un tercer documento
(define gDocs4000 ((login gDocs4 "user1" "pass1" create) (date 30 08 2021) "doc2" "contenidx doc2"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y SHARE-----------------------------------------------------

; User 1 se loguea correctamente y le da acceso de lectura al user 2 para el documento 0
(define gDocs5 ((login gDocs4000 "user1" "pass1" share) 0 (access "user2" #\r)))

; User 2 se loguea correctamente y le da acceso de lectura a user 1 y a acceso de escritura a user 3 para el documento 1
(define gDocs6 ((login gDocs5 "user2" "pass2" share) 1 (access "user1" #\r) (access "user3" #\w)))

; Ejemplo erroneo:  User 3 se loguea correctamente pero intenta darse permisos en un documento que no es de su propiedad
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 0 (access "user3" #\c)))

; Ejemplo erroneo: User 1 se loguea correctamente, pero intenta dar permisos a un User que no esta registrado, aunque se le dan
; permisos correctamente a User 3, luego intenta darse permisos a si mismo e intenta dar un permiso que no existe a user 3
(define gDocs7000 ((login gDocs6 "user1" "pass1" share) 2 (access "user4" #\r)(access "user1" #\c) (access "user3" #\w) (access "user3" #\b)))

; Ejemplo erroneo: Se intenta usar share sin loguear y se intenta dar acceso a un documento que no existe
(define gDocs7001 (share gDocs7000 3 (access "user2" #\c)))

; Ejemplo erroneo: User 1 se loguea correctamente pero intenta dar permisos a un documento que no existe
(define gDocs7002 ((login gDocs7000 "user1" "pass1" share) 3 (access "user3" #\w)(access "user3" #\c) (access "user3" #\r)))

; User 1 se loguea correctamente y le cambia los permisos a User 3. Este se queda con el ultimo (lectura en este caso)
(define gDocs7003 ((login gDocs7000 "user1" "pass1" share) 2 (access "user3" #\w)(access "user3" #\c) (access "user3" #\r)))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y ADD--------------------------------------------------------

; User 1 se loguea correctamente y agrega contenido a su propio documento. 
(define gDocs8 ((login gDocs7003 "user1" "pass1"  add) 0 (date 30 11 2021) "perro"))

; User 3 se loguea correctamente y agrega contenido a un documento donde tiene permisos de escritura
(define gDocs9 ((login gDocs8 "user3" "pass3" add) 1 (date 30 11 2021)"doc" ))

; User 1 se loguea correctamente pero intenta escribir en un documento que no existe
(define gDocs8000 ((login gDocs7003 "user1" "pass1" add) 3  (date 16 10 2021) "Paradigmas"))

; User 3 se loguea correctamente pero intenta escribir en un documento donde no tiene permisos
(define gDocs9000 ((login gDocs8 "user3" "pass3" add) 1 (date 16 10 2021) "Texto"))

; User 1 se loguea pero intenta agregar texto de formato incorrecto a uno de sus documentos
(define gDocs10 ((login gDocs9 "user1" "pass3" add) 0 (date 16 10 2021) 123313))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y RESTOREVERSION----------------------------------------

; User 2 intenta restaurar una version de un documento que no es de su propiedad
(define gDocs11 ((login gDocs9 "user2" "pass2" restoreVersion) 0 0))

; User 1 intenta restaurar una version de un documento que no existe
(define gDocs1100 ((login gDocs9 "user1" "pass1" restoreVersion) 5 0))

; User 1 intenta restaurar una version que no existe uno de sus documentos
(define gDocs1101 ((login gDocs9 "user1" "pass1" restoreVersion) 0 5))

; User 1 intenta restaurar incorrectamente una version de un documento
(define gDocs1102 ((login gDocs9 "user1" "pass1" restoreVersion) "0" "0"))

; User 1 logra restaurar la version inicial del documento 0
(define gDocs1103 ((login gDocs9 "user1" "pass1" restoreVersion) 0 0))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y REVOKEALLACCESSES-----------------------------

;User 1 se loguea y retira el permiso de todos sus documentos
(define gDocs12 (login gDocs9 "user1" "pass1" revokeAllAccesses))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y SEARCH----------------------------------------

(define gDocs13 ((login gDocs9 "user1" "pass1" search) "o"))

;-----------------------------------EJEMPLOS PARA LA FUNCION LOGIN Y PARADIGMADOCS->STRING-------------------------

(define gDocs14 (login gDocs9 "user1" "pass1" paradigmadocs->string))

(define gDocs15 ((login gDocs9 "user1" "pass1" delete) 0 (date 20 10 2021) 3))

(define gDocs16 ((login gDocs7000 "user1" "pass1" searchAndReplace) 0 (date 24 10 2021) "doc" "john"))
(define gDocs17 ((login gDocs7000 "user1" "pass1" comment) 1 (date 24 10 2021) "doc" "Este es un comment"))