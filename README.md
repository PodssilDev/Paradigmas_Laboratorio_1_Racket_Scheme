# Laboratorio 1 Paradigmas de Programación: Programación Funcional
* **Autor:** John Serrano Carrasco
* **Seccion:** 13310-0-A-1
* **Fecha de inicio:** 05 de Octubre de 2021
* **Fecha de Entrega:** 04 de Noviembre de 2021

## Descripción
Este es el respositorio de mi **laboratorio 1** del curso de Paradigmas de Programación impartido en la Universidad de Santiago. En este laboratorio se ve la programación funcional, a traves del lenguaje de programación Racket o principalmente, Scheme, donde se busca crear una simulación a algo similar a una plataforma estilo Google Docs.
Para ello, se crearon distintos TDAs o Tipos de Datos Abstractos, como por ejemplo, [TDA Paradigmadocs](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDAParadigmadocs.rkt) que consiste en el TDA de una plataforma estilo Google Docs, [TDA Documento](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDADocumento.rkt) que representa a un documento donde se tiene el autor de este, fecha de creacion, nombre del documento, contenido, entre otras cosas. [TDA User](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDAUser.rkt) que representa a un usuario donde se tiene su fecha de registro, su username y su password, [TDA Access](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDAAccess.rkt) donde se guardan las access list, las cuales son las listas de permisos, [TDA Historial](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDAHistorial.rkt), que corresponde a un historial de versiones de un documento, [TDA Permiso](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDAPermiso.rkt) que corresponde a una lista de permisos 
y [TDA Fecha](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/TDAFecha.rkt) donde se tiene una fecha con formato dd mm aaaa. Cada TDA tiene sus funciones, respetando lo visto en clase como lo son constructores, funciones de pertenencia, selectores, modificadores y otras funciones.
Todos estos TDAs son utilizados en el archivo [main](https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme/blob/main/main.rkt), donde se tienen las funciones principales junto con sus ejemplos. **Se crearon 14/15 funciones en total, 9/9 obligatorias y 5/6 opcionales.**
## Prerrequisitos
Se necesita del programa [Dr.Racket 6.11](https://download.racket-lang.org) o superior para poder ejecutar los códigos del repositorio.
## Como ejecutar
El archivo main trae ejemplos para sus funciones en la parte final. Para ejecutar estos ejemplos primero se debe tener todos los TDAs anteriormente mencionados en una carpeta, junto con el archivo main. Luego, se debe aplicar "run" y luego colocar el "nombre" del ejemplo. Por ejemplo, para el ejemplo: 
```sh
(define gDocs0 (register paradigmadocs (date 26 10 2021) "user1" "pass1"))
```
solo se debe escribir gDocs0 en la consola para poder ejecutar este ejemplo de la función Register.
Se debe clonar el repositorio al computador del corrector para hacer el proceso mas facil:
```sh
git clone https://github.com/PodssilDev/Paradigmas_Laboratorio_1_Racket_Scheme.git
```

## Evaluación después de revisión
* Nota Informe: 6.9 / 7.0
* Nota Requerimientos Funcionales: 7.0 / 7.0
* Nota Requerimientos No Funcionales: 6.1 / 7.0
* Nota Final: 6.8 / 7.0
