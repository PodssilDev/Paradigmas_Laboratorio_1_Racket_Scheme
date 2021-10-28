<<<<<<< Updated upstream
# Laboratorio 1 Paradigmas de Programación: Programación Funcional
**Autor:** John Serrano Carrasco
**Seccion:** 13310-0-A-1
**Fecha de inicio:** 05 de Octubre de 2021
**Fecha de Entrega:** 04 de Noviembre de 2021

## Descripción
Este es el respositorio de mi **laboratorio 1** del curso de Paradigmas de Programación impartido en la Universidad de Santiago. En este laboratorio se ve la programación funcional, a traves del lenguaje de programación Racket o principalmente, Scheme, donde se busca crear una simulación a algo similar a una plataforma estilo Google Docs.
Para ello, se crearon distintos TDAs o Tipos de Datos Abstractos, como por ejemplo, [TDA Paradigmadocs](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDAParadigmadocs_20537567_SerranoCarrasco.rkt) que consiste en el TDA de una plataforma estilo Google Docs, [TDA Documento](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDADocumento_20537567_SerranoCarrasco.rkt) que representa a un documento donde se tiene el autor de este, fecha de creacion, nombre del documento, contenido, entre otras cosas. [TDA User](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDAUser_20537567_SerranoCarrasco.rkt) que representa a un usuario donde se tiene su fecha de registro, su username y su password y [TDA Fecha](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDAFecha_20537567_SerranoCarrasco.rkt) donde se tiene una fecha con formato dd mm aaaa. Cada TDA tiene sus funciones, respetando lo visto en clase como lo son constructores, funciones de pertenencia, selectores, modificadores y otras funciones.
Todos estos TDAs son utilizados en el archivo [main](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/main_20537567_SerranoCarrasco.rkt), donde se tienen las funciones principales junto con sus ejemplos. **Se crearon 15/16 funciones en total, 10/10 obligatorias y 5/6 opcionales.**
## Prerrequisitos
Se necesita del programa [Dr.Racket 6.11](https://download.racket-lang.org) o superior para poder ejecutar los códigos del repositorio.
## Como ejecutar
El archivo main trae ejemplos para sus funciones en la parte final. Para ejecutar estos ejemplos, se debe aplicar "run" y luego colocar el "nombre" del ejemplo. Por ejemplo, para el ejemplo: 
```sh
(define gDocs0 (register paradigmadocs (date 26 10 2021) "user1" "pass1"))
```
solo se debe escribir gDocs0 en la consola para poder ejecutar este ejemplo de la función Register.
Se debe clonar el repositorio al computador del corrector para hacer el proceso mas facil:
```sh
git clone https://github.com/PodssilDev/lab1_20537567_Serrano.git
=======
# Laboratorio 1 Paradigmas de Programación: Programación Funcional
**Autor:** John Serrano Carrasco
**Seccion:** 13310-0-A-1
**Fecha de inicio:** 05 de Octubre de 2021
**Fecha de Entrega:** 04 de Noviembre de 2021

## Descripción
Este es el respositorio de mi **laboratorio 1** del curso de Paradigmas de Programación impartido en la Universidad de Santiago. En este laboratorio se ve la programación funcional, a traves del lenguaje de programación Racket o principalmente, Scheme, donde se busca crear una simulación a algo similar a una plataforma estilo Google Docs.
Para ello, se crearon distintos TDAs o Tipos de Datos Abstractos, como por ejemplo, [TDA Paradigmadocs](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDAParadigmadocs_20537567_SerranoCarrasco.rkt) que consiste en el TDA de una plataforma estilo Google Docs, [TDA Documento](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDADocumento_20537567_SerranoCarrasco.rkt) que representa a un documento donde se tiene el autor de este, fecha de creacion, nombre del documento, contenido, entre otras cosas. [TDA User](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDAUser_20537567_SerranoCarrasco.rkt) que representa a un usuario donde se tiene su fecha de registro, su username y su password y [TDA Fecha](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/TDAFecha_20537567_SerranoCarrasco.rkt) donde se tiene una fecha con formato dd mm aaaa. Cada TDA tiene sus funciones, respetando lo visto en clase como lo son constructores, funciones de pertenencia, selectores, modificadores y otras funciones.
Todos estos TDAs son utilizados en el archivo [main](https://github.com/PodssilDev/lab1_20537567_Serrano/blob/a82e99141aed0a830509bf8df123d1f5ccb07634/main_20537567_SerranoCarrasco.rkt), donde se tienen las funciones principales junto con sus ejemplos. **Se crearon 15/16 funciones en total, 10/10 obligatorias y 5/6 opcionales.**
## Prerrequisitos
Se necesita del programa [Dr.Racket 6.11](https://download.racket-lang.org) o superior para poder ejecutar los códigos del repositorio.
## Como ejecutar
El archivo main trae ejemplos para sus funciones en la parte final. Para ejecutar estos ejemplos, se debe aplicar "run" y luego colocar el "nombre" del ejemplo. Por ejemplo, para el ejemplo: 
```sh
(define gDocs0 (register paradigmadocs (date 26 10 2021) "user1" "pass1"))
```
solo se debe escribir gDocs0 en la consola para poder ejecutar este ejemplo de la función Register.
Se debe clonar el repositorio al computador del corrector para hacer el proceso mas facil:
```sh
git clone https://github.com/PodssilDev/lab1_20537567_Serrano.git
>>>>>>> Stashed changes
```