
Introduccion
------------

El presente proyecto contiene los ejercicios correspondientes a la primera práctica
de la asignatura "Métodos de Aprendizaje en Inteligencia Artificial"
del máster "Máster Universitario en I.A. Avanzada: fundamentos, métodos y aplicaciones"

El lenguaje de programación en el que se realizan estos ejercicios es Clojure, un
dialecto de Lisp que puede ejecutarse sobre la máquina virtual java o .Net. Existen
también intérpretes de Clojure para javascript.



Estructura de directorios de este proyecto
------------------------------------------

resources/maia/
  - datos.txt                  - conjunto de datos para la práctica (ejercicio 1)
  - ejemplos.scm               - los mismos ejemplos del archivo de datos, pero formateados
                                 de modo que Clojure los puede interpretar fácilmente
                                 (ejercicio 7)
  - ejemplos2.scm              - extensión de los ejemplos anteriores con otros 5
  - practica-tema1-144696.pdf  - documento con la descripción de la práctica
                                 dada por el equipo docente
src/amanas17/maia/
  - p1_1.clj                   - ejercicios correspondientes al bloque 1 de la práctica 1
                                 (ejercicios 2 a 6)
  - p1_3.clj                   - ejercicios correspondientes al bloque 3 de la práctica 1
                                 (ejercicios 8 a 11)
  - p1_4.clj                   - ejercicios correspondientes al bloque 4 de la práctica 1
                                 (ejercicios 12 a 14)
  - p1_5.clj                   - ejercicios correspondientes al bloque 5 de la práctica 1
                                 (ejercicios 15 a 19)
  - p1_6.clj                   - ejercicios correspondientes al bloque 6 de la práctica 1
                                 (ejercicios 22 a 24)
  - p1_9.clj                   - ejercicios correspondientes al bloque 1 de la práctica 1
                                 (ejercicios opcionales 20 a 21)
project.clj                    - archivo típico de configuración de un projecto Clojure
                                 con lein como gestor de dependencias
README.txt                     - archivo con descripción del proyecto e instrucciones para
                                 su ejecución




Instrucciones para ejecutar el código de los ejemplos
-----------------------------------------------------

Hay bastantes formas de ejecutar código Clojure.
Seguramente, lo más sencillo es utilizar un IDE que además de ejecutar el código nos
permite explorar el código de una forma muy hergonómica.
Dependiendo del entorno de trabajo de la persona que explora este proyecto y de sus
preferencias, se pueden elegir entre las siguientes opciones:

1. Eclipse con CounterClockWise (http://doc.ccw-ide.org)
   Escasamente desarrollado

2. Emacs, en mi opnición el mejor plugin es emacs live (http://overtone.github.io/emacs-live)
   No es muy recomendable para alguien que no tenga manejo con emacs porque este IDE realiza
   cierto abuso de combinaciones de teclas que pueden resultar difíciles al principio

3. IntellijIDEA con el plugin cursive (https://cursive-ide.com/userguide/)
   Muy recomendable, muy cómodo y robusto

4. LightTable (http://lighttable.com/)
   Creo que para iniciarse en Clojure, este es el IDE más sencillo y directo. No conlleva
   complicaciones de ningún tipo - sólo descargar, abrir proyecto y ejecutar.
   En el siquiente punto detallo mejor como usar LightTable para ejecutar el código contenido
   en este proyecto

5. ... algunos otros ...




Ejecutar el código de los ejemplos con LightTable
-------------------------------------------------

1. Descargue LightTable desde http://lighttable.com/
2. Descomprima el archivo descargado en un lugar de su preferencia y arranque el IDE - en entorno
   Windows esto se hace simplemente clickando dos veces en LightTable.exe
3. Una vez arrancado el IDE, haga:
   File > Open Folder
   Seleccione la carpeta en donde esté este proyecto
4. En el navegador de la izquierda, clickar en la estructura de directorios hasta que se abra el
   archivo src/amanas17/maia/p1_1.clj en el editor LightTable
5. Para ejecutar código Clojure de modo interactivo, primero necesitamos instanciar un REPL.
   Para ello, simplemente, en el archivo que tiene abierto, posicione el cursor justos después de:
   (ns amanas17.maia.p1-1)
   y pulse Ctrl+Enter.
   Después de esperar unos segundos, una nueva instancia del REPL clojure está abierta y a nuestra
   disposición.
6. Podemos seguir ejecutando bloques de código posicionando el cursor justo detrás del bloque
   y pulsando Ctrl+Enter.
7. O mejor aún, podemos evaluar el fichero de una vez en el REPL seleccionándolo por completo
   (Ctrl+A) y enviándolo al REPL (Ctrl+Enter) - LE RECOMIENDO ESTA OPCIÓN




















