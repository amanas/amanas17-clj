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

project.clj                    - archivo típico de configuración de un projecto Clojure con lein como gestor de dependencias

README.txt                     - archivo con descripción del proyecto e instrucciones para su ejecución

resources/maia/
  - agaricus-lepiota.scm       - dataset de agaricus lepiota
  - bloque3.pdf                - enunciado de la práctica del tema 3
  - datos.txt                  - conjunto de datos para la práctica (ejercicio 1)
  - ejemplos.scm               - ejercicio 1.7 - los mismos ejemplos del archivo de datos, pero con formato scm
  - ejemplos2.scm              - extensión de los ejemplos anteriores con otros 5
  - ionosphere.scm             - dataset de ionosphere
  - ionosphere.trazas          - trazas del procesado del dataset ionosphere con el algoritmo EGS
  - langley-cap*.pdf           - material de estudio
  - practica-tema1-144696.pdf  - enunciado de la práctica del tema 1

src/amanas17/maia/
  - p1_1.clj                   - práctica 1 - ejercicios 2 a 6
  - p1_3.clj                   - práctica 1 - ejercicios 8 a 11
  - p1_4.clj                   - práctica 1 - ejercicios 12 a 14
  - p1_5.clj                   - práctica 1 - ejercicios 15 a 19 - Primeros algoritmos de aprendizaje
  - p1_6.clj                   - práctica 1 - ejercicios 22 a 24 - Evaluación del aprendizaje
  - p1_9.clj                   - práctica 1 - ejercicios 20 a 21 - Algoritmos incrementales (OPCIONAL)
  - p2_1.clj                   - práctica 2 - ejercicios 1 a 5   - Representación CL (introducción)
  - p2_2.clj                   - práctica 2 - ejercicios 6 a 8   - Representaciones CL (comparacioes)
  - p2_3.clj                   - práctica 2 - ejercicios 9 a 14  - Representaciones CL (especializaciones/generalizaciones)
  - p2_4.clj                   - práctica 2 - ejercicios 15 a 16 - Inducción exaustiva de conceptos CL
  - p2_5.clj                   - práctica 2 - ejercicios 17 a 19 - Inducción heurística de conceptos CL
  - p2_6.clj                   - práctica 2 - ejercicios 20 a 22 - Representación de conceptos TC (OPTATIVA)
  - p2_7.clj                   - práctica 2 - ejercicios 23 a 26 - Representación de conceptos UU
  - p2_9_0.clj                 - práctica 2 - ejercicios 27 a 33 - Inducción de conceptos UU y LUU
  - p2_9_1.clj                 - práctica 2 - ejercicios 34 a 36 - Conceptos IB
  - p2_9_2.clj                 - práctica 2 - ejercicios 37 a 42 - Conceptos NB
  - p3_0.clj                   - práctica 3 - ejercicios 1 a 1   - Representación de conceptos LD
  - p3_1.clj                   - práctica 3 - ejercicios 2 a 6   - Inducción de conceptos LD
  - p3_2.clj                   - práctica 3 - ejercicios 7 a 10  - Representación de conceptos JC
  - p3_3.clj                   - práctica 3 - ejercicios 11 a 17 - Induccion conceptos JC-adc mediante DDT
  - p3_4.clj                   - práctica 3 - ejercicios 18 a 21 - Transformacion conceptos JC -> DL

test/amanas17/maia/
  - p1_1_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p1_1.clj
  - p1_3_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p1_3.clj
  - p1_4_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p1_4.clj
  - p1_5_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p1_5.clj
  - p1_6_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p1_6.clj
  - p1_9_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p1_9.clj
  - p2_1_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p2_1.clj
  - p2_2_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p2_2.clj
  - p2_3_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p2_3.clj
  - p2_4_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p2_4.clj
  - p2_7_test.clj              - pruebas unitarias de las funciones de src/amanas17/maia/p2_7.clj
  - p2_9_0_test.clj            - pruebas unitarias de las funciones de src/amanas17/maia/p2_9_0.clj





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
