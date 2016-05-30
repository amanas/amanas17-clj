(ns amanas17.maia.p2-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]))

(defn without
  "Elimina un elemento de una colección"
  [coll x]
  (remove (partial = x) coll))

;; Abuso de pmap (pararell map) para optimizar el rendimiento
;; en máquinas con varios procesadores

;; Cacheo además la función que devuelve la intension de un conjunto
;; de ejemplos para optimizar el rendimiento

(def memo-int (memoize (fn [SET] (map butlast (rest SET)))))

(defn match-all?
  "Comprueba si H satisface todos los ejemplos de PSET"
  [H PSET]
  (every? true? (pmap (partial match-CL H) (memo-int PSET))))

(defn match-any?
  "Comprueba si H satisface algún ejemplo de NSET"
  [H NSET]
  (some true? (pmap (partial match-CL H) (memo-int NSET))))

(defn count-matchings
  "Cuenta los ejemplos que satisface un concepto"
  [H NSET]
  (count (filter true? (pmap (partial match-CL H) (memo-int NSET)))))

(defn specs-matching<=H
  "Devuelve las especializaciones inmediatas en un atributo de H
   que satisfacen igual o menos ejemplos negativos que H"
  [H NSET]
  (let [meta (first NSET)
        H-match (count-matchings H NSET)]
    (->> (pmap #(for [S (especializaciones-CL H meta %)]
                 (when (<= (count-matchings S NSET) H-match) S))
               (rest NSET))
         (apply concat)
         (remove nil?)
         (remove (partial = H))
         distinct)))

(defn none-more-general?
  "Devuelve true si ningún concepto de CSET es más general que S"
  [CSET S]
  (every? (partial <= 0) (pmap #(cmp-concepto-CL % S) CSET)))

;; Ejercicio 2.15

(defn EGS0
  "Algoritmo de búsqueda exaustivo EGS, de general a específico, de inducción
   de conjunciones lógicas.
   Devuelve el conjunto de conjunciones lógicas más generales consistentes
   con los datos de entrenamiento.
   Asume que el primer elemento de PSET y NSET son los metadatos"
  [PSET NSET CSET HSET]
  (let [DUMMY (vec HSET)
        CSET (atom (set CSET))
        HSET (atom (set HSET))]
    (prn "First  [CSET,HSET]=" [(count @CSET) (count @HSET)])
    (doall (pmap (fn [H]
                   (when-not (match-all? H PSET)
                     (swap! HSET without H))
                   (when-not (match-any? H NSET)
                     (do (swap! HSET without H)
                         (swap! CSET conj H))))
                 DUMMY))
    (prn "Second [CSET,HSET]=" [(count @CSET) (count @HSET)])
    (if (empty? @HSET) (vec @CSET)
                       (let [DUMMY (vec @HSET)
                             NEWSET (atom #{})]
                         (doall (pmap (fn [H]
                                        (doall (pmap (fn [S]
                                                       (when (none-more-general? @CSET S)
                                                         (swap! NEWSET conj S)))
                                                     (specs-matching<=H H NSET))))
                                      DUMMY))
                         (EGS0 PSET NSET @CSET @NEWSET)))))

(defn EGS
  "Devuelve un concepto al azar del resultado de aplicar EGS0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
        ej- (->> ejemplos rest (filter (comp (partial = '-) last)))
        egs0 (EGS0 (cons meta ej+) (cons meta ej-) [] [(concepto-CL-mas-general meta)])]
    ;; (clojure.pprint/pprint egs0)
    (prn "Total" (count egs0))
    (obtener-al-azar egs0)))

;; He realizad una llamado a EGS con el conjunto de ejemplos habitual
(comment (time (EGS ejemplos)))
;; El resultado ha sido:
;; [[**] [27 35] [65 95] [**] [contento] [**] [**]]
;; Previamente, el concepto que yo tenía definido como
;; buen día para salir el campo era:
;; [[soleado] [20 30] [60 80] [no] [contento] [**] [solvente]]
;; Observo que:
;; 1. Mi concepto de buen día para salir está incorporado en el concepto
;;    que devuelve el algoritmo EGS
;; 2. Sin embargo, el concepto que devuelve el algoritmo EGS es muy genérico,
;;    no concreta casi nada ya que deja muchos atributos completamente
;;    opcionales.
;; Efectivamente, tal como se indica en el material de estudio, en este
;; ejemplo práctico se muestra que el resultado del algoritmo EGS tiende
;; a devolver conceptos que describen los ejemplos de forma genérica.

;; (he-tardado 300 2.15)


;; Ejercicio 2.16

;; En la ejecución del algoritmo EGS en los archivos
;; de ejemplo del material, para no acabar en una StackOverflowException,
;; tengo que cambiar los parámetros creación de la
;; máquina virtual dónde corre Clojure, aumentando:
;;  1. Xmx a 2 GB
;;  2. Xss a 64 MB (un montón, sí, pero así nos curamos en salud)
;; Es decir, el tamaño de las listas que se generan en los pasos intermedios
;; del proceso es enorme.


;; Cargamos los ejemplos de ionosfera y agaricus-lepiota
(def ionosphere (leer-ejemplos "resources/maia/ionosphere.scm"))
(comment (time (EGS ionosphere)))
(def agaricus-lepiota (leer-ejemplos "resources/maia/agaricus-lepiota.scm"))
(comment (time (EGS agaricus-lepiota)))

;; Pese a lo anterior y cientos de mejoras con las que he probado, principalmente
;; de cacheo de comprobaciones intermedias, no he podido acabar con éxito la ejecución de EGS
;; en los archivos indicados, puesto que el proceso parece no acabar nunca. LLega un
;; momento en el que tengo la sensación de que el ordenador se va a quemar.
