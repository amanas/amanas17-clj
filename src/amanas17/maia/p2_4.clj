(ns amanas17.maia.p2-4
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]))

(defn without
  "Removes an element from a collection"
  [coll x]
  (remove (partial = x) coll))

(defn match-all?
  "Comprueba si H satisface todos los ejemplos de PSET.
   Asume que PSET tiene cabecera de atributos"
  [H pSEText]
  (every? (partial match-CL H) pSEText))

(defn match-any?
  "Comprueba si H satisface algún ejemplo de NSET.
   Asume que NSET tiene cabecera de atributos"
  [H nSEText]
  (some (partial match-CL H) nSEText))

(defn count-matchings
  "Cuenta los ejemplos que satisface un concepto"
  [H nSEText]
  (->> nSEText (filter (partial match-CL H)) count))

;; Abuso de pmap (pararell map) para sacar rendimiento a los procesadores
;; multicore
(defn specs-matching<=H
  "Devuelve las especializaciones inmediatas en un atributo de H
   que satisfacen igual o menos ejemplos negativos que H"
  [H NSET]
  (let [meta (first NSET)
        nSET (rest NSET)
        nSEText (map butlast nSET)
        H-match (count-matchings H nSEText)
        specs (->> nSET (pmap (partial especializaciones-CL H meta))
                   (apply concat) distinct (remove (partial = H)))
        filter-fn  (fn [S] (when (< (count-matchings S nSEText) H-match) S))]
    (remove nil? (pmap filter-fn specs))))

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
  (let [CSET (atom (set CSET))
        HSET (atom (set HSET))
        pSEText (map butlast (rest PSET))
        nSEText (map butlast (rest NSET))]
    (prn "First  [CSET,HSET]=" [(count @CSET) (count @HSET)])
    (loop [[H & more] (seq @HSET)]
      (when H (do (when-not (match-all? H pSEText)
                    (swap! HSET without H))
                  (when-not (match-any? H nSEText)
                    (do (swap! HSET without H)
                        (swap! CSET conj H)))
                  (recur more))))
    (prn "Second [CSET,HSET]=" [(count @CSET) (count @HSET)])
    (if (empty? @HSET) (vec @CSET)
        (let [NEWSET (atom #{})]
          (loop [[H & more] (seq @HSET)]
            (when H (do (loop [[S & more] (specs-matching<=H H NSET)]
                          (when S (do (if (none-more-general? @CSET S)
                                        (swap! NEWSET conj S))
                                      (recur more))))
                        (recur more))))
          (EGS0 PSET NSET @CSET @NEWSET)))))

(defn EGS
  "Devuelve un concepto al azar del resultado de aplicar EGS0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
        ej- (->> ejemplos rest (filter (comp (partial = '-) last)))
        egs0 (EGS0 (cons meta ej+) (cons meta ej-) [] [(concepto-CL-mas-general meta)])]
    (clojure.pprint/pprint egs0)
    (prn "Total" (count egs0))
    (obtener-al-azar egs0)))

;; He realizad una llamado a EGS con el conjunto de ejemplos habitual
(comment
  (time (EGS ejemplos)))
;; El resultado ha sido:
;; [[**] [27 35] [65 95] [**] [contento] [**] [**]]
;; Previamente, el concepto que yo tenía definido como
;; buen día para salir el campo era:
;; [[soleado] [20 30] [60 80] [si no] [contento] [**] [solvente]]
;; Observo que:
;; 1. Mi concepto de buen día para salir está incorporado en el concepto
;;    que devuelve el algoritmo EGS
;; 2. Sin embargo, el concepto que devuelve el algoritmo EGS es muy genérico,
;;    no concreta casi nada ya que deja muchos atributos completamente
;;    opcionales.
;; Efectivamente, tal como se indica en el material de estudio, en este
;; ejemplo practico se muestra que el resultado del algoritmo EGS tiende
;; a devolver conceptos que describen los ejemplos de forma genérica.

;; (he-tardado 300 2.15)


;; Ejercicio 2.16

;; Cargamos los ejemplos de ionosfera y agaricus-lepiota
(def ionosphere (leer-ejemplos "resources/maia/ionosphere.scm"))
(comment (time (EGS ionosphere)))
(def agaricus-lepiota (leer-ejemplos "resources/maia/agaricus-lepiota.scm"))
(comment (time (EGS agaricus-lepiota)))

;; No he podido acabar con éxito la búsqueda en los archivos indicados
;; el el material porque siempre acabo en una StackOverflow Exception
