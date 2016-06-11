(ns amanas17.maia.p2-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-6]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]
        [clojure.pprint]))

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
                 (when (<= (count-matchings S NSET) H-match)
                   S))
               (rest NSET))
         (apply concat)
         (remove nil?)
         (remove (partial = H))
         distinct
         ;; No consigo que la computación acabe a menos que limite el número
         ;; de ejemplos del que construimos hipótesis en ese paso
         shuffle
         (take 50))))

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
   Asume que el primer elemento de PSET y NSET son los metadatos
   Los conjuntos ionosphere y agaricus-lepiota tienen muchos ejemplos,
   por lo que para que el algoritmo acabe sin desbordarse tengo que tomar
   samples"
  ([PSET NSET CSET HSET]
   (EGS0 PSET NSET CSET HSET 100))
  ([PSET NSET CSET HSET sample-size]
   (let [meta (first PSET)
         PSET (cons meta (take sample-size (shuffle (rest PSET))))
         NSET (cons meta (take sample-size (shuffle (rest NSET))))
         CSET (atom (set CSET))
         HSET (atom (set HSET))]
     (prn "First  [CSET,HSET]=" [(count @CSET) (count @HSET)])
     (doall (pmap (fn [H]
                    (when-not (match-all? H PSET)
                      (swap! HSET without H))
                    (when-not (match-any? H NSET)
                      (do (swap! HSET without H)
                          (swap! CSET conj H))))
                  @HSET))
     (prn "Second [CSET,HSET]=" [(count @CSET) (count @HSET)])
     (if (empty? @HSET)
       (vec @CSET)
       (let [NEWSET (atom #{})]
         (doall (pmap (fn [H]
                        (doall (pmap (fn [S]
                                       (when (none-more-general? @CSET S)
                                         (swap! NEWSET conj S)))
                                     (specs-matching<=H H NSET))))
                      @HSET))
         (EGS0 PSET NSET @CSET @NEWSET))))))

(defn EGS
  "Devuelve un concepto al azar del resultado de aplicar EGS0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
        ej- (->> ejemplos rest (filter (comp (partial = '-) last)))
        egs0 (EGS0 (cons meta ej+) (cons meta ej-) [] [(concepto-CL-mas-general meta)])]
    (prn "Total" (count egs0))
    (pprint egs0)
    (obtener-al-azar egs0)))

;; He realizad una llamado a EGS con el conjunto de ejemplos habitual
(comment (time (EGS all-ejemplos)))

;; El resultado ha sido:
;; [[soleado] [-inf 35] [-inf 70] (*) (*) (*) (*)]
;; Previamente, el concepto que yo tenía definido como
;; buen día para salir el campo era:
;; '((soleado) (20 30) (50 55) (no) (contento) (*) (solvente)))

;; Observo que:
;; 1. Mi concepto de buen día para salir está incorporado en el concepto
;;    que devuelve el algoritmo EGS
;; 2. Sin embargo, el concepto que devuelve el algoritmo EGS es muy genérico,
;;    no concreta casi nada ya que deja muchos atributos completamente
;;    opcionales.

;; Efectivamente, tal como se indica en el material de estudio, en este
;; ejemplo práctico se muestra que el resultado del algoritmo EGS tiende
;; a devolver conceptos que describen los ejemplos de forma genérica.

(he-tardado 300 2.15)


;; Ejercicio 2.16

;; En la ejecución del algoritmo EGS en los archivos
;; de ejemplo del material, para no acabar en una StackOverflowException,
;; tengo que cambiar los parámetros creación de la
;; máquina virtual dónde corre Clojure, aumentando:
;;  1. Xmx a 2 GB
;;  2. Xss a 64 MB (un montón, sí, pero así nos curamos en salud)
;; Es decir, el tamaño de las listas que se generan en los pasos intermedios
;; del proceso es enorme.


;; Cargamos los ejemplos de ionosfera
(def ionosphere (leer-ejemplos "resources/maia/ionosphere.scm"))

;; y lanzamos el algoritmo:
(comment (time (EGS ionosphere)))

;; El resultado para los datos del archivo ionosphere es el concepto:
;;[(*) [0 +inf] [0.02337 +inf] [-1 +inf] [-1 +inf] [-1 +inf] (*) [-1 +inf]
;; (*) [-1 +inf] (*) (*) (*) (*) (*) [-0.97515 +inf] (*) [-1 +inf] (*) (*)
;; (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*)]

;; El algoritmo ha tardado:
;; 94 segundos

;; Las trazas que ha dejado son:
;; "First  [CSET,HSET]=" [0 1]
;; "Second [CSET,HSET]=" [0 1]
;; "First  [CSET,HSET]=" [0 50]
;; "Second [CSET,HSET]=" [6 1]
;; "First  [CSET,HSET]=" [6 50]
;; "Second [CSET,HSET]=" [17 1]
;; "First  [CSET,HSET]=" [17 50]
;; "Second [CSET,HSET]=" [20 2]
;; "First  [CSET,HSET]=" [20 100]
;; "Second [CSET,HSET]=" [38 3]
;; "First  [CSET,HSET]=" [38 144]
;; "Second [CSET,HSET]=" [84 5]
;; "First  [CSET,HSET]=" [84 248]
;; "Second [CSET,HSET]=" [170 10]
;; "First  [CSET,HSET]=" [170 449]
;; "Second [CSET,HSET]=" [290 15]
;; "First  [CSET,HSET]=" [290 691]
;; "Second [CSET,HSET]=" [385 20]
;; "First  [CSET,HSET]=" [385 949]
;; "Second [CSET,HSET]=" [542 21]
;; "First  [CSET,HSET]=" [542 1024]
;; "Second [CSET,HSET]=" [634 22]
;; "First  [CSET,HSET]=" [634 1081]
;; "Second [CSET,HSET]=" [781 0]
;; "Total" 781


;; Cargamos los ejemplos agaricus-lepiota
(def agaricus-lepiota (leer-ejemplos "resources/maia/agaricus-lepiota.scm"))

;; y lanzamos el algoritmo:
(comment (time (EGS agaricus-lepiota)))

;; No consigo que el algoritmo acabe nunca
