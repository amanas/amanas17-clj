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

(defn count-matchings
  "Cuenta los ejemplos que incorpora el concepto"
  [concepto ejemplos]
  (->> ejemplos (map butlast) (map (partial match-CL concepto)) (filter true?) count))

(defn specs-matching-less-than
  "Devuelve las especializaciones inmediatas del concepto h que satisfacen menos
   ejemplos que el propio h"
  [h ejemplos meta]
  (let [h-matching (count-matchings h ejemplos)
        filter-fn  (fn [s] (< (count-matchings s ejemplos) h-matching))
        specs (->> ejemplos
                   (mapcat (partial especializaciones-CL h meta)))
        filtered (filter filter-fn specs)]
    (prn "count specs" (count specs))
    (prn "count distinct specs" (count (distinct specs)))
    (prn "count filter specs" (count filtered))
    (prn "count distinct filter specs" (count (distinct filtered)))
    filtered))

(defn none-more-general?
  "Devuelve true ningún concepto de conceptos es más general que el concepto"
  [conceptos concepto]
  (->> conceptos
       (map (fn [c] (cmp-concepto-CL c concepto)))
       (filter pos?)
       empty?))

;; Ejercicio 15
(defn EGS0
  "Algoritmo de búsqueda exaustivo EGS, de general a específico, de inducción
   de conjunciones lógicas.
   Asume que el primer elemento de PSET y NSET son los metadatos"
  [PSET NSET CSET HSET]
  (prn "Main [CSET,HSET]="[(count CSET) (count HSET)])
  (let [meta (first PSET)
        pSET (rest PSET)
        pSEText (map butlast pSET)
        nSET (rest NSET)
        nSEText (map butlast nSET)
        [CSET HSET] (loop [[h & more] HSET
                           cSET CSET
                           hSET HSET]
                      (prn "First loop [cSET,hSET]="[(count cSET) (count hSET)])
                      (if (nil? h) [cSET hSET]
                          (let [addToC? (not-any? (partial match-CL h) nSEText)
                                delFrH? (or addToC?
                                            (not-every? (partial match-CL h) pSEText))
                                newCSET (if addToC? (conj    cSET h) cSET)
                                newHSET (if delFrH? (without hSET h) hSET)]
                            (recur more newCSET newHSET))))]
    (if (empty? HSET) CSET
        (let [NEWSET
              (loop [[h & more :as hSET] HSET
                     newSET []]
                (prn "Second loop [hSET,newSET]="[(count hSET) (count newSET)])
                (if (nil? h) newSET
                    (recur more (->> (specs-matching-less-than h nSET meta)
                                     (filter (partial none-more-general? CSET))
                                     (concat newSET)
                                     distinct))))]
          (EGS0 PSET NSET CSET NEWSET)))))

(defn EGS
  "Devuelve un concepto al azar del resultado de aplicar EGS0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ejemplos+ (remove (fn [e] (= '- (last e))) (rest ejemplos))
        ejemplos- (remove (fn [e] (= '+ (last e))) (rest ejemplos))
        egs0 (EGS0 (cons meta ejemplos+)
                   (cons meta ejemplos-)
                   []
                   [(concepto-CL-mas-general meta)])]
    (prn egs0)
    (obtener-al-azar egs0)))

;; He realizad una llamado a EGS con el conjunto de ejemplos habitual
(comment (EGS ejemplos))

;; El resultado ha sido:
;; [[**] [15 +inf] [**] [**] (contento) [**] [**]]
;; Previamente, el concepto que yo tenía definido como
;; buen día para salir el campo era:
;; [['soleado] [20 30] [60 80] [si no] [contento] [**] [solvente]]
;; Observo que:
;; 1. Mi concepto de buen día para salir está incorporado en el concepto
;;    que devuelve el algoritmo EGS
;; 2. Sin embargo, el concepto que devuelve el algoritmo EGS es muy genérico,
;;    no concreta casi nada ya que deja muchos atributos completamente
;;    opcionales.
;; Efectivamente, tal como se indica en el material de estudio, en este
;; ejemplo practico se muestra que el resultado del algoritmo EGS tiende
;; a ser el concepto más genérico posible que describe los ejemplos.

(he-tardado 300 2.15)

;; Ejercicio 16

;; Cargamos los ejemplos de ionosfera
(def ionosphere (leer-ejemplos "resources/maia/ionosphere.scm"))
;; Y lanzamos el algoritmo - lo dejo comentado para que no se lance más
;; por accidente.
(comment (time (EGS ionosphere)))
;; El resultado de los logs arrojados por esta ejecución está en
;; resources/maia/ionosphere.debug
;; Ha tardado 72 segundos y observo que el NEWSET es generado con 3262
;; nuevas hipótesis a comprobar.
;; Efectivamente, en la función  specs-matching-less-than,
;; para cada ejemplo negativo (y tenemos 126)
(comment (->> ionosphere
              rest
              (map last)
              (group-by identity)
              (#(get % '-))
              count))
;; se pueden crear hasta 2 * 34 especializaciones (pues tenemos 34
;; atributos). Es decir, un máximo de 8568. En realidad solo se acaban considerando
;; 3262, pues muchas de ellas están repetidas en su creación.
;; Y para cada una de ellas, hay que contar cuantos ejemplos
;; positivos cubren. Es decir, el coste computacional de este algoritmo
;; cuando los ejemplos y los atributos son grandes es elevádisimo.


;; Similares reflexiones se aplican al caso
(def agaricus-lepiota (leer-ejemplos "resources/maia/agaricus-lepiota.scm"))
(comment (time (EGS agaricus-lepiota)))
;; En este caso, el proceso tarda:
;; TODO: acabar esto
