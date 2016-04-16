(ns amanas17.maia.p2-9-0
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-4]
        [amanas17.maia.p2-6]
        [amanas17.maia.p2-7]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inducción de conceptos UU ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 2.27
(defn score-TC
  [concepto-TC PSET NSET]
  (let [Pc (->> PSET memo-int (filter (partial match-TC concepto-TC)) count)
        Nc (->> NSET memo-int (remove (partial match-TC concepto-TC)) count)]
    (/ (+ Pc Nc) (+ (dec (count PSET)) (dec (count NSET))))))

(he-tardado 30 2.27)

(defn sort-by-scoreTC-desc
  [PSET NSET conceptos]
  (->> conceptos
       (pmap (fn [C] [C (score-TC C PSET NSET)]))
       (sort-by second)
       (map first)
       reverse))

(defn naive-cmp-concepto-TC
  "Compara dos conceptos TC. Devuelve:
   1 si TC1 es más general que TC2
  -1 si TC1 es más específico que TC2
   0 en otro caso"
  [[u1 & CL1 :as TC1] [u2 & CL2 :as TC2]]
  (cond (> u1 u2) -1
        (< u1 u2)  1
        :else      (cmp-concepto-CL CL1 CL2)))

;; Ejercicio 2.28
(defn HTC0
  "Algoritmo de búsqueda heurística HTC, de general a específico,
   dirigido por una función de evaluación.
   Devuelve el concepto que satisface la mayoría de los ejemplos
   positivos y excluye la mayoría de los negativos.
   Asume que los conjuntos PSET y NSET tienen cabecera de atributos
   Sobrecargo la función para que admita como parámetro el beam-size"
  ([PSET NSET CLOSED-SET HSET]
   (HTC0 PSET NSET CLOSED-SET HSET 10))
  ([PSET NSET CLOSED-SET HSET beam-size]
   (let [CLOSED-SET (atom (set CLOSED-SET))
         OPEN-SET   (atom #{})]
     (prn "1 - HTC0 [CLOSED-SET,HSET]=" [(count @CLOSED-SET) (count HSET)])
     (doall (pmap (fn [H] (let [NEW-SET (atom #{})]
                           (doall (pmap (fn [S] (when (> (score-TC S PSET NSET) (score-TC H PSET NSET))
                                                 (swap! NEW-SET conj S)))
                                        (->> (rest NSET)
                                             (pmap (partial especializaciones-TC H (first NSET)))
                                             (apply concat)
                                             (remove (partial = H))
                                             distinct)))
                           (if (empty? @NEW-SET)
                             (swap! CLOSED-SET conj H)
                             (doall (pmap (fn [S] (swap! OPEN-SET conj S)
                                            (doall (pmap (fn [C] (when  (<= (naive-cmp-concepto-TC S C) 0)
                                                                  (if (> (score-TC C PSET NSET)
                                                                         (score-TC S PSET NSET))
                                                                    (swap! OPEN-SET   without S)
                                                                    (swap! CLOSED-SET without C))))
                                                         @CLOSED-SET)))
                                          @NEW-SET)))))
                  HSET))
     (prn "2 - HTC0 [CLOSED-SET,OPEN-SET]=" [(count @CLOSED-SET) (count @OPEN-SET)])
     (if (empty? @OPEN-SET)
       (let [result (->> @CLOSED-SET (sort-by-scoreTC-desc PSET NSET))]
         (first result))
       (let [BEST-SET (->> @OPEN-SET (into @CLOSED-SET) (sort-by-scoreTC-desc PSET NSET)
                           (take beam-size) set)
             CLOSED-SET (filter BEST-SET @CLOSED-SET)
             OPEN-SET   (filter BEST-SET @OPEN-SET)]
         (HTC0 PSET NSET CLOSED-SET OPEN-SET beam-size))))))


(defn HTC
  "Devuelve un concepto al azar del resultado de aplicar HTC0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  ([ejemplos]
   (HTC ejemplos 10))
  ([ejemplos beam-size]
   (let [meta (first ejemplos)
         ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
         ej- (->> ejemplos rest (filter (comp (partial = '-) last)))
         htc0 (HTC0 (cons meta ej+) (cons meta ej-)
                    [] [(cons 0 (concepto-CL-mas-general meta))] beam-size)]
     htc0)))

(comment (time (prn (HTC ejemplos 1000))))
;; Compare el concepto TC devuelto por HTC con el concepto CL obtenido por HGS
;; y el concepto CL del ejercicio 3.
;; Mi buen concepto:        [  [soleado] [20 30]   [60 80]   [no] [contento] [**] [solvente]]
;; El concepto con HGS es:  [  [**]      [22 40]   [79 +inf] [**] [contento] [**] [**]]
;; El concepto con HTC es:  [7 [**]      [18 37]   [79 +inf] [**] [contento] [**] [**]]

;; El concepto devuelto por HTC es perfectamente coherente con el concepto que yo había
;; definido como buen día para salir el campo
;; De hecho, los tres conceptos anteriores parecen bastante coherentes entre sí

;; El tiempo empleado por el algorítmo para procesar los ejemplos de prueba ha sido de
;; 24 segundos con 7 iteraciones


;; Ejercicio 2.29
;; Ejecute HTC mediante el conjunto de ejemplos agaricuslepiota. scm e ionosphere.scm
;; (ver repositorio de material ejemplos UCI).
;; Observe la traza del algoritmo y describa su comportamiento en presencia
;; de estos nuevos ejemplos. Tambien compare el comportamiento de HTC con el de HGS.


(comment (prn (HTC ionosphere 1)))
;; Según HTC, el concepto que mejor describe el dataset ionosphere es:
;; ...
;; El tiempo empleado por el algorítmo para procesar el dataset ionosphere ha sido de
;; ... segundos con ... iteraciones (muy elevado, teniendo en cuenta que hemos utilizado un
;; beam-size de 1)

(comment (prn (HTC agaricus-lepiota 1)))
;; Según HTC, el concepto que mejor describe el dataset agaricus-lepiota es:
;; ...
;; El tiempo empleado por el algorítmo para procesar el dataset agaricus-lepiota ha sido de
;; ... segundos con ... iteraciones (muy elevado, teniendo en cuenta que hemos utilizado un
;; beam-size de 1)


(time (prn (HTC ejemplos 1000)))
