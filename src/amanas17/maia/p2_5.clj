(ns amanas17.maia.p2-5
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]
        [amanas17.maia.p2-4]))

;; Ejercicio 17
(defn score-CL
  [concepto-CL pSET nSET]
  (let [Pc (->> pSET (map (partial match-CL concepto-CL)) (filter true?) count)
        Nc (->> nSET (map (partial match-CL concepto-CL)) (remove true?) count)]
    (/ (+ Pc Nc) (+ (count pSET) (count nSET)))))


;; Ejercicio 18
(def beam-size 5)

(defn sort-by-score-desc
  [conceptos pSET nSET]
  (reverse (sort-by (fn [c] (score-CL c pSET nSET)) conceptos)))

(defn HGS0
  "Algoritmo de búsqueda heurística HGS, de general a específico,
   no incremental de conjunciones lógicas dirigido por una función de evaluación.
   Devuelve una concjunción lógica que satisface la mayoría de los ejemplos
   positivos y no satisface la mayoría de los negativos.
   Asume que los conjuntos PSET y NSET tienen cabecera de atributos"
  [PSET NSET CLOSED-SET HSET]
  (let [meta (first PSET)
        pSET (rest PSET)
        pSEText (map butlast pSET)
        nSET (rest NSET)
        nSEText (map butlast nSET)
        CLOSED-SET (atom CLOSED-SET)
        OPEN-SET   (atom [])
        NEW-SET    (atom [])]
    (doall (for [H HSET]
             (do (prn "H" H)
                 (doall (for [S (mapcat (partial especializaciones-CL H meta) nSET)]
                          (when (> (score-CL S pSEText nSEText) (score-CL H pSEText nSEText))
                            (swap! NEW-SET conj S))))
                 (pr "NEW-SET count" (count @NEW-SET))
                 (if (empty? @NEW-SET)
                   (swap! CLOSED-SET conj H)
                   (doall (for [S @NEW-SET]
                            (do (prn "S" S)
                                (swap! OPEN-SET conj S)
                                (doall (for [C @CLOSED-SET]
                                         (when (concepto-CL>= C S)
                                           (if (> (score-CL C pSEText nSEText) (score-CL S pSEText nSEText))
                                             (swap! OPEN-SET   without S)
                                             (swap! CLOSED-SET without C))))))))))))
    (pr "OPEN-SET count" (count @OPEN-SET))
    (if (empty? @OPEN-SET)
      (first (sort-by-score-desc @CLOSED-SET pSEText nSEText))
      (let [BEST-SET (take beam-size
                           (sort-by-score-desc (distinct (concat @CLOSED-SET @OPEN-SET))
                                               PSET nSET))
            CLOSED-SET (filter (set BEST-SET) @CLOSED-SET)
            OPEN-SET   (filter (set BEST-SET) @OPEN-SET)]
        (HGS0 PSET NSET CLOSED-SET OPEN-SET)))))


(defn HGS
  "Devuelve un concepto al azar del resultado de aplicar HGS0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ejemplos+ (remove (fn [e] (= '- (last e))) (rest ejemplos))
        ejemplos- (remove (fn [e] (= '+ (last e))) (rest ejemplos))
        hgs0 (HGS0 (cons meta ejemplos+)
                   (cons meta ejemplos-)
                   []
                   [(concepto-CL-mas-general meta)])]
    hgs0))

(HGS ejemplos)
