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
  [[u1 & CL1 :as TC1] [u2 & CL2 :as TC2]]
  (cond (> u1 u2) -1
        (< u1 u2) 1
        :else     (cmp-concepto-CL CL1 CL2)))

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
     (prn "First  [CLOSED-SET,HSET]=" [(count @CLOSED-SET) (count HSET)])
     (doall
      (map (fn [H]
              (let [NEW-SET (atom #{})]
                (doall
                 (map (fn [S]
                         (when (> (score-TC S PSET NSET) (score-TC H PSET NSET))
                           (swap! NEW-SET conj S)))
                       (->> (rest NSET)
                            (map (partial especializaciones-TC H (first NSET)))
                            (apply concat)
                            (remove (partial = H))
                            distinct)))
                (if (empty? @NEW-SET)
                  (swap! CLOSED-SET conj H)
                  (doall
                   (map (fn [S]
                           (swap! OPEN-SET conj S)
                           (doall
                            (map (fn [C]
                                    (when  (= -1 (naive-cmp-concepto-TC S C))
                                      ;;(concepto-CL>= C S)
                                      (if (> (score-TC C PSET NSET)
                                             (score-TC S PSET NSET))
                                        (swap! OPEN-SET   without S)
                                        (swap! CLOSED-SET without C))))
                                  @CLOSED-SET)))
                         @NEW-SET)))))
            HSET))
     (prn "Second [CLOSED-SET,OPEN-SET]=" [(count @CLOSED-SET) (count @OPEN-SET)])
     (if (empty? @OPEN-SET)
       (let [result (->> @CLOSED-SET (sort-by-scoreTC-desc PSET NSET))]
         (prn "Result" result)
         (prn "Total" (count result))
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
                    ;; todo: change 0 by (dec (count (first ejemplos)))
                    [] [(cons (dec (count meta)) (concepto-CL-mas-general meta))] beam-size)]
     htc0)))

(HTC ejemplos 10)
