(ns amanas17.maia.p3-1
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-4]
        [amanas17.maia.p1-5]
        [amanas17.maia.p1-6]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-4]
        [amanas17.maia.p2-5]
        [amanas17.maia.p2-6]
        [amanas17.maia.p2-7]
        [amanas17.maia.p2-9-0]
        [amanas17.maia.p2-9-1]
        [amanas17.maia.p2-9-2]
        [amanas17.maia.p3-0]))

;; Ejercicio 3.2
(defn funcion-match
  "Para cada algoritmo devuelve la función match asociada"
  [algoritmo]
  (condp = algoritmo
    HGS match-CL
    HTC match-CL
    PCP match-LUU
    LMS match-LUU
    IB match-IB
    NB match-NB))

(assert (eval '((funcion-match HGS) '((soleado) (*)) '(soleado si))))

(he-tardado 30 3.2)


;; Ejercicio 3.3
(defn NSC0
  "Implementación no incremental de inducción de formas normales
  disjuntivas (DNF) usando un enfoque de divide y vencerás"
  [algoritmo PSET NSET DNF]
  (loop [PSET PSET
         NSET NSET
         DNF DNF]
    (if (empty? (rest PSET))
      DNF
      (let [concepto (algoritmo (concat PSET (rest NSET)))
            newPSET (remove (fn [e] ((funcion-match algoritmo) concepto (butlast e))) (rest PSET))
            ;; TODO: review
            newPSET (if (= (count newPSET) (count (rest PSET))) [] newPSET)]
        (recur (cons (first PSET) newPSET) NSET (concat DNF [concepto]))))))

(defn NSC
  [algoritmo ejemplos]
  (let [meta (first ejemplos)
        ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
        ej- (->> ejemplos rest (filter (comp (partial = '-) last)))]
    (NSC0 algoritmo (cons meta ej+) (cons meta ej-) [])))


;; Ejercicio 3.4
;Estudie la salida de la función NSC a partir de los datos
;“buen díıa para salir al campo” y los datos “agaricus-lepiota” e “ionosphere”
;procedentes de UCI mediante los algoritmos HGS, HTC, PCP y LMS. Comen-
;te las diferencias, por ejemplo ¿qu ́e configuración devuelve listas de mayor
;tamaño? ¿cuáles considera que podr ́ıan ofrecer mayor precisión?
;; TODO
(comment (NSC HGS all-ejemplos))
(comment (NSC HTC all-ejemplos))
(comment (NSC PCP all-ejemplos))
(comment (NSC LMS all-ejemplos))
(comment (NSC IB all-ejemplos))
(comment (NSC NB all-ejemplos))

;; Ejercicio 3.5
(defn MSC0
  "Algoritmo de inducción no incremental de listas de decisión multiclase"
  [algoritmo ejemplos]
  (let [meta (first ejemplos)
        CLASS-SET (second (last (first ejemplos)))
        RULE-SET (atom '())]
    (for [CLASS CLASS-SET]
      (let [PSET (cons meta (->> ejemplos rest (filter (fn [x] (= CLASS (last x))))))
            NSET (cons meta (->> ejemplos rest (remove (fn [x] (= CLASS (last x))))))
            DNF (NSC0 algoritmo PSET NSET [])]
        (for [D DNF]
          (do (prn D)
              (swap! RULE-SET concat (list (funcion-match algoritmo) D '=> CLASS))))))))

(defn MSC
  "Añade a la lista de decisión devuelta por MSC0 un valor por defecto"
  [algoritmo ejemplos]
  (concat (MSC0 algoritmo ejemplos)
          (list match-CL (repeat (dec (count (first ejemplos))) '(*)) '=> (A0 ejemplos)))) )

(he-tardado 60 3.5)


;; Ejercicio 3.6
;; Estudie la eficiencia de MSC mediante distintas configuraciones variando
;; el algoritmo introducido como parámetro: HGS, HTC, PCP y LMS.
;; Para ello, denomine cada nueva función como MSC-<subalgoritmo> (p.ej. MSC-HGS).
;;
;; Tras ello, compruebe la eficiencia de cada uno mediante ten-fold-crossvalidation
;; y los datos “buen d ́ıa para salir al campo”, “agaricus-lepiota”, “ionosphere”,
;; “poker” y “lymphography”. ¿Qu ́e configuración produce mejores resultados?
;; ¿Por qué cree que es así?.

;; TODO














