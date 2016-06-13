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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InducciÃ³n de conceptos LD ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 3.2
(defn funcion-match
  "Para cada algoritmo devuelve la funciÃ³n match asociada"
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
  "ImplementaciÃ³n no incremental de inducciÃ³n de formas normales
  disjuntivas (DNF) usando un enfoque de divide y vencerÃ¡s"
  [algoritmo PSET NSET DNF]
  (loop [PSET PSET
         NSET NSET
         DNF DNF]
    (if (empty? (rest PSET))
      DNF
      (let [concepto (algoritmo (concat PSET (rest NSET)))
            newPSET (remove (fn [e] ((funcion-match algoritmo) concepto (butlast e))) (rest PSET))
            newPSET (if (= (count newPSET) (count (rest PSET))) [] newPSET)]
        (recur (cons (first PSET) newPSET) NSET (concat DNF [concepto]))))))

(defn NSC
  [algoritmo ejemplos]
  (let [meta (first ejemplos)
        ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
        ej- (->> ejemplos rest (filter (comp (partial = '-) last)))]
    (NSC0 algoritmo (cons meta ej+) (cons meta ej-) [])))

(he-tardado 60 3.3)

;; Ejercicio 3.4
;Estudie la salida de la funciÃ³n NSC a partir de los datos
;â€œbuen dÃ­Ä±a para salir al campoâ€� y los datos â€œagaricus-lepiotaâ€� e â€œionosphereâ€�
;procedentes de UCI mediante los algoritmos HGS, HTC, PCP y LMS. Comen-
;te las diferencias, por ejemplo Â¿qu Ì�e configuraciÃ³n devuelve listas de mayor
;tamaÃ±o? Â¿cuÃ¡les considera que podr Ì�Ä±an ofrecer mayor precisiÃ³n?

(comment (NSC HGS all-ejemplos))
(comment (NSC HTC all-ejemplos))
(comment (NSC PCP all-ejemplos))
(comment (NSC LMS all-ejemplos))
(comment (NSC IB all-ejemplos))
(comment (NSC NB all-ejemplos))

(he-tardado 120 3.4)

;; Ejercicio 3.5
(defn MSC0
  "Algoritmo de inducciÃ³n no incremental de listas de decisiÃ³n multiclase"
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
  "AÃ±ade a la lista de decisiÃ³n devuelta por MSC0 un valor por defecto"
  [algoritmo ejemplos]
  (concat (MSC0 algoritmo ejemplos)
          (list match-CL (repeat (dec (count (first ejemplos))) '(*)) '=> (A0 ejemplos)))) )

(he-tardado 60 3.5)



