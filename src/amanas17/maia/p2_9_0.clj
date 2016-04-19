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
;; Inducción de conceptos TC ;;
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

;; TODO: test me
(defn cmp-concepto-TC
  "Compara dos conceptos TC. Devuelve:
   1 si TC1 es más general que TC2
  -1 si TC1 es más específico que TC2
   0 en otro caso"
  [[u1 & CL1 :as TC1] [u2 & CL2 :as TC2]]
  (if (= u1 u2) (cmp-concepto-CL CL1 CL2)
      (let [cmp-sum (->> (interleave CL1 CL2)
                         (map vector)
                         (partition 2)
                         (map (partial apply cmp-concepto-CL))
                         (reduce +)
                         (+ u2 (* -1 u1)))]
 ;;       (prn cmp-sum)
        (cond (neg?  cmp-sum) -1
              (zero? cmp-sum)  0
              :else            1))))

(assert (and (=  0 (cmp-concepto-TC [0 [**]] [0 [**]]))
             (= -1 (cmp-concepto-TC [0 []]   [0 [**]]))
             (=  1 (cmp-concepto-TC [0 [**]] [0 []]))
             (=  0 (cmp-concepto-TC [1 [**]] [1 [**]]))
             (= -1 (cmp-concepto-TC [1 [**]] [0 [**]]))
             (=  1 (cmp-concepto-TC [0 [**]] [1 [**]]))
             (=  0 (cmp-concepto-TC [0 []]   [1 [**]]))
             (=  0 (cmp-concepto-TC [0 []]   [1 ['a]]))
             (= -1 (cmp-concepto-TC [0 []]   [0 ['a]]))
             (=  0 (cmp-concepto-TC [0 ['b]] [0 ['a]]))))

;; Ejercicio 2.28
(defn HTC0
  "Algoritmo de búsqueda heurística HTC, de general a específico,
   dirigido por una función de evaluación.
   Devuelve el concepto que satisface la mayoría de los ejemplos
   positivos y excluye la mayoría de los negativos.
   Asume que los conjuntos PSET y NSET tienen cabecera de atributos
   Sobrecargo la función para que admita como parámetro el beam-size
   Los conjuntos ionosphere y agaricus-lepiota tienen muchos ejemplos,
   por lo que para que el algoritmo acabe sin desbordarse tengo que tomar
   samples"
  ([PSET NSET CLOSED-SET HSET]
   (HTC0 PSET NSET CLOSED-SET HSET 10 100))
  ([PSET NSET CLOSED-SET HSET beam-size sample-size]
   (let [CLOSED-SET (atom (set CLOSED-SET))
         OPEN-SET   (atom #{})]
     (prn "1 - HTC0 [CLOSED-SET,HSET]" [(count @CLOSED-SET) (count HSET)])
     (let [meta (first PSET)
           PSET (cons meta (take sample-size (shuffle (rest PSET))))
           NSET (cons meta (take sample-size (shuffle (rest NSET))))]
       (doall (pmap (fn [H] (let [NEW-SET (atom #{})]
                             (doall (pmap (fn [S] (when (> (score-TC S PSET NSET) (score-TC H PSET NSET))
                                                   (swap! NEW-SET conj S)))
                                          (->> (rest NSET)
                                               (pmap (partial especializaciones-TC H meta))
                                               (apply concat)
                                               shuffle (take sample-size)
                                               (remove (partial = H)) distinct)))
                             (if (empty? @NEW-SET)
                               (swap! CLOSED-SET conj H)
                               (doall (pmap (fn [S] (swap! OPEN-SET conj S)
                                              (doall (pmap (fn [C] (when  (< (cmp-concepto-TC S C) 0)
                                                                    (if (> (score-TC C PSET NSET)
                                                                           (score-TC S PSET NSET))
                                                                      (swap! OPEN-SET   without S)
                                                                      (swap! CLOSED-SET without C))))
                                                           @CLOSED-SET)))
                                            @NEW-SET)))))
                    HSET)))
     (prn "2 - HTC0 [CLOSED-SET,OPEN-SET]" [(count @CLOSED-SET) (count @OPEN-SET)])
     (if (empty? @OPEN-SET)
       (let [result (->> @CLOSED-SET (sort-by-scoreTC-desc PSET NSET))]
         (first result))
       (let [BEST-SET (->> @OPEN-SET (into @CLOSED-SET) (sort-by-scoreTC-desc PSET NSET)
                           (take beam-size) set)
             CLOSED-SET (filter BEST-SET @CLOSED-SET)
             OPEN-SET   (filter BEST-SET @OPEN-SET)]
         (HTC0 PSET NSET CLOSED-SET OPEN-SET beam-size sample-size))))))


(defn HTC
  "Devuelve un concepto al azar del resultado de aplicar HTC0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  ([ejemplos] (HTC ejemplos 50))
  ([ejemplos beam-size] (HTC ejemplos beam-size 10000))
  ([ejemplos beam-size sample-size]
   (let [meta (first ejemplos)
         ej+ (cons meta  (->> ejemplos rest (filter (comp (partial = '+) last))))
         ej- (cons meta (->> ejemplos rest (filter (comp (partial = '-) last))))
         concepto-TC-mas-general (cons 0 (concepto-CL-mas-general meta))
         htc0 (HTC0 ej+ ej-  [] [concepto-TC-mas-general] beam-size sample-size)]
     htc0)))

(comment (time (prn (HTC ejemplos))))
;; Compare el concepto TC devuelto por HTC con el concepto CL obtenido por HGS
;; y el concepto CL del ejercicio 3.
;; Mi buen concepto:        [  [soleado] [20 30]   [60 80]   [no] [contento] [**] [solvente]]
;; El concepto con HGS es:  [  [**]      [22 40]   [79 +inf] [**] [contento] [**] [**]]
;; El concepto con HTC es:  [7 [**]      [-inf 37] [65 +inf] [**] [contento] [**] [solvente]]

;; El concepto devuelto por HTC es perfectamente coherente con el concepto que yo había
;; definido como buen día para salir el campo. De hecho es bastante más parecido que el devuelto
;; por HGS.
;; En todo caso, los tres conceptos anteriores parecen bastante coherentes entre sí

;; El tiempo empleado por el algorítmo para procesar los ejemplos de prueba ha sido de
;; 2 segundos con 5 iteraciones


;; Ejercicio 2.29
;; Ejecute HTC mediante el conjunto de ejemplos agaricuslepiota.scm e ionosphere.scm
;; (ver repositorio de material ejemplos UCI).
;; Observe la traza del algoritmo y describa su comportamiento en presencia
;; de estos nuevos ejemplos. Tambien compare el comportamiento de HTC con el de HGS.

<<<<<<< HEAD
;; TODO: porque aquí me quedo sin memoria



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inducción de conceptos LUU ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 2.30
(defn PRM
  "Algoritmo de inducción incremental de umbrales utilizando búsqueda
   por gradiente descendente"
  ([concepto-UU ejemplos]
   (PRM concepto-UU ejemplos 1))
  ([concepto-UU ejemplos revision-rate]
   (loop [[ATTS [& values] :as H] concepto-UU
          [I & more] (rest ejemplos)]
     (if (empty? more) H
         (let [C (last I)
               P (last (LUUi H (butlast I)))
               S (cond (and  (= '- P) (= '+ C))  1
                       (and  (= '+ P) (= '- C)) -1
                       :else nil)
               new-H (if (= C P) H
                         (let [new-values (map (fn [i] (+ (nth values i)
                                                         (* S revision-rate
                                                            (traducir (nth ATTS i) (nth I i)))))
                                               (range (dec (count values))))
                               new-umbral (+ (last values) (* S revision-rate))]
                           [ATTS (conj new-values new-umbral)]))]
           (recur new-H more))))))

;; Compruebo el funcionamiento de PRM con los ejemplos
(comment (PRM [(first ejemplos) '(1 1 1 1 1 1 1 5)] ejemplos))
;; El resultado es:
;; [[[perspectiva [soleado nublado lluvioso]]
;;   [temperatura numerico]
;;   [humedad numerico]
;;   [viento [no si]]
;;   [animo [contento triste]]
;;   [estres [relajado estresado]]
;;   [dinero [solvente insuficiente]]
;;   [clase [+ -]]]
;;  [15 27 92 -1 -16 22 -4 5]]

(he-tardado 120 2.30)


;; Ejercicio 2.31
(defn PCP
  "Implementación del procedimiento de convergencia de perceptrón"
  ([ejemplos]
   (PCP ejemplos 1 1000))
  ([[meta & instances :as ejemplos] revision-rate maximun-iterations]
   (loop [H (nuevo-conceptoUU meta 1)]
     H)
   ))

(PCP ejemplos)
=======
(comment (time (prn (HTC ionosphere 1))))
;; Para reducir la computación, utilizo un beam-size de 1
;; Según HTC, este concepto que describe el dataset ionosphere es:
;; [34 [**] [**] [0 +inf] [**] [0.22222 +inf] [-1 +inf] [**] [-1 +inf]
;;     [**] [-1 +inf] [**] [**] [**] [-0.69707 +inf] [**] [-inf 1] [**]
;;     [-1 +inf] [**] [**] [**] [**] [**] [**] [**] [**] [**] [-1 +inf]
;;     [-1 +inf] [**] [**] [**] [**] [-1 +inf]]

;; El tiempo empleado por el algorítmo para procesar el dataset ionosphere ha sido de
;; 322 segundos con 13 iteraciones (muy elevado, teniendo en cuenta que hemos utilizado un
;; beam-size de 1)

(comment (time (prn (HTC agaricus-lepiota 1 100))))
;; Para reducir la computación, utilizo un beam-size de 1
;; Según HTC, el concepto que mejor describe el dataset agaricus-lepiota es:
;; ...
;; El tiempo empleado por el algorítmo para procesar el dataset agaricus-lepiota ha sido de
;; ... segundos con ... iteraciones (muy elevado, teniendo en cuenta que hemos utilizado un
;; beam-size de 1)
>>>>>>> branch 'master' of https://github.com/amanas/amanas17-clj.git
