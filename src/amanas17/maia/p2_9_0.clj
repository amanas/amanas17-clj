(ns amanas17.maia.p2-9-0
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-6]
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
                  (cond (neg? cmp-sum) -1
                        (zero? cmp-sum) 0
                        :else 1))))

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
         OPEN-SET (atom #{})]
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
                                               (doall (pmap (fn [C] (when (< (cmp-concepto-TC S C) 0)
                                                                      (if (> (score-TC C PSET NSET)
                                                                             (score-TC S PSET NSET))
                                                                        (swap! OPEN-SET without S)
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
             OPEN-SET (filter BEST-SET @OPEN-SET)]
         (HTC0 PSET NSET CLOSED-SET OPEN-SET beam-size sample-size))))))


(defn HTC
  "Devuelve un concepto al azar del resultado de aplicar HTC0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  ([ejemplos] (HTC ejemplos 50))
  ([ejemplos beam-size] (HTC ejemplos beam-size 10000))
  ([ejemplos beam-size sample-size]
   (let [meta (first ejemplos)
         ej+ (cons meta (->> ejemplos rest (filter (comp (partial = '+) last))))
         ej- (cons meta (->> ejemplos rest (filter (comp (partial = '-) last))))
         concepto-TC-mas-general (cons 0 (concepto-CL-mas-general meta))
         htc0 (HTC0 ej+ ej- [] [concepto-TC-mas-general] beam-size sample-size)]
     htc0)))

(comment (time (prn (HTC ejemplos))))
;; Compare el concepto TC devuelto por HTC con el concepto CL obtenido por HGS
;; y el concepto CL del ejercicio 3.
;; Mi buen concepto:        [  [soleado] [20 30]   [60 80]   [no] [contento] [*] [solvente]]
;; El concepto con EGS es:  [  [soleado] [-inf 35] [-inf 70] [*]  [*]        [*]  [*]]
;; El concepto con HTC es:  [7 [*]       [6 +inf]  [-inf 70] [*]  [*]        [*]  [*]]

;; El concepto devuelto por HTC es perfectamente coherente con el concepto que yo había
;; definido como buen día para salir el campo. De hecho es bastante más parecido que el devuelto
;; por HGS.
;; En todo caso, los tres conceptos anteriores parecen bastante coherentes entre sí

;; El tiempo empleado por el algoritmo para procesar los ejemplos de prueba ha sido de
;; 2 segundos con 5 iteraciones

(he-tardado 300 2.28)

;; Ejercicio 2.29
;; Ejecute HTC mediante el conjunto de ejemplos agaricuslepiota.scm e ionosphere.scm
;; (ver repositorio de material ejemplos UCI).
;; Observe la traza del algoritmo y describa su comportamiento en presencia
;; de estos nuevos ejemplos. Tambien compare el comportamiento de HTC con el de HGS.

(comment (time (prn (HTC ionosphere 1))))
;; Para reducir la computación, utilizo un beam-size de 1
;; Según HTC, este concepto que describe el dataset ionosphere es:
;; [34 [**] [**] [0 +inf] [**] [0.22222 +inf] [-1 +inf] [**] [-1 +inf]
;;     [**] [-1 +inf] [**] [**] [**] [-0.69707 +inf] [**] [-inf 1] [**]
;;     [-1 +inf] [**] [**] [**] [**] [**] [**] [**] [**] [**] [-1 +inf]
;;     [-1 +inf] [**] [**] [**] [**] [-1 +inf]]

;; El tiempo empleado por el algoritmo para procesar el dataset ionosphere ha sido de
;; 322 segundos con 13 iteraciones (muy elevado, teniendo en cuenta que hemos utilizado un
;; beam-size de 1)

(comment (time (prn (HTC agaricus-lepiota 1 100))))
;; Para reducir la computación, utilizo un beam-size de 1
;; Según HTC, el concepto que mejor describe el dataset agaricus-lepiota es:
;; ...
;; El tiempo empleado por el algoritmo para procesar el dataset agaricus-lepiota ha sido de
;; ... segundos con ... iteraciones (muy elevado, teniendo en cuenta que hemos utilizado un
;; beam-size de 1)
;; No consigo que acabe nunca
(he-tardado 120 2.29)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inducción de conceptos LUU ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 2.30
(defn PRM
  "Algoritmo de inducción incremental de umbrales utilizando búsqueda
   por gradiente descendente.
   Asume que ejemplos tiene cabecera de atributos"
  ([concepto-UU ejemplos]
   (PRM concepto-UU ejemplos 1))
  ([concepto-UU ejemplos revision-rate]
   (loop [[ATTS [& values] :as H] concepto-UU
          [I & more] (rest ejemplos)]
     (if (empty? more)
       H
       (let [C (last I)
             P (last (LUUi H (butlast I)))
             S (cond (and (= '- P) (= '+ C)) 1
                     (and (= '+ P) (= '- C)) -1
                     :else nil)
             new-H (if (= C P)
                     H
                     (let [new-values
                           (map (fn [i]
                                  (+ (nth values i)
                                     (* S revision-rate
                                        (traducir (nth ATTS i) (nth I i)))))
                                (range (dec (count values))))
                           new-umbral (+ (last values) (* S revision-rate))]
                       [ATTS (conj new-values new-umbral)]))]
         (recur new-H more))))))

(nuevo-conceptoUU (first all-ejemplos) 1)

;; Compruebo el funcionamiento de PRM con los ejemplos
(comment (PRM (nuevo-conceptoUU (first all-ejemplos) 1) all-ejemplos))
;; El resultado es:
;; [[[perspectiva [soleado nublado lluvioso]]
;;   [temperatura numerico]
;;   [humedad numerico]
;;   [viento [no si]]
;;   [animo [contento triste]]
;;   [estres [relajado estresado]]
;;   [dinero [solvente insuficiente]]
;;   [clase [+ -]]]
;;  (-44.383805766568 -16.086289740523597 -13.239547202258358 -13.623480206900027
;;   -15.630331093156265 -20.404609821193304 -49.401342824614545 -37.03602040856606)

(he-tardado 120 2.30)


;; Ejercicio 2.31
(defn PCP
  "Implementación del procedimiento de convergencia de perceptrón.
   Inducción no incremental de unidades umbral lineales mediante la aplicación
   iterativa de PRM al conjunto de entrenamiento hasta que produce un LTU sin errores
   Devuelve una LUU"
  ([ejemplos] (PCP ejemplos 1 1000))
  ([[meta & instances :as ejemplos] revision-rate max-iterations]
   (loop [H (nuevo-conceptoUU meta 1)
          COUNT max-iterations]
     (if (= 0 COUNT)
       H
       (let [NO-ERRORS (every? true? (map (fn [i] (= (last i) (last (LUUi H (butlast i))))) instances))]
         (if NO-ERRORS
           H
           (recur (PRM H ejemplos revision-rate) (dec COUNT))))))))


;; El resultado con mis ejemplos de entrenamiento es el siguiente
(comment (prn (PCP all-ejemplos)))
;; [[[perspectiva [soleado nublado lluvioso]]
;;   [temperatura numerico]
;;   [humedad numerico]
;;   [viento [no si]]
;;   [animo [contento triste]]
;;   [estres [relajado estresado]]
;;   [dinero [solvente insuficiente]]
;;   [clase [+ -]]]
;;   (-18.211270201681756 -32.72141351918313 -20.742765012614257 45.127372896406484
;;    -8.148694630567007 -17.940338877283438 -0.20577680512042207 -43.926932047154025)

(he-tardado 120 2.31)




;; Ejercicio 2.32

(defn LMS
  "Implementación del procedimiento de mínimos cuadrados.
   Inducción no incremental de unidades umbral lineales usando gradiente descendente
   con la intención de minimizar el error cuadrático medio entre predicciones
   y observaciones. LMS asume que el valor del umbral es siempre 1.
   Devuelve una LUU"
  ([ejemplos] (LMS ejemplos 1 1 1000 1))
  ([[meta & instances :as ejemplos] gain momentum max-iterations min-error]
   (loop [deltas (repeat (count meta) 0)
          [meta weights :as H] (nuevo-conceptoUU meta 0.5)
          COUNT max-iterations]
     (if (= 0 COUNT)
       H
       (let [TOTAL-ERROR (atom 0)]
         (doall (for [I instances]
                  (let [Oi (traducir (last meta) (last I))
                        Pi (traducir (last meta) (last (LUUi H (butlast I))))]
                    (swap! TOTAL-ERROR + (Math/pow (- Oi Pi) 2)))))
         (if (< @TOTAL-ERROR min-error)
           H
           (let [new-DW-fn
                 (fn [i]
                   (let [W (nth weights i)
                         A (nth meta i)
                         D (nth deltas i)
                         GRADIENT (atom 0)]
                     (doall (for [I instances]
                              (let [Oi (traducir (last meta) (last I))
                                    Pi (traducir (last meta) (last (LUUi H (butlast I))))
                                    d (* Pi (- 1 Pi) (- Oi Pi))
                                    v (traducir A (nth I i))]
                                (swap! GRADIENT + (* gain d v)))))
                     [(+ @GRADIENT (* momentum D)) (+ W @GRADIENT (* momentum D))]))
                 new-DWs (map new-DW-fn (range (count weights)))]
             (recur (map first new-DWs) [meta (map second new-DWs)] (dec COUNT)))))))))

;; El resultado con mis ejemplos de entrenamiento es el siguiente
(comment (LMS all-ejemplos))
;; [[[perspectiva [soleado nublado lluvioso]]
;;   [temperatura numerico]
;;   [humedad numerico]
;;   [viento [no si]]
;;   [animo [contento triste]]
;;   [estres [relajado estresado]]
;;   [dinero [solvente insuficiente]]
;;   [clase [+ -]]]
;;  (0.38388766148071873 -0.1605937637606173 -0.023410616245329474 0.26412345677057236
;;   -0.03263919954078087 -0.48302186083242205 0.4315593345019314 -0.24718183805573568))

(he-tardado 90 2.32)

;; Ejercicio 2.33
;; Compruebe la diferencia de precisión entre PCP y LMS
;; (utilice la función cross-validation con folds a 10) en los conjuntos de
;; ejemplos buen día para salir al campo, agaricus-lepiota.scm e ionosphere.scm.

;; El algoritmo PCP sobre los ejemplos habituales
(comment (cross-validation PCP LUUi all-ejemplos 10))
;; ofrece una precisión de 7/12

;; El algoritmo LMS sobre los ejemplos habituales
(comment (cross-validation LMS LUUi all-ejemplos 10))
;; ofrece una precisión de 2/3

;; ¿Encuentra diferencias significativas?
;; Al amparo de los resultados en la precisión, parece que LMS clasifica
;; sutilmente mejor los ejemplos

;; No consigo que la computación llegue a acabar con los datos ionosphere
;; y agaricuslepiota
(comment (cross-validation PCP LUUi ionosphere 10))
(comment (cross-validation PCP LUUi agaricus-lepiota 10))
