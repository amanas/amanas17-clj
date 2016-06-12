(ns amanas17.maia.p3-2
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
        [amanas17.maia.p2-9-2]))

;; Ejercicio 3.7
(defn adc
  "Elige la rama apropiada"
  [ejemplo-sin-clase ramas]
  (or (->> (for [[concepto _ subrama] ramas]
             (when (match-CL concepto ejemplo-sin-clase) subrama))
           (remove nil?)
           first)
      '()))


(assert (= (adc '(soleado 20)
                '((((soleado) (*)) -> (adc (((*) (-inf 30)) -> (=> +))
                                           (((*) ((30) +inf)) -> (=> -))))
                   (((nublado) (*)) -> (=> -))
                   (((lluvioso) (*)) -> (adc (((*) (-inf 10)) -> (=> -))
                                             (((*) ((10) +inf)) -> (=> +))))))
           '(adc (((*) (-inf 30)) -> (=> +))
                 (((*) ((30) +inf)) -> (=> -)))))

(assert (= (adc '(soleado 20)
                '((((*) (-inf 30)) -> (=> +))
                   (((*) ((30) +inf)) -> (=> -))))
           '(=> +)))

(he-tardado 60 3.7)

;; Ejercicio 3.8
(def JC-multivariado-equiv '((((soleado) (-inf 30)) -> (=> +))
                              (((soleado) ((30) +inf)) -> (=> -))
                              (((nublado) (*)) -> (=> -))
                              (((lluvioso) (-inf 10)) -> (=> -))
                              (((lluvioso) ((10) +inf)) -> (=> -))))

(he-tardado 20 3.8)


;; Ejercicio 3.9

(defn adg
  "Método de elección de rama para árboles de decisión generalizados"
  [ejemplo-sin-clase ramas]
  (or (->> (for [[[f-match concepto] _ subrama] ramas]
             (let [eval-concepto (condp = (name f-match)
                                   "match-LUU" [(try (eval (first concepto)) (catch Exception e (first concepto)))
                                                (try (eval (second concepto)) (catch Exception e (second concepto)))]
                                   concepto)]
               (when ((eval f-match) eval-concepto ejemplo-sin-clase)
                 subrama)))
           (remove nil?)
           first)
      '()))

(assert (= (adg '(soleado 20)
                '(((match-CL ((soleado) (*))) ->
                    (adg ((match-LUU ((map (vec (first ejemplos)) [0 1 7]) (1 2 -10))) -> (=> +))
                         ((match-LUU ((map (vec (first ejemplos)) [0 1 7]) (1 2 10))) -> (=> -))))
                   ((match-CL ((nublado) (*))) -> (=> -))))
           '(adg ((match-LUU ((map (vec (first ejemplos)) [0 1 7]) (1 2 -10))) -> (=> +))
                 ((match-LUU ((map (vec (first ejemplos)) [0 1 7]) (1 2 10))) -> (=> -)))))

(assert (= (adg '(soleado 20)
                '(((match-LUU ((map (vec (first ejemplos)) [0 1 7]) (1 2 50))) -> (=> +))
                   ((match-LUU ((map (vec (first ejemplos)) [0 1 7]) (1 2 -10))) -> (=> -))))
           '(=> -)))

(he-tardado 120 3.9)


;; Ejercicio 3.10
(defn JCi
  "Intérprete de Jerarquías de conceptos"
  [[f & ramas :as concepto-JC] ejemplo-sin-clase]
  (if (empty? concepto-JC)
    ejemplo-sin-clase
    (let [result ((eval f) ejemplo-sin-clase ramas)]
      (if (= '=> (first result))
        (concat ejemplo-sin-clase [(second result)])
        (JCi result ejemplo-sin-clase)))))


(assert (= (JCi '(adg
                   ((match-CL ((soleado) (*))) ->
                     (adg
                       ((match-CL ((*) (-inf 30))) -> (=> +))
                       ((match-CL ((*) ((30) +inf))) -> (=> -))))
                   ((match-CL ((nublado) (*))) -> (=> -))
                   ((match-CL ((lluvioso) (*))) ->
                     (adg
                       ((match-CL ((*) (-inf 10))) -> (=> -))
                       ((match-CL ((*) ((10) +inf))) => (=> +)))))
                '(soleado 20))
           '(soleado 20 +)))
(assert (= (JCi '(adc
                   (((soleado) (*)) ->
                     (adc
                       (((*) (-inf 30)) -> (=> +))
                       (((*) ((30) +inf)) -> (=> -))))
                   (((nublado) (*)) -> (=> -))
                   (((lluvioso) (*)) ->
                     (adc
                       (((*) (-inf 10)) -> (=> -))
                       (((*) ((10) +inf)) -> (=> +)))))
                '(lluvioso 50))
           '(lluvioso 50 +)))
(assert (= (JCi '(adc
                   (((soleado) (*)) ->
                     (adc
                       (((*) (-inf 30)) -> (=> +))
                       (((*) ((30) +inf)) -> (=> -))))
                   (((nublado) (*)) -> (=> -))
                   (((lluvioso) (*)) ->
                     (adc
                       (((*) (-inf 10)) -> (=> -))
                       (((*) ((10) +inf)) -> ()))))
                '(lluvioso 50))
           '(lluvioso 50)))

(he-tardado 60 3.10)