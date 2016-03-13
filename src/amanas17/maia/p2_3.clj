(ns amanas17.maia.p2-3
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]))

;; Ejercicio 2.9
(defn especializaciones-atributo-nominal
  "Devuelva una lista de todos los conceptos CL que sean especialización inmediata
  de concepto-CL en el atributo referenciado mediante indice-atributo"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        test (if (= [*] test) (second (nth metadatos indice-atributo)) test)
        especs (map (fn [v] (remove #{v} test)) test)
        especs (if (empty? especs) [[]] especs)]
    (map (fn [espec] (concat (take indice-atributo concepto-CL)
                             [espec]
                             (drop (inc indice-atributo) concepto-CL)))
         especs)))

(he-tardado 60 2.9)

;; Ejercicio 2.10
(defn generalizaciones-atributo-nominal
  "Devuelva una lista de todos los conceptos CL que sean generalización inmediata
  de concepto-CL en el atributo referenciado mediante indice-atributo"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        attribute-values (second (nth metadatos indice-atributo))
        remaining-values (remove (partial match-nominal? test) attribute-values)
        geners (cond (empty? remaining-values) [[*]]
                     (= 1 (count remaining-values)) [[*]]
                     :else (map (fn [v] (concat test [v])) remaining-values))]
    (map (fn [gener] (concat (take indice-atributo concepto-CL)
                             [gener]
                             (drop (inc indice-atributo) concepto-CL)))
         geners)))

(he-tardado 60 2.10)

;; Ejercicio 2.11
(defn generalizacion-atributo-numerico
  "** Objetivo: el nuevo concepto cubre el ejemplo si es positivo **
  En el caso de que el ejemplo sea negativo o el concepto-CL ya cubra el ejemplo en el atributo
  referenciado por indice-atributo, devuelve el concepto-CL tal cual lo recibe.
  En el caso de que el ejemplo sea positivo y el concepto-CL no cubra el ejemplo en el atributo
  referenciado por indice-atributo, devuelve la generalición inmediata del concepto-CL que si
  cubre el ejemplo en el atributo referenciado por indice-atributo."
  [concepto-CL indice-atributo ejemplo]
  (if (= :- (last ejemplo))  concepto-CL
    (let [test (nth concepto-CL indice-atributo)]
      (if (match-numerico? test atributo) concepto-CL
        (let [[a b :as t] (normalize-numerico test)
              atributo (nth ejemplo indice-atributo)
              gener (cond (= [] t) [atributo]
                          (= [*] t) test
                          (extremo<= atributo a) (normalize-numerico [[atributo] b])
                          (extremo<= b atributo) (normalize-numerico [a [atributo]])
                          :else test)]
          (concat (take indice-atributo concepto-CL)
                  [gener]
                  (drop (inc indice-atributo) concepto-CL)))))))

;; Ejercicio 2.12
(defn especializaciones-atributo-numerico
  "** Objetivo: el nuevo concepto rechaza el ejemplo si es negativo **
  En el caso de que el ejemplo sea positivo o el concepto-CL no cubra el ejemplo en el atributo
  referenciado por indice-atributo, devuelve el concepto-CL tal cual lo recibe.
  En el caso de que el ejemplo sea negativo y el concepto-CL si cubra el ejemplo en el atributo
  referenciado por indice-atributo, devuelve una lista con las especializaciones inmediata
  del concepto-CL que en el atributo referenciado por indice-atributo."
  [concepto-CL indice-atributo ejemplo]
  (if (= :+ (last ejemplo)) [concepto-CL]
    (let [test (nth concepto-CL indice-atributo)]
      (if (not (match-numerico? test atributo)) [concepto-CL]
        (let [[a b :as t] (normalize-numerico test)
              atributo (nth ejemplo indice-atributo)
              specs (cond (= [] t) [[]]
                          (= [*] t) [[:-inf atributo] [atributo :+inf]]
                          :else [(normalize-numerico [a atributo]) (normalize-numerico [atributo b])])]
          (map (fn [spec] (concat (take indice-atributo concepto-CL)
                                  [spec]
                                  (drop (inc indice-atributo) concepto-CL)))
               specs))))))








