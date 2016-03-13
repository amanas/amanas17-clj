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
  "Devuelve el concepto CL que es generalización inmediata de concepto-CL en el atributo
  referenciado  mediante indice-atributo para el ejemplo"
  [concepto-CL indice-atributo ejemplo]
  (let [test (nth concepto-CL indice-atributo)
        [a b :as t] (normalize-numerico test)
        atributo (nth ejemplo indice-atributo)
        gener (cond (= :- (last ejemplo))  test
                    (match-numerico? test atributo) test
                    (= [] t) [atributo]
                    (= [*] t) test
                    (comp< atributo a) (normalize-numerico [[atributo] b])
                    (comp< b atributo) (normalize-numerico [a [atributo]])
                    :else test)]
    (concat (take indice-atributo concepto-CL)
            [gener]
            (drop (inc indice-atributo) concepto-CL))))

;; Ejercicio 2.12
(defn especializaciones-atributo-numerico
  "Devuelve una lista de todos los conceptos CL que sean especialización inmediata
  de concepto-CL en el atributo referenciado mediante indice-atributo."
  [concepto-CL indice-atributo ejemplo]
  (let [test (nth concepto-CL indice-atributo)
        [a b :as t] (normalize-numerico test)
        atributo (nth ejemplo indice-atributo)
        specs (cond (= :+ (last ejemplo))  [test]
                    (not (match-numerico? test atributo)) [test]
                    (= [] t) [[]]
                    (= [*] t) [[:-inf atributo] [atributo :+inf]]
                    ;(coll? a) (cond (comp< atributo a)) test
                    ;(comp< b atributo) (normalize-numerico [a [atributo]])
                    ;:else test
                    )]
    (concat (take indice-atributo concepto-CL)
            [specs]
            (drop (inc indice-atributo) concepto-CL))))
