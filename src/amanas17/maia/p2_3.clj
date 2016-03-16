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
  Devuelve la generalición inmediata de <concepto-CL> que sí cubre
  <ejemplo> en el atributo referenciado por <indice-atributo>.
  Si <ejemplo> es negativo o <concepto-CL> ya satisface el ejemplo
  en el atributo referenciado por <indice-atributo>, devuelve el concepto
  tal cual lo recibe."
  [concepto-CL indice-atributo ejemplo]
  (if (= :- (last ejemplo)) concepto-CL
    (let [test (nth concepto-CL indice-atributo)
          atributo (nth ejemplo indice-atributo)]
      (if (match-numerico? test atributo) concepto-CL
        (let [[a b :as t] (normalize-numerico test)
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
  Devuelve una lista con las especializaciones inmediatas de <concepto-CL>
  que no cubren el <ejemplo> en el atributo referenciado por <indice-atributo>
  cuando el ejemplo es negativo.
  Si <ejemplo> es positivo o <concepto-CL> no cubre el ejemplo en el atributo
  referenciado por indice-atributo, devuelve una lista cuyo único elemento es
  <concepto-CL>."
  [concepto-CL indice-atributo ejemplo]
  (if (= :+ (last ejemplo)) [concepto-CL]
    (let [test (nth concepto-CL indice-atributo)]
      (if-not (match-numerico? test atributo) [concepto-CL]
        (let [[a b :as t] (normalize-numerico test)
              atributo (nth ejemplo indice-atributo)
              specs (cond (= [] t) [[]]
                          (= [*] t) [[:-inf atributo] [atributo :+inf]]
                          :else [(normalize-numerico [a atributo])
                                 (normalize-numerico [atributo b])])]
          (map (fn [spec] (concat (take indice-atributo concepto-CL)
                                  [spec]
                                  (drop (inc indice-atributo) concepto-CL)))
               specs))))))

(defn cartesian-product [colls]
  "Devuelve el producto cartesiano de una lista de listas"
  (if (empty? colls) [[]]
    (for [x (first colls) more (cartesian-product (rest colls))]
      (cons x more))))

(defn incluye-atributo-nominal
  [concepto-CL indice ejemplo metadatos]
  (let [test (nth concepto-CL indice)]
    (if (= [*] test) concepto-CL
      (let [atribs (distinct (conj test (nth ejemplo indice)))
            new-test (if (= (count atribs) (count (second (nth metadatos indice)))) [*] atribs)]
        (concat (take indice concepto-CL) [new-test] (drop (inc indice) concepto-CL))))))

;; Ejercicio 2.13
(defn generalizaciones-CL
  "** Objetivo: los nuevos conceptos cubren el ejemplo si es positivo **
  Devuelve el conjunto total de generalizaciones inmediatas
  (respecto a todos los atributos) de concepto-CL para el ejemplo"
  ([concepto-CL metadatos ejemplo indice]
   (if (= :numerico (second (nth metadatos indice)))
     [(nth (generalizacion-atributo-numerico concepto-CL indice ejemplo) indice)]
     (let [concepto-CL (incluye-atributo-nominal concepto-CL indice ejemplo metadatos)
           test (nth concepto-CL indice)
           atributo (nth ejemplo indice)
           geners (if (match-nominal? test atributo)
                    [concepto-CL]
                    (generalizaciones-atributo-nominal concepto-CL indice metadatos))]
       (map (fn[g] (nth g indice)) geners))))
  ([concepto-CL metadatos ejemplo]
   (->> metadatos count range butlast
        (map (partial generalizaciones-CL concepto-CL metadatos ejemplo))
        cartesian-product)))
