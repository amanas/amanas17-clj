(ns amanas17.maia.p2-3
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]))

;; Ejercicio 2.9
(defn especializaciones-atributo-nominal
  "Devuelva una lista las especializaciones inmediatas del concepto en el atributo
  referenciado por el índice"
  [concepto-CL indice-atributo metadatos]
  (let [test (let [t (nth concepto-CL indice-atributo)
                   atribs (second (nth metadatos indice-atributo))]
               (if (= [**] t) atribs t))
        tests (map (fn [v] (remove #{v} test)) test)
        tests (if (empty? tests) [[]] tests)]
    (map (partial assoc concepto-CL indice-atributo) tests)))

(he-tardado 60 2.9)

;; Ejercicio 2.10
(defn generalizaciones-atributo-nominal
  "Devuelva una lista de generalizaciones inmediatas del concepto en el atributo
  referenciado por el índice"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        attribute-values (second (nth metadatos indice-atributo))
        remaining-values (remove (partial match-nominal? test) attribute-values)
        tests (cond (empty? remaining-values) [[**]]
                    (= 1 (count remaining-values)) [[**]]
                    :else (map (fn [v] (conj test v)) remaining-values))]
    (map (partial assoc concepto-CL indice-atributo) tests)))

(he-tardado 60 2.10)

;; Ejercicio 2.11
(defn generalizacion-atributo-numerico
  "Devuelve la generalición inmediata del concepto que incluye al ejemplo
   para el atributo referenciado por el índice.
   Si el ejemplo es negativo o el concepto ya incluye el ejemplo, devuelve el
   concepto tal cual lo recibe."
  [concepto-CL indice-atributo ejemplo]
  (if (= -- (last ejemplo)) concepto-CL
      (let [test (nth concepto-CL indice-atributo)
            atributo (nth ejemplo indice-atributo)]
        (if (match-numerico? test atributo) concepto-CL
            (let [[a b :as t] (normalize-numerico test)
                  gener (cond (= [] t) [atributo]
                              (= [**] t) test
                              (extremo<= atributo a) (normalize-numerico [[atributo] b])
                              (extremo<= b atributo) (normalize-numerico [a [atributo]])
                              :else test)]
              (assoc concepto-CL indice-atributo gener))))))

;; Ejercicio 2.12
(defn especializaciones-atributo-numerico
  "Devuelve la lista de las especializaciones inmediatas del concepto que excluyen
   el ejemplo en el índice indicado.
   Si el ejemplo es positivo o el concepto ya excluye el ejemplo, devuelve una lista
   cuyo único elemento es el concepto."
  [concepto-CL indice-atributo ejemplo]
  (if (= ++ (last ejemplo)) [concepto-CL]
      (let [test (nth concepto-CL indice-atributo)]
        (if-not (match-numerico? test atributo) [concepto-CL]
                (let [[a b :as t] (normalize-numerico test)
                      atributo (nth ejemplo indice-atributo)
                      specs (cond (= [] t) [[]]
                                  (= [**] t) [[-inf atributo] [atributo +inf]]
                                  :else [(normalize-numerico [a atributo])
                                         (normalize-numerico [atributo b])])]
                  (map (partial assoc concepto-CL indice-atributo) specs))))))

(defn cartesian-product
  "Devuelve el producto cartesiano de una lista de listas"
  [colls]
  (if (empty? colls) [[]]
      (for [x (first colls) more (cartesian-product (rest colls))]
        (cons x more))))

;; Ejercicio 2.13
(defn generalizaciones-CL
  "Devuelve el conjunto total de generalizaciones inmediatas (respecto a todos los
   atributos) del concepto para el ejemplo.
   Si el ejemplo es negativo, devuelve el concepto en una lista tal cual"
  ([concepto-CL metadatos ejemplo indice]
   (if (= numerico (second (nth metadatos indice)))
     [(generalizacion-atributo-numerico concepto-CL indice ejemplo)]
     (generalizaciones-atributo-nominal concepto-CL indice metadatos)))
  ([concepto-CL metadatos ejemplo]
   (if (= -- (last ejemplo)) [concepto-CL]
       (let [geners (->> (range (dec (count metadatos)))
                         (mapcat (partial generalizaciones-CL concepto-CL metadatos ejemplo))
                         distinct
                         (remove (partial = concepto-CL)))]
         (if (empty? geners) [concepto-CL] geners)))))
;; (cartesian-product (for [i (range (dec (count metadatos)))] (map (fn [g] (nth g i)) (generalizaciones-CL concepto-CL metadatos ejemplo i)

;; Ejercicio 2.14
(defn especializaciones-CL
  "Devuelve el conjunto total de especializaciones inmediatas (respecto a todos los
   atributos) del concepto para el ejemplo
   Si el ejemplo es positivo, devuelve el concepto en una lista tal cual"
  ([concepto-CL metadatos ejemplo indice]
   (if (= numerico (second (nth metadatos indice)))
     (especializaciones-atributo-numerico concepto-CL indice ejemplo)
     (especializaciones-atributo-nominal concepto-CL indice metadatos)))
  ([concepto-CL metadatos ejemplo]
   (if (= ++ (last ejemplo)) [concepto-CL]
       (let [specs (->> (range (dec (count metadatos)))
                        (mapcat (partial especializaciones-CL concepto-CL metadatos ejemplo))
                        distinct
                        (remove (partial = concepto-CL)))]
         (if (empty? specs) [concepto-CL] specs)))))
;; (cartesian-product (for [i (range (dec (count metadatos)))] (map (fn [e] (nth e i)) (especializaciones-CL concepto-CL metadatos ejemplo i))))
