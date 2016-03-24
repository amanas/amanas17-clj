(ns amanas17.maia.p2-3
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]))

;; Ejercicio 2.9
(defn especializaciones-atributo-nominal
  "Devuelva una lista las especializaciones inmediatas del concepto en el atributo
   referenciado por el índice.
   Si no hay especializaciones, devuelve el concepto en una lista"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        all-values (second (nth metadatos indice-atributo))]
    (cond (= test []) [concepto-CL]
          (= test [**]) (especializaciones-atributo-nominal
                         (assoc concepto-CL indice-atributo all-values)
                         indice-atributo metadatos)
          :else (->> (for [v test] (remove #{v} test))
                     vec (map (partial assoc concepto-CL indice-atributo))))))

(he-tardado 60 2.9)

;; Ejercicio 2.10
(defn generalizaciones-atributo-nominal
  "Devuelva una lista de generalizaciones inmediatas del concepto en el atributo
   referenciado por el índice
   Si no hay generalizaciones, devuelve el concepto en una lista"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        all-values (second (nth metadatos indice-atributo))
        remaining-values (remove (partial match-nominal? test) all-values)]
    (cond (<= (count remaining-values) 1) [(assoc concepto-CL indice-atributo [**])]
          :else (->> (for [r remaining-values] (conj test r))
                     vec (map (partial assoc concepto-CL indice-atributo))))))

(he-tardado 60 2.10)

;; Ejercicio 2.11
(defn generalizacion-atributo-numerico
  "Devuelve la generalición inmediata del concepto que incluye al ejemplo
   para el atributo referenciado por el índice.
   Si el ejemplo es negativo o el concepto ya incluye el ejemplo, devuelve el
   concepto tal cual lo recibe."
  [concepto-CL indice-atributo ejemplo]
  (if (= - (last ejemplo)) concepto-CL
      (let [test (nth concepto-CL indice-atributo)
            atributo (nth ejemplo indice-atributo)]
        (if (match-numerico? test atributo) concepto-CL
            (let [[a b :as t] (normalize-numerico test)
                  gener (cond (= [] t) [atributo]
                              (= [**] t) t
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
  (if (= + (last ejemplo)) [concepto-CL]
      (let [test (nth concepto-CL indice-atributo)
            atributo (nth ejemplo indice-atributo)]
        (if-not (match-numerico? test atributo) [concepto-CL]
                (let [[a b :as t] (normalize-numerico test)
                      specs (cond (= [] t) [t]
                                  (= [**] t) [[-inf atributo] [atributo +inf]]
                                  :else [(normalize-numerico [a atributo])
                                         (normalize-numerico [atributo b])])]
                  (map (partial assoc concepto-CL indice-atributo) specs))))))

;; Ejercicio 2.13
(defn generalizaciones-CL
  "Devuelve el conjunto total de generalizaciones inmediatas (respecto a todos los
   atributos) del concepto para el ejemplo.
   Si el ejemplo es negativo, devuelve el concepto tal cual en una lista"
  ([concepto-CL metadatos ejemplo indice]
   (if (= numerico (second (nth metadatos indice)))
     [(generalizacion-atributo-numerico concepto-CL indice ejemplo)]
     (generalizaciones-atributo-nominal concepto-CL indice metadatos)))
  ([concepto-CL metadatos ejemplo]
   ;; todo: no estoy completamente seguro de que haya que hacer esta compobación
   (if (or (= - (last ejemplo))
           (match-CL concepto-CL (butlast ejemplo)))
     [concepto-CL]
     (->> (range (dec (count metadatos)))
          (mapcat (partial generalizaciones-CL concepto-CL metadatos ejemplo))
          (remove (partial = concepto-CL))
          distinct)) ))

;; Ejercicio 2.14
(defn especializaciones-CL
  "Devuelve el conjunto total de especializaciones inmediatas (respecto a todos los
   atributos) del concepto para el ejemplo.
   Si el ejemplo es positivo o ya es rechazado por el concepto,
   devuelve el concepto tal cual en una lista"
  ([concepto-CL metadatos ejemplo indice]
   (if (= numerico (second (nth metadatos indice)))
     (especializaciones-atributo-numerico concepto-CL indice ejemplo)
     (especializaciones-atributo-nominal concepto-CL indice metadatos)))
  ([concepto-CL metadatos ejemplo]
   ;; todo: no estoy completamente seguro de que haya que hacer esta compobación
   (if (or (= + (last ejemplo))
           (not (match-CL concepto-CL (butlast ejemplo))))
     [concepto-CL]
     (->> (range (dec (count metadatos)))
          (mapcat (partial especializaciones-CL concepto-CL metadatos ejemplo))
          (remove (partial = concepto-CL))
          distinct))))
