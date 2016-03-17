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


(defn incluye-atributo-nominal
  "Incluye en el concepto el atributo del ejemplo en el índice indicado"
  [concepto-CL indice metadatos ejemplo]
  (let [test (nth concepto-CL indice)]
    (if (= [*] test) concepto-CL
      (let [atribs (distinct (conj test (nth ejemplo indice)))
            new-test (if (= (count atribs) (count (second (nth metadatos indice)))) [*] atribs)]
        (concat (take indice concepto-CL) [new-test] (drop (inc indice) concepto-CL))))))

;; Ejercicio 2.10
(defn generalizaciones-atributo-nominal
  "Devuelva una lista de todos los conceptos CL que sean generalización inmediata
  de concepto-CL en el atributo referenciado mediante indice-atributo"
  ([concepto-CL indice-atributo metadatos]
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
  ([concepto-CL indice metadatos ejemplo]
   (if (= :- (last ejemplo)) [concepto-CL]
       (let [concepto-CL (incluye-atributo-nominal concepto-CL indice metadatos ejemplo)
             test (nth concepto-CL indice)
             atributo (nth ejemplo indice)]
         (generalizaciones-atributo-nominal concepto-CL indice metadatos)))))

(he-tardado 60 2.10)

;; Ejercicio 2.11
(defn generalizacion-atributo-numerico
  "Devuelve la generalización inmediata del concepto que admite el ejemplo
  si es positivo en el índice indicado.
  Si el ejemplo es negativo o ya es admitido por el concepto, devuelve el
  concepto tal cual se recibe"
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
  "Devuelve una lista de especializaciones inmediatas del concepto que rechazan
  el ejemplo si es negativo en el índice indicado.
  Si el ejemplo es positivo o ya es rechazado por el concepto, devuelve
  una lista con el concepto tal cual se recibe"
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

(defn cartesian-product
  "Devuelve el producto cartesiano de una lista de listas"
  [colls]
  (if (empty? colls) [[]]
    (for [x (first colls) more (cartesian-product (rest colls))]
      (cons x more))))

;; Ejercicio 2.13
(defn generalizaciones-CL
  "Devuelve la lista de generalizacines inmediatas del concepto para el ejemplo
  en todos sus atributos si el ejemplo es positivo.
  Si el ejemplo es negativo o ya es admitido por el concepto, devuelve
  una lista con el concepto tal cual se recibe"
  ([concepto-CL metadatos ejemplo indice]
   (if (= :numerico (second (nth metadatos indice)))
     [(nth (generalizacion-atributo-numerico concepto-CL indice ejemplo) indice)]
     (map (fn[g] (nth g indice))
          (generalizaciones-atributo-nominal concepto-CL indice metadatos ejemplo))))
  ([concepto-CL metadatos ejemplo]
   (->> metadatos count range butlast
        (map (partial generalizaciones-CL concepto-CL metadatos ejemplo))
        cartesian-product)))


;; Ejercicio 2.14
(defn especializaciones-CL
  "Devuelve la lista de especializaciones inmediatas del concepto para el ejemplo
  en todos sus atributos si el ejemplo es positivo.
  Si el ejemplo es negativo o ya es rechazado por el concepto, devuelve
  una lista con el concepto tal cual se recibe"
  ([concepto-CL metadatos ejemplo indice]
   (if (= :numerico (second (nth metadatos indice)))
     (nth (especializaciones-atributo-numerico concepto-CL indice ejemplo) indice)
     (let [concepto-CL (incluye-atributo-nominal concepto-CL indice ejemplo metadatos)
           test (nth concepto-CL indice)
           atributo (nth ejemplo indice)
           geners (if (match-nominal? test atributo)
                    [concepto-CL]
                    (generalizaciones-atributo-nominal concepto-CL indice metadatos))]
       (map (fn[g] (nth g indice)) geners))))
  ([concepto-CL metadatos ejemplo]
   (->> metadatos count range butlast
        (map (partial especializaciones-CL concepto-CL metadatos ejemplo))
        cartesian-product)))
