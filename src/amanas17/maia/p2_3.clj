(ns amanas17.maia.p2-3
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representaciones CL (especializaciones/generalizaciones) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 2.9
(defn especializaciones-atributo-nominal
  "Devuelva una lista las especializaciones inmediatas del concepto en el atributo
   referenciado por el índice.
   Si no hay especializaciones, devuelve el concepto en una lista"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        all-values (second (nth metadatos indice-atributo))]
    (if (= test '(*))
      (map #(assoc (vec concepto-CL) indice-atributo [%]) all-values)
      [(assoc (vec concepto-CL) indice-atributo [])])))


(he-tardado 60 2.9)

;; Ejercicio 2.10
(defn generalizaciones-atributo-nominal
  "Devuelva una lista de generalizaciones inmediatas del concepto en el atributo
   referenciado por el índice
   Si no hay generalizaciones, devuelve el concepto en una lista"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        all-values (second (nth metadatos indice-atributo))]
    (if (= test [])
      (map #(assoc (vec concepto-CL) indice-atributo [%]) all-values)
      [(assoc (vec concepto-CL) indice-atributo '(*))])))

(he-tardado 60 2.10)

;; Ejercicio 2.11
(defn generalizacion-atributo-numerico
  "Devuelve la generalición inmediata del concepto que incluye al ejemplo
   para el atributo referenciado por el índice.
   Si el ejemplo es negativo o el concepto ya incluye el ejemplo, devuelve el
   concepto tal cual lo recibe."
  [concepto-CL indice-atributo ejemplo]
  (if (= '- (last ejemplo))
    concepto-CL
    (let [test (nth concepto-CL indice-atributo)
          atributo (nth ejemplo indice-atributo)]
      (if (match-numerico? test atributo)
        concepto-CL
        (let [[a b :as t] (normalize-numerico test)
              gener (cond (= '() t) [atributo]
                          (= '(*) t) t
                          (extremo<= atributo a) (normalize-numerico [[atributo] b])
                          (extremo<= b atributo) (normalize-numerico [a [atributo]])
                          :else test)]
          (assoc (vec concepto-CL) indice-atributo gener))))))

(he-tardado 120 2.11)

;; Ejercicio 2.12
(defn especializaciones-atributo-numerico
  "Devuelve la lista de las especializaciones inmediatas del concepto que excluyen
   el ejemplo en el índice indicado.
   Si el ejemplo es positivo o el concepto ya excluye el ejemplo, devuelve una lista
   cuyo único elemento es el concepto."
  [concepto-CL indice-atributo ejemplo]
  (if (= '+ (last ejemplo))
    [concepto-CL]
    (let [test (nth concepto-CL indice-atributo)
          atributo (nth ejemplo indice-atributo)]
      (if-not (match-numerico? test atributo)
        [concepto-CL]
        (let [[a b :as t] (normalize-numerico test)
              specs (cond (= '() t) [t]
                          (= '(*) t) [['-inf atributo] [atributo '+inf]]
                          :else [(normalize-numerico [a atributo])
                                 (normalize-numerico [atributo b])])]
          (map (partial assoc (vec concepto-CL) indice-atributo) specs))))))

(he-tardado 90 2.12)

;; Ejercicio 2.13
(defn generalizaciones-CL
  "Devuelve el conjunto total de generalizaciones inmediatas (respecto a todos los
   atributos) del concepto para el ejemplo.
   Si el ejemplo es negativo, devuelve el concepto tal cual en una lista"
  ([concepto-CL metadatos ejemplo indice]
   (if (= 'numerico (second (nth metadatos indice)))
     [(generalizacion-atributo-numerico concepto-CL indice ejemplo)]
     (generalizaciones-atributo-nominal concepto-CL indice metadatos)))
  ([concepto-CL metadatos ejemplo]
   (if (or (= '- (last ejemplo))
           (match-CL concepto-CL (butlast ejemplo)))
     [concepto-CL]
     (->> (range (dec (count metadatos)))
          (mapcat (partial generalizaciones-CL concepto-CL metadatos ejemplo))
          (remove (partial = concepto-CL))
          distinct))))

(he-tardado 70 2.13)

;; Ejercicio 2.14
(defn especializaciones-CL
  "Devuelve el conjunto total de especializaciones inmediatas (respecto a todos los
   atributos) del concepto para el ejemplo.
   Si el ejemplo es positivo o ya es rechazado por el concepto,
   devuelve el concepto tal cual en una lista"
  ([concepto-CL metadatos ejemplo indice]
   (if (= 'numerico (second (nth metadatos indice)))
     (especializaciones-atributo-numerico concepto-CL indice ejemplo)
     (especializaciones-atributo-nominal concepto-CL indice metadatos)))
  ([concepto-CL metadatos ejemplo]
   (if (or (= '+ (last ejemplo))
           (not (match-CL concepto-CL (butlast ejemplo))))
     [concepto-CL]
     (->> (range (dec (count metadatos)))
          (mapcat (partial especializaciones-CL concepto-CL metadatos ejemplo))
          (remove (partial = concepto-CL))
          distinct))))
          
(he-tardado 90 2.14)
