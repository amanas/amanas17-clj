(ns amanas17.maia.p2-2
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]))


(defn test-ambivalente>=
  "Indica si un test ambivalente es más general o de la misma categoría
  que otro"
  [test1 test2]
  (or (= [**] test1)
      (= [] test2)))

(defn extremo<=
  "Compara extremos de intervalos"
  [a b]
  (cond (= -inf a)          true
        (= +inf b)          true
        (every? coll? [a b]) (comp<= (first a) (first b))
        (coll? a)            (comp<= (first a) b)
        (coll? b)            (comp< a (first b))
        :else                (comp<= a b)))

(defn test-contenido?
  "Determina si un test numérico está contenido en otro"
  [t1 t2]
  (let [[a b :as t1] (normalize-numerico t1)
        [c d :as t2] (normalize-numerico t2)]
    (or (= [] t1)
        (= [**] t2)
        (and (extremo<= c a)
             (extremo<= b d)))))

(defn test-numerico>=
  "Indica si un test numérico es más general o de la misma categoría
  que otro"
  [t1 t2]
  (or (test-contenido? t2 t1)
      (not (test-contenido? t1 t2))))

(defn test-nominal>=
  "Indica si un test nominal es más general o de la misma categoría
  que otro"
  [t1 t2]
  (or (every? (set t1) t2)
      (not (every? (set t2) t1))))

;; Ejercicio 2.6
(defn test-CL>=
  "Indica si un test es más general o de la misma categoría que otro"
  ([t1 t2] (cond (some test-ambivalente? [t1 t2]) (test-ambivalente>= t1 t2)
                 (every? test-numerico? [t1 t2]) (test-numerico>= t1 t2)
                 (every? test-nominal? [t1 t2]) (test-nominal>= t1 t2)
                 :else "Comparación no soportada")))

(he-tardado 60 2.6)

;; Ejercicio 2.7
(defn concepto-CL>=
  "Indica si todos los tests de un concepto tienen mayor o igual
  generalidad que los de otro"
  [c1 c2]
  (every? true? (map (partial apply test-CL>=) (partition 2 (interleave c1 c2)))))

(he-tardado 5 2.7)

(defn concepto-CL= [c1 c2] (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1)))
(defn concepto-CL<= [c1 c2] (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1)))

;; Ejercicio 2.8
(defn cmp-concepto-CL
  "Compara dos conceptos. Devuelve:
  1 si c1 es estrictamente más general que c2
  0 si c1 es estrictamente de la misma categoría que c2
  -1 si c1 es estrictamente más específico que c2"
  [c1 c2]
  (cond (and (concepto-CL>= c1 c2) (not (concepto-CL>= c2 c1)))  1
        (and (not (concepto-CL>= c1 c2)) (concepto-CL>= c2 c1)) -1
        :else 0))

(he-tardado 60 2.8)
