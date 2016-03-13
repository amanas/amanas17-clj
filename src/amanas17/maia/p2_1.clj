(ns amanas17.maia.p2-1
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]))

;; Ejercicios 1 a 5 de la práctica 2

;; Por simplicidad defino los valores -infinito y +infinito como keyworkds
;; :-inf y :+inf

(defn- test-ambivalente?
  "Determina si un test puede ser aplicado indistintamente
  a un atributo numérico o nominal"
  [test]
  (or (= [] test)
      (= [*] test)))

(defn- test-numerico?
  "Determina si un test es aplicable a un atributo numérico"
  [[v1 v2 :as test]]
  (or (test-ambivalente? test)
      (and ((some-fn :-inf :+inf number?) (if (coll? v1) (first v1) v1))
           ((some-fn :-inf :+inf number? nil?) (if (coll? v2) (first v2) v2)))))

(defn- test-nominal?
  "Determina si un test es aplicable a un atributo nominal"
  [test]
  (or (test-ambivalente? test)
      (and (not (some #{:-inf :+inf} test))
           (every? keyword? test))))

(defn- match-nature?
  "Determina si un test (o array de ellos) y un atributo (o array de ellos)
  tienen la misma naturaleza (nominal o numérica)"
  ([[tests atributos]]
   (->> (interleave tests atributos)
        (partition 2)
        (map (partial apply match-nature?))
        (every? true?)))
  ([test atributo]
   (or (test-ambivalente? test)
       (and (test-numerico? test) (number? atributo))
       (and (test-nominal? test) (keyword? atributo)))))

(defn- match-ambivalente?
  "Determina si un atributo satisface un test ambivalente"
  [test atributo]
  (= [*] test))

(defn- match-nominal?
  "Determina si un atributo nominal satisface un test nominal"
  [test atributo]
  (or (match-ambivalente? test atributo)
      (not (nil? (some (set test) [atributo])))))

(defn- comp<=
  "Función auxiliar que permite hacer comparaciones habida cuenta de que ahora
  :-inf y :+inf tienen que poderse comparar con números.
  Sobre esta función recae en última instancia el orden parcial de los tests
  numéricos"
  [a b]
  (cond (= :-inf a)            true
        (= :+inf b)            true
        (every? number? [a b]) (<= a b)
        :else                  false))
(defn- comp=  [a b] (and (comp<= a b) (comp<= b a)))
(defn- comp<  [a b] (and (comp<= a b) (not (comp= a b))))
(defn- comp>  [a b] (not (comp<= a b)))
(defn- comp>= [a b] (not (comp<  a b)))

(defn- normalize-numerico
  "Los tests numéricos admiten demasiada variedad en su forma de
  ser expresados.
  Ésta función los encapsula en una estructura común que puede adoptar
  cualquiera de estas formas:
  - []
  - [*]
  - [a b]
  - [[a] b]
  - [a [b]]
  - [[a] [b]]
  siendo a y b números o :-inf, :+inf"
  [[a b :as t]]
  (cond (= [] t)             []
        (= [*] t)            [*]
        (= [:-inf] a)        (normalize-numerico [:-inf b])
        (= [:+inf] b)        (normalize-numerico [a :+inf])
        (= [:-inf :+inf] t)  [*]
        (nil? b)             (normalize-numerico [a a])
        (every? coll? [a b]) (cond (comp= (first a) (first b)) [(first a) (first b)]
                                   (comp< (first a) (first b)) t
                                   :else                       [])
        (coll? a)            (if (comp< (first a) b) t [])
        (coll? b)            (if (comp< a (first b)) t [])
        (comp<= a b)         t
        :else                []))

(defn- match-numerico?
  "Determina si un atributo numérico satisface un test numérico"
  [test v]
  (let [[a b :as t] (normalize-numerico test)]
    (or (match-ambivalente? t v)
        (= a b v)
        (and (if (coll? a) (comp<= (first a) v) (comp< a v))
             (if (coll? b) (comp<= v (first b)) (comp< v b))))))

(defn- match?
  "Determina si un test (o array de ellos) y un atributo (o array de ellos)
  son satisfechos"
  ([[tests atributos]]
   (->> (interleave tests atributos)
        (partition 2)
        (map (partial apply match?))
        (every? true?)))
  ([test atributo]
   (cond (test-ambivalente? test) (match-ambivalente? test atributo)
         (test-numerico? test) (match-numerico? test atributo)
         (test-nominal? test)  (match-nominal? test atributo)
         :else false)))

;; Ejercicio 1
(defn match-CL
  "Determina si un conjunto de atributos satisfacen un conjunto de tests"
  [[& tests :as  concepto]
   [& atributos :as ejemplo-sin-clase]]
  (and (= (count tests) (count atributos))
       (match-nature? [tests atributos])
       (match? [tests atributos])))

(assert (and (not (match-CL [[:soleado] [*] [10 40] [:si]]   [:soleado 30 40 :si]))
             (match-CL [[:soleado] [*] [10 [40]] [:si]] [:soleado 30 40 :si])
             (not (match-CL [[:soleado] [*] [10 40] [:si]]   [:soleado 30 25 :no]))
             (not (match-CL [[:soleado] [*] [10 40] [:si]]   [30 :soleado 25 :no]))))

(he-tardado 360 2.1)


;; Ejercicio 2
(defn CLi
  "Intérprete basado en conjunciones lógicas"
  [concepto ejemplo-sin-clase]
  (concat ejemplo-sin-clase [(if (match-CL concepto ejemplo-sin-clase) :+ :-)]))

;; Nota:
;; hay un error tipográfico en este ejemplo en el material de estudio
(assert (= [:soleado 30 40 :si :+]
           (CLi [[:soleado] [*] [10 [40]] [:si]] [:soleado 30 40 :si])))

(he-tardado 45 2.2)


;; Ejercicio 3
;; Mi entendimiento de la lectura del enunciado es que este ejercicio
;; no implica programar sino simplemente enunciar los coceptos más
;; incluyentes y excluyentes.

;; Como mis ejemplos tienen 7 atributos más la clase, debería ser
(def concepto-mas-general-posible [[*] [*] [*] [*] [*] [*] [*]])
(def concepto-mas-especifico-posible [[] [] [] [] [] [] []])
;; Un concepto que para mí podría suponer un buen día para salir al campo es
(def concepto-mas-cercano-para-mi [[:soleado]
                                   [20 30]
                                   [60 80]
                                   [:brisa :no]
                                   [:contento :normal]
                                   [*]
                                   [:solvente :ajustado]])

(he-tardado 20 2.3)


;; Ejercicio 4
(defn concepto-CL-mas-general
  "Devuelve el concepto CL más general posible dada la descripción de
  atributos de un conjunto de datos.
  Metadatos se entiende que es la cabecera de descripción de atributos"
  [metadatos]
  (repeat (dec (count metadatos)) [*]))

(assert (= [[*] [*] [*] [*] [*] [*] [*]]
           (concepto-CL-mas-general (first ejemplos))))

(he-tardado 20 2.5)

;; Ejercicio 5
(defn concepto-CL-mas-especifico
  "Devuelve el concepto CL más específico posible dada la descripción de
  atributos de un conjunto de datos.
  Metadatos se entiende que es la cabecera de descripción de atributos"
  [metadatos]
  (repeat (dec (count metadatos)) []))

(assert (= [[] [] [] [] [] [] []]
           (concepto-CL-mas-especifico (first ejemplos))))

(he-tardado 3 2.6)


(defn- test-ambivalente>=
  "Indica si un test ambivalente es más general o de la misma categoría
  que otro"
  [test1 test2]
  (or (= [*] test1)
      (= [] test2)))

(defn- extremo<=
  "Compara extremos de intervalos"
  [a b]
  (cond (= :-inf a)          true
        (= :+inf b)          true
        (every? coll? [a b]) (comp<= (first a) (first b))
        (coll? a)            (comp<= (first a) b)
        (coll? b)            (comp< a (first b))
        :else                (comp<= a b)))

(defn- test-contenido?
  "Determina si un test numérico está contenido en otro"
  [t1 t2]
  (let [[a b :as t1] (normalize-numerico t1)
        [c d :as t2] (normalize-numerico t2)]
    (or (= [] t1)
        (= [*] t2)
        (and (extremo<= c a)
             (extremo<= b d)))))

(defn- test-numerico>=
  "Indica si un test numérico es más general o de la misma categoría
  que otro"
  [t1 t2]
  (or (test-contenido? t2 t1)
      (not (test-contenido? t1 t2))))

(defn- test-nominal>=
  "Indica si un test nominal es más general o de la misma categoría
  que otro"
  [t1 t2]
  (or (every? (set t1) t2)
      (not (every? (set t2) t1))))

;; Ejercicio 6
(defn test-CL>=
  "Indica si un test es más general o de la misma categoría que otro"
  ([[t1 t2]] (test-CL>= t1 t2))
  ([t1 t2] (cond (some test-ambivalente? [t1 t2]) (test-ambivalente>= t1 t2)
                 (every? test-numerico? [t1 t2]) (test-numerico>= t1 t2)
                 (every? test-nominal? [t1 t2]) (test-nominal>= t1 t2)
                 :else "Comparación no soportada")))

(assert (and (test-CL>= [*] [*])
             (test-CL>= [:lluvioso] [:soleado])
             (test-CL>= [:lluvioso] [])
             (not (test-CL>=  []   [:lluvioso]))
             (test-CL>= [25 30] [26])
             (test-CL>=  [25 30] [21 25])
             (not (test-CL>= [26] [25 30]))))

(he-tardado 60 2.6)

;; Ejercicio 7
(defn concepto-CL>=
  "Indica si todos los tests de un concepto son tienen mayor o igual
  generalidad que los de otro"
  [c1 c2]
  (every? test-CL>= (partition 2 (interleave c1 c2))))

(assert (and (concepto-CL>= [[:lluvioso] [2 10]] [[:soleado] [10 20]])
             (concepto-CL>= [[:lluvioso] [2 35]] [[:soleado] [23]])
             (not (concepto-CL>= [[:soleado][23]] [[:lluvioso][2 35]]))))

(he-tardado 5 2.7)

(defn- concepto-CL= [c1 c2] (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1)))
(defn- concepto-CL<= [c1 c2] (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1)))

;; Ejercicio 8
(defn cmp-concepto-CL
  "Compara dos conceptos. Devuelve:
  1 si c1 es estrictamente más general que c2
  0 si c1 es estrictamente de la misma categoría que c2
  -1 si c1 es estrictamente más específico que c2"
  [c1 c2]
  (cond (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1))        0
        (and (concepto-CL>= c1 c2) (not (concepto-CL>= c2 c1)))  1
        (and (not (concepto-CL>= c1 c2)) (concepto-CL>= c2 c1)) -1))

(assert (and (=  1 (cmp-concepto-CL [[:lluvioso] [2 35]] [[:soleado]  [23]]))
             (=  0 (cmp-concepto-CL [[:lluvioso] [2 10]] [[:soleado]  [10 20]]))
             (= -1 (cmp-concepto-CL [[:soleado]  [23]]   [[:lluvioso] [2 35]]))))

;; Ejercicio 9
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

;; creo que el ejemplo del material de estudio está mal
(assert (= [[[:soleado :lluvioso :niebla :diluvia][*][20][:si]]
            [[:nublado :lluvioso :niebla :diluvia][*][20][:si]]
            [[:nublado :soleado :niebla :diluvia][*][20][:si]]
            [[:nublado :soleado :lluvioso :diluvia][*][20][:si]]
            [[:nublado :soleado :lluvioso :niebla][*][20][:si]]]
           (especializaciones-atributo-nominal [[*][*][20][:si]] 0 (first ejemplos))))
(assert (= [[[][*][20][:si]]]
           (especializaciones-atributo-nominal [[:soleado][*][20][:si]] 0 [first ejemplos])))
(assert (= [[[][*][20][:si]]]
           (especializaciones-atributo-nominal [[][*][20][:si]] 0 [first ejemplos])))

(he-tardado 60 2.9)

;; Ejercicio 10
(defn generalizaciones-atributo-nominal
  "Devuelva una lista de todos los conceptos CL que sean generalización inmediata
  de concepto-CL en el atributo referenciado mediante indice-atributo"
  [concepto-CL indice-atributo metadatos]
  (let [test (nth concepto-CL indice-atributo)
        attribute-values (second (nth metadatos indice-atributo))
        remaining-values (remove (partial match-nominal? test) attribute-values)
        geners (map (fn [v] (concat test [v])) remaining-values)
        geners (if (empty? geners) [[*]] geners)]
    (map (fn [gener] (concat (take indice-atributo concepto-CL)
                             [gener]
                             (drop (inc indice-atributo) concepto-CL)))
         geners)))

(assert (= [[[:soleado][*][20][:fuerte]]
            [[:soleado][*][20][:brisa]]
            [[:soleado][*][20][:no]]]
           (prn(generalizaciones-atributo-nominal [[:soleado][*][20][]] 3 (first ejemplos)))))
(assert (= [[[:soleado][*][20][:si :fuerte]]
            [[:soleado][*][20][:si :brisa]]
            [[:soleado][*][20][:si :no]]]
           (generalizaciones-atributo-nominal [[:soleado][*][20][:si]] 3 (first ejemplos))))
(assert (= [[[:soleado][*][20][*]]]
           (generalizaciones-atributo-nominal [[:soleado][*][20][*]] 3 (first ejemplos))))
(he-tardado 60 2.10)

;; Ejercicio 11
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

(assert (= [[:soleado][25][20][:si]]
           (generalizacion-atributo-numerico [[:soleado][][20][:si]] 1
                                             [:soleado 25 40 :si :+])))
(assert (= [[:soleado][][20][:si]]
           (generalizacion-atributo-numerico [[:soleado][][20][:si]] 1
                                             [:soleado 25 40 :si :-])))
(assert (= [[:soleado][15 30][20][:si]]
           (generalizacion-atributo-numerico [[:soleado][15 30][20][:si]] 1
                                             [:soleado 25 40 :si :+])))

;; Ejercicio 12
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
