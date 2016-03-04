(ns amanas17.maia.p2-1
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]))

;; Ejercicios 1 a 5 de la práctica 2

;; Por simplicidad defino los valores -infinito y +infinito como keyworkds
;; :-inf
;; :+inf

(defn- limite-numerico?
  "Determina si el límite indicado en un test numérico es válido"
  [l]
  (if (coll? l)
    (number? (first l))
    (or (= :-inf l)
        (= :+inf l)
        (number? l))))

(assert (->> [-1 0 1 :-inf :+inf [-1] [0] [1]]
             (every? limite-numerico?)))

(defn- test-ambivalente?
  "Determina si un test puede ser aplicado indistintamente
   a un atributo numérico o nominal"
  [test]
  (or (= [] test)
      (= [*] test)))

(assert (every? test-ambivalente? [[] [*]]))

(defn- test-numerico?
  "Determina si un test es aplicable a un atributo numérico"
  [test]
  (and (coll? test)
       (or (test-ambivalente? test)
           (every? limite-numerico? test))))

(assert (every? test-numerico? [[] [*] [1] [1 2] [[1] 2] [[1] [2]] [1 [2]]]))

(defn- test-nominal?
  "Determina si un test es aplicable a un atributo nominal"
  [test]
  (or (test-ambivalente? test)
      (not (test-numerico? test))))

(assert (every? test-nominal? [[] [*] [:a] [:a :b]]))

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

(assert (match-nature? [] 1))
(assert (match-nature? [*] 1))
(assert (match-nature? [1] 1))
(assert (match-nature? [1 2] 1))
(assert (match-nature? [[1]] 1))
(assert (match-nature? [[1] 2] 1))
(assert (match-nature? [1 [2]] 1))
(assert (match-nature? [[1] [2]] 1))
(assert (match-nature? [:a] :b))
(assert (match-nature? [:a :b] :c))
(assert (match-nature? [] :a))
(assert (match-nature? [*] :a))

(defn- match-ambivalente?
  "Determina si un atributo satisface un test ambivalente"
  [test atributo]
  (= [*] test))

(defn- match-nominal?
  "Determina si un atributo nominal satisface un test nominal o ambivalente"
  [test atributo]
  (or (match-ambivalente? test atributo)
      (not (nil? (some (set test) [atributo])))))

(assert (match-nominal? [:a] :a))
(assert (match-nominal? [:a :b] :a))
(assert (not (match-nominal? [:a :b] :c)))

(defn- match-numerico?
  "Determina si un atributo numérico satisface un test numérico o ambivalente"
  [test atributo]
  (or (match-ambivalente? test atributo)
      (and (= 1 (count test))
           (= (first test) atributo))
      (and (= 2 (count test))
           (let [[a b] test
                 left (cond (coll? a)   (partial <= (first a))
                            (= :-inf a) (constantly true)
                            (= :+inf a) (constantly false)
                            :else       (partial < a))
                 right (cond (coll? b)   (partial >= (first b))
                             (= :-inf b) (constantly false)
                             (= :+inf b) (constantly true)
                             :else       (partial > b))]
             (and (left atributo) (right atributo))))))

(assert (not (match-numerico? [] 1)))
(assert (match-numerico? [*] 1))
(assert (match-numerico? [1] 1))
(assert (match-numerico? [1 2] 1.5))
(assert (match-numerico? [[1] 2] 1))
(assert (match-numerico? [1 [2]] 2))
(assert (match-numerico? [[1] [2]] 1))


(defn- match?
  "Determina si un test (o array de ellos) y un atributo (o array de ellos)
   son satisfechos"
  ([[tests atributos]]
   (->> (interleave tests atributos)
        (partition 2)
        (map (partial apply match?))
        (every? true?)))
  ([test atributo]
   (cond (test-numerico? test) (match-numerico? test atributo)
         (test-nominal? test) (match-nominal? test atributo)
         :else (match-ambivalente? test atributo))))

;; Ejercicio 1
(defn match-CL
  "Determina si un conjunto de atributos satisfacen un conjunto de tests"
  [[& tests :as  concepto]
   [& atributos :as ejemplo-sin-clase]]
  (and (= (count tests) (count atributos))
       (match-nature? [tests atributos])
       (match? [tests atributos])))

(assert (not (match-CL [[:soleado] [*] [10 40] [:si]]   [:soleado 30 40 :si])))
(assert      (match-CL [[:soleado] [*] [10 [40]] [:si]] [:soleado 30 40 :si]))
(assert (not (match-CL [[:soleado] [*] [10 40] [:si]]   [:soleado 30 25 :no])))
(assert (not (match-CL [[:soleado] [*] [10 40] [:si]]   [30 :soleado 25 :no])))

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

(assert (test-ambivalente>= [*] [*]))
(assert (test-ambivalente>= [*] []))
(assert (test-ambivalente>= [] []))
(assert (not (test-ambivalente>= [] [*])))

(defn- limite=
  "Indica si un límite de un intervalo es igual.
   Pueden ser cerrados '[number]' o abiertos 'number'"
  [left right]
  (cond (or (every? coll? [left right])
            (every? number? [left right])
            (some #{:-inf :+inf} [left right])) (= left right)
        (number? left) (< left (first right))
        :else (< (first left) right)))

(defn- limite<
  "Indica si un límite de un intervalo es anterior a otro.
   Pueden ser cerrados '[number]' o abiertos 'number'"
  [left right]
  (cond (= :-inf left) true
        (= :+inf left) false
        (= :-inf right) false
        (= :+inf right) true
        (every? coll? [left right]) (< (first left) (first right))
        (every? number? [left right]) (< left right)
        (number? left) (< left (first right))
        :else (<= (first left) right)))

(defn- limite<=
  "Indica si un límite de un intervalo es anterior o igual a otro.
   Pueden ser cerrados '[number]' o abiertos 'number'"
  [left right]
  (or (limite= left right)
      (limite< left right)))

(defn- test-numerico>=
  "Indica si un test numérico es más general o de la misma categoría
   que otro"
  [[v11 v12 :as test1] [v21 v22 :as test2]]
  (cond (nil? v12) (test-numerico>= [v11 v11] [v21 v22])
        (nil? v22) (test-numerico>= [v11 v12] [v21 v21])
        :else (or (and (limite<= v11 v21)
                       (limite<= v22 v12))
                  (and (limite<= v21 v11)
                       (not (limite<= v12 v22))))))

(assert (and
         [(test-numerico>= [1 2] [3 4])
          (test-numerico>= [1 4] [2 3])
          (test-numerico>= [1 3] [2 4])
          (test-numerico>= [1 4] [2 3])
          (not (test-numerico>= [2 3] [1 4]))
          (test-numerico>= [1] [1])
          (test-numerico>= [1] [1 1])
          (test-numerico>= [1 1] [1])
          (test-numerico>= [1 1] [1 1])
          (test-numerico>= [[1] 1] [1 1])
          (test-numerico>= [1 [1]] [1 1])
          (test-numerico>= [:-inf 1] [1 1])
          (not (test-numerico>= [:+inf 1] [1 1]))
          (test-numerico>= [1 :+inf] [1 1])
          (not (test-numerico>= [1 :-inf] [1 1]))
          (not (test-numerico>= [1 1] [:-inf 1]))
          (test-numerico>= [1 1] [:+inf 1])
          (not (test-numerico>= [1 1] [1 :+inf]))
          (test-numerico>= [1 1] [1 :-inf])]

             ))

(defn- test-nominal>=
  "Indica si un test nominal es más general o de la misma categoría
   que otro"
  [test1 test2]
  (or (= (set test1) (set test2))
      (not (every? (set test2) test1))))

(assert (test-nominal>= [:a :b] [:a]))
(assert (not (test-nominal>= [:a] [:a :b])))

;; Ejercicio 6
(defn test-CL>=
  "Indica si un test es más general o de la misma categoría que otro"
  [t1 t2]
  (cond (some test-ambivalente? [t1 t2]) (test-ambivalente>= t1 t2)
        (every? test-numerico? [t1 t2]) (test-numerico>= t1 t2)
        (every? test-nominal? [t1 t2]) (test-nominal>= t1 t2)
        :else "Comparación no soportada"))

(assert (test-CL>= [*] [*]))
(assert (test-CL>= [:lluvioso] [:soleado]))
(assert (test-CL>= [:lluvioso] []))
(assert (not (test-CL>=  []   [:lluvioso])))
(assert (test-CL>= [25 30] [26]))

(assert (test-CL>=  [25 30] [21 25]))
(assert (not (test-CL>= [26] [25 30])))
