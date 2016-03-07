(ns amanas17.maia.p2-1
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]))

;; Ejercicios 1 a 5 de la práctica 2

;; Por simplicidad defino los valores -infinito y +infinito como keyworkds
;; :-inf
;; :+inf

(defn- test-ambivalente?
  "Determina si un test puede ser aplicado indistintamente
  a un atributo numérico o nominal"
  [test]
  (not (nil? (some #{[] [*]} [test]))))

(assert (every? test-ambivalente? [[] [*]]))

(defn- test-numerico?
  "Determina si un test es aplicable a un atributo numérico"
  [[v1 v2 :as test]]
  (or (test-ambivalente? test)
      (and ((some-fn :-inf :+inf number?) (if (coll? v1) (first v1) v1))
           ((some-fn :-inf :+inf number? nil?) (if (coll? v2) (first v2) v2)))))

(assert (and (test-numerico? [])
             (test-numerico? [*])
             (test-numerico? [1])
             (test-numerico? [1 2])
             (test-numerico? [[1] 2])
             (test-numerico? [[1] [2]])
             (test-numerico? [1 [2]])))

(defn- test-nominal?
  "Determina si un test es aplicable a un atributo nominal"
  [test]
  (or (test-ambivalente? test)
      (and (not (some #{:-inf :+inf} test))
           (every? keyword? test))))

(assert (and (test-nominal? [])
             (test-nominal? [*])
             (test-nominal? [:a])
             (test-nominal? [:a :b])))

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

(assert (and (match-nature? [] 1)
             (match-nature? [*] 1)
             (match-nature? [1] 1)
             (match-nature? [1 2] 1)
             (match-nature? [[1]] 1)
             (match-nature? [[1] 2] 1)
             (match-nature? [1 [2]] 1)
             (match-nature? [[1] [2]] 1)
             (match-nature? [:a] :b)
             (match-nature? [:a :b] :c)
             (match-nature? [] :a)
             (match-nature? [*] :a)))

(defn- match-ambivalente?
  "Determina si un atributo satisface un test ambivalente"
  [test atributo]
  (= [*] test))

(defn- match-nominal?
  "Determina si un atributo nominal satisface un test nominal"
  [test atributo]
  (not (nil? (some (set test) [atributo]))))

(assert (and (match-nominal? [:a] :a)
             (match-nominal? [:a :b] :a)
             (not (match-nominal? [:a :b] :c))))

; defino la aritmética de límites de intervalos
(defn- lv= [l v] (= l v))
(defn- lv< [l v] (cond (every? coll? [l v]) (lv< (first l) (first v))
                       (coll? l) (lv< (first l) v)
                       (coll? v) (lv< l (first v))
                       (= :-inf l) true
                       (= :+inf v) true
                       (= :-inf v) false
                       (= :+inf l) false
                       :else (< l v)))
(defn- lv<= [l v] (or (lv= l v) (lv< l v)))
(defn- lv> [l v] (not (lv<= l v)))
(defn- lv>= [l v] (or (lv= l v) (lv> l v)))

(defn- normalize-numerico
  "Los tests numéricos admiten demasiada variedad en su forma de
  ser expresados.
  Ésta función los encapsula en una estructura común consistente en
  que puede adoptar cualquiera de estas formas:
  - [a b]
  - [[a] b]
  - [a [b]]
  - [[a] [b]]
  siendo a y b números o :-inf, :+inf"
  [[l1 l2 :as t]]
  (cond (= t [])         [[:+inf] [:-inf]]
        (= t [*])        [[:-inf] [:+inf]]
        (nil? l2)        (normalize-numerico [l1 l1])
        (every? coll? t) (if (lv> (first l1) (first l2)) (normalize-numerico []) t)
        (coll? l1)       (if (lv>= (first l1) l2) (normalize-numerico []) t)
        (coll? l2)       (if (lv>= l1 (first l2)) (normalize-numerico []) t)
        (lv> l1 l2)      (normalize-numerico [])
        (lv= l1 l2)      [[l1] [l2]]
        :else            t))

(assert (and (= [[:+inf] [:-inf]] (normalize-numerico []))
             (= [[:-inf] [:+inf]] (normalize-numerico [*]))
             (= [:-inf 1] (normalize-numerico [:-inf 1]))
             (= [[:+inf] [:-inf]] (normalize-numerico [1 :-inf]))
             (= [1 :+inf] (normalize-numerico [1 :+inf]))
             (= [[:+inf] [:-inf]] (normalize-numerico [:+inf 1]))
             (= [[:-inf] 1] (normalize-numerico [[:-inf] 1]))
             (= [[1] [1]] (normalize-numerico [1]))
             (= [[1] [1]] (normalize-numerico [[1]]))
             (= [[1] [1]] (normalize-numerico [1 1]))
             (= [[:+inf] [:-inf]] (normalize-numerico [1 [1]]))
             (= [[:+inf] [:-inf]] (normalize-numerico [[1] 1]))
             (= [1 2] (normalize-numerico [1 2]))
             (= [1 [2]] (normalize-numerico [1 [2]]))
             (= [[1] 2] (normalize-numerico [[1] 2]))
             (= [[1] [2]] (normalize-numerico [[1] [2]]))
             (= [[:+inf] [:-inf]] (normalize-numerico [2 1]))))

(defn- match-numerico?
  "Determina si un atributo numérico satisface un test numérico"
  [test v]
  (let [[l1 l2] (normalize-numerico test)]
    (and (if (coll? l1) (lv<= (first l1) v) (lv< l1 v))
         (if (coll? l2) (lv<= v (first l2)) (lv< v l2)))))

(assert (and (match-numerico? [1] 1)
             (match-numerico? [1 2] 1.5)
             (match-numerico? [[1] 2] 1)
             (match-numerico? [1 [2]] 2)
             (match-numerico? [[1] [2]] 1)
             (match-numerico? [1 1] 1)))

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

(assert (and (test-ambivalente>= [*] [*])
             (test-ambivalente>= [*] [])
             (test-ambivalente>= [] [])
             (not (test-ambivalente>= [] [*]))))

(defn- test-numerico>= [t1 t2]
  "Indica si un test numérico es más general o de la misma categoría
  que otro"
  (let [[l1 r1] (normalize-numerico t1)
        [l2 r2] (normalize-numerico t2)
        t1-in-t2 (and (lv<= l2 l1) (lv<= r1 r2))
        t2-in-t1 (and (lv<= l1 l2) (lv<= r2 r1))]
    (or t2-in-t1 (not t1-in-t2))))

(assert (and (test-numerico>= [1 2] [3 4])
             (test-numerico>= [1 4] [2 3])
             (test-numerico>= [1 3] [2 4])
             (not (test-numerico>= [2 3] [1 4]))
             (test-numerico>= [1 2] [1 2])
             (test-numerico>= [1] [1])
             (test-numerico>= [1] [1 1])
             (test-numerico>= [1 1] [1])
             (test-numerico>= [1 1] [1 1])
             (not(test-numerico>= [[1] 1] [1 1]))
             (not (test-numerico>= [1 [1]] [1 1]))
             (test-numerico>= [1 1] [[1] 1])
             (test-numerico>= [1 1] [1 [1]])
             (test-numerico>= [1 2] [[1] 2])
             (test-numerico>= [1 2] [1 [2]])
             (test-numerico>= [:-inf 1] [1 1])
             (not (test-numerico>= [:+inf 1] [1 1]))
             (test-numerico>= [1 :+inf] [1 1])
             (not (test-numerico>= [1 :-inf] [1 1]))))

;; (defn- test-numerico= [t1 t2] (every? test-numerico>= [[t1 t2] [t2 t1]]))
;; (defn- test-numerico<= [t1 t2] (or (test-numerico= t1 t2) (test-numerico>= t2 t1)))

(defn- test-nominal>=
  "Indica si un test nominal es más general o de la misma categoría
  que otro"
  [t1 t2]
  (or (every? (set t1) t2)
      (not (every? (set t2) t1))))

(assert (and (test-nominal>= [:a :b] [:a])
             (test-nominal>= [:a :b] [:a :c])
             (not (test-nominal>= [:a] [:a :b]))
             (not (test-nominal>= [] [:a]))
             (test-nominal>= [:a] [])))

 ;; Ejercicio 6
(defn test-CL>=
  "Indica si un test es más general o de la misma categoría que otro"
  [t1 t2]
  (cond (some test-ambivalente? [t1 t2]) (test-ambivalente>= t1 t2)
        (every? test-numerico? [t1 t2]) (test-numerico>= t1 t2)
        (every? test-nominal? [t1 t2]) (test-nominal>= t1 t2)
        :else "Comparación no soportada"))

(assert (and (test-CL>= [*] [*])
             (test-CL>= [:lluvioso] [:soleado])
             (test-CL>= [:lluvioso] [])
             (not (test-CL>=  []   [:lluvioso]))
             (test-CL>= [25 30] [26])
             (test-CL>=  [25 30] [21 25])
             (not (test-CL>= [26] [25 30]))))
