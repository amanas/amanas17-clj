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
  (or (match-ambivalente? test atributo)
      (not (nil? (some (set test) [atributo])))))

(assert (and (match-nominal? [:a] :a)
             (match-nominal? [:a :b] :a)
             (not (match-nominal? [:a :b] :c))))

;; Defino funciones que nos permiten comparar límites de intervalos
;; normalizados con valores
(defn- lv<= [l v]
  (cond (= :-inf l)            true
        (= :+inf v)            true
        (every? coll? [l v])  (lv<=  l (first v))
        (coll? l)             (lv<= (first l) v)
        (every? number? [l v])(<= l v)
        :else                 false))
(defn- lv=  [l v] (and (lv<= l v) (lv<= v l)))
(defn- lv<  [l v] (and (lv<= l v) (not (lv= l v))))
(defn- lv>  [l v] (not (lv<= l v)))
(defn- lv>= [l v] (not (lv<  l v)))

(defn- weird?
  "Indica si el test es 'extraño', es decir, con extremos validos pero
  sin valores posibles en su interior. Es decir, casos de este tipo:
  - [[a] a]
  - [a [a]]
  siendo a un valor numérico, :-inf o :+inf"
  [l1 l2]
  (or (and (coll? l1) (not (coll? l2)) (lv= (first l1) l2))
      (and (not (coll? l1)) (coll? l2) (lv= l1 (first l2))))  )

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
  [[l1 l2 :as t]]
  (cond (= t [])            []
        (= t [*])           [*]
        (= t [:-inf :+inf]) [*]
        (= [:-inf] l1)      (normalize-numerico [:-inf l2])
        (= [:+inf] l2)      (normalize-numerico [l1 :+inf])
        (nil? l2)           (normalize-numerico [l1 l1])
        (weird? l1 l2)      []
        (lv= l1 l2)         (if (every? coll? [l1 l2]) [(first l1) (first l2)] t)
        (coll? l2)          (if (lv< l1 (first l2)) t [])
        (lv> l1 l2)         []
         :else               t))

(assert (and
         (= [] (normalize-numerico []))
         (= [*] (normalize-numerico [*]))
         (= [:-inf 1] (normalize-numerico [:-inf 1]))
         (= [] (normalize-numerico [1 :-inf]))
         (= [1 :+inf] (normalize-numerico [1 :+inf]))
         (= [] (normalize-numerico [:+inf 1]))
         (= [:-inf 1] (normalize-numerico [[:-inf] 1]))
         (= [1 1] (normalize-numerico [1]))
         (= [1 1] (normalize-numerico [[1]]))
         (= [1 1] (normalize-numerico [1 1]))
         (= [] (normalize-numerico [1 [1]]))
         (= [] (normalize-numerico [[1] 1]))
         (= [1 2] (normalize-numerico [1 2]))
         (= [1 [2]] (normalize-numerico [1 [2]]))
         (= [[1] 2] (normalize-numerico [[1] 2]))
         (= [[1] [2]] (normalize-numerico [[1] [2]])))
        (= [] (normalize-numerico [2 1])))

(defn- match-numerico?
  "Determina si un atributo numérico satisface un test numérico"
  [test v]
  (let [[l1 l2] (normalize-numerico test)]
    (or (match-ambivalente? test v)
        (cond (= l1 l2) (= l1 v)
              :else     (and (if (coll? l1) (lv<= (first l1) v) (lv< l1 v))
                             (if (coll? l2) (lv<= v (first l2)) (lv< v l2)))))))

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
  (let [[l1 r1 :as t1] (normalize-numerico t1)
        [l2 r2 :as t2] (normalize-numerico t2)
        t1-in-t2 (or (= t1 [])
                     (= t2 [*])
                     (and (lv<= l2 l1) (lv<= r1 r2)))
        t2-in-t1 (or (= t2 [])
                     (= t1 [*])
                     (and (lv<= l1 l2) (lv<= r2 r1)))]
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
             (not (test-numerico>= [[1] 1] [1 1]))
             (not (test-numerico>= [1 [1]] [1 1]))
             (test-numerico>= [1 1] [[1] 1])
             (test-numerico>= [1 1] [1 [1]])
             (not (test-numerico>= [1 2] [[1] 2]))
             (test-numerico>= [1 2] [1 [2]])
             (test-numerico>= [:-inf 1] [1 1])
             (not (test-numerico>= [:+inf 1] [1 1]))
             (test-numerico>= [1 :+inf] [1 1])
             (not (test-numerico>= [1 :-inf] [1 1]))))

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
           (generalizaciones-atributo-nominal [[:soleado][*][20][]] 3 (first ejemplos))))
(assert (= [[[:soleado][*][20][:si :fuerte]]
            [[:soleado][*][20] [:si :brisa]]
            [[:soleado][*][20] [:si :no]]]
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
        [a b] (normalize-numerico test)
        atributo (nth ejemplo indice-atributo)
        gener (cond (= :- (last ejemplo))  concepto-CL
                    (match-numerico? test atributo) concepto-CL
                    (lv< atributo a) (normalize-numerico [[atributo] b])
                    (lv< b atributo) (normalize-numerico [a [atributo]])
                    :else test)]
     (concat (take indice-atributo concepto-CL)
             [gener]
             (drop (inc indice-atributo) concepto-CL))))

(generalizacion-atributo-numerico [[:soleado][][20][:si]] 1 [:soleado 25 40 :si +])
