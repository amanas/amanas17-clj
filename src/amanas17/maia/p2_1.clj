(ns amanas17.maia.p2-1)

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
      (let [[a b] test
            left (cond (nil? b)    (partial = (first (flatten [a])))
                       (coll? a)   (partial <= (first a))
                       (= :-inf a) (constantly true)
                       (= :+inf a) (constantly false)
                       :else       (partial < a))
            right (cond (nil? b)    (constantly true)
                        (coll? b)   (partial >= (first b))
                        (= :-inf b) (constantly false)
                        (= :+inf b) (constantly true)
                        :else       (partial > b))]
        (and (left atributo)
             (right atributo)))))

(assert (not (match-numerico? [] 1)))
(assert (match-numerico? [*] 1))
(assert (match-numerico? [1] 1))
(assert (match-numerico? [1 2] 1.5))
(assert (match-numerico? [[1]] 1))
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
