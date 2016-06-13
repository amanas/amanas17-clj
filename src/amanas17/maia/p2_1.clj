(ns amanas17.maia.p2-1
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representación CL (introducción) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Por simplicidad defino los valores -infinito y +infinito por los simbolos -inf y +inf
;; porque en clojure no se pueden utilizar los indicados en el material de estudio

(defn test-ambivalente?
  "Determina si un test puede ser aplicado indistintamente
  a un atributo numérico o nominal"
  [test]
  (or (= '() test)
      (= '(*) test)))

(defn test-nominal?
  "Determina si un test es aplicable a un atributo nominal"
  [[v & more :as test]]
  (or (test-ambivalente? test)
      (and (symbol? v)
           (nil? more)
           (not= '-inf v)
           (not= '+inf v))))

(defn test-numerico?
  "Determina si un test es aplicable a un atributo numérico"
  [test]
  (or (test-ambivalente? test)
      (not (test-nominal? test))))


(defn match-nature?
  "Determina si un test (o array de ellos) y un atributo (o array de ellos)
  tienen la misma naturaleza (nominal o numérica)"
  ([[tests atributos]]
   (->> (interleave tests atributos)
        (partition 2)
        (every? (partial apply match-nature?))))
  ([test atributo]
   (or (test-ambivalente? test)
       (and (test-nominal? test) (symbol? atributo))
       (and (test-numerico? test) (number? atributo)))))

(defn match-ambivalente?
  "Determina si un atributo satisface un test ambivalente"
  [test atributo]
  (= '(*) test))

(defn match-nominal?
  "Determina si un atributo nominal satisface un test nominal"
  [test atributo]
  (or (match-ambivalente? test atributo)
      (= (first test) atributo)))

(defn comp<=
  "Función auxiliar que permite hacer comparaciones habida cuenta de que ahora
  -inf y +inf tienen que poderse comparar con números.
  Sobre esta función recae en última instancia el orden parcial de los tests
  numéricos"
  [a b]
  (cond (= '-inf a) true
        (= '+inf b) true
        (every? number? [a b]) (<= a b)
        :else false))

(defn comp< [a b]
  (and (comp<= a b) (not= a b)))

(defn normalize-numerico
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
  siendo a y b números o -inf, +inf"
  [[a b :as t]]
  (cond (= '() t) t
        (= '(*) t) t
        (= '(-inf) a) (normalize-numerico ['-inf b])
        (= '(+inf) b) (normalize-numerico [a '+inf])
        (= '(-inf +inf) t) '(*)
        (nil? b) (normalize-numerico [a a])
        (every? coll? t) (if (comp<= (first a) (first b)) t '())
        (every? (comp not coll?) t) (cond (= a b) [[a] [b]]
                                          (comp< a b) t
                                          :else '())
        (coll? a) (if (comp< (first a) b) t '())
        (coll? b) (if (comp< a (first b)) t '())
        (comp<= a b) t
        :else '()))

(defn match-numerico?
  "Determina si un atributo numérico satisface un test numérico"
  [test v]
  (let [[a b :as t] (normalize-numerico test)]
    (or (match-ambivalente? t v)
        (= a b v)
        (and (if (coll? a) (comp<= (first a) v) (comp< a v))
             (if (coll? b) (comp<= v (first b)) (comp< v b))))))

(defn match?
  "Determina si un test (o array de ellos) y un atributo (o array de ellos)
  son satisfechos"
  ([[tests atributos]]
   (->> (interleave tests atributos)
        (partition 2)
        (every? (partial apply match?))))
  ([test atributo]
   (cond (test-ambivalente? test) (match-ambivalente? test atributo)
         (test-numerico? test) (match-numerico? test atributo)
         (test-nominal? test) (match-nominal? test atributo)
         :else false)))

; (match-CL [[soleado] [**] [10 [40]] [si]] [soleado 30 40 si])
;; Ejercicio 2.1
(defn match-CL
  "Determina si un conjunto de atributos satisfacen un conjunto de tests"
  [[& tests :as concepto]
   [& atributos :as ejemplo-sin-clase]]
  (and (= (count tests) (count atributos))
       (match-nature? [tests atributos])
       (match? [tests atributos])))

(he-tardado 360 2.1)


;; Ejercicio 2.2
(defn CLi
  "Intérprete basado en conjunciones lógicas"
  [concepto ejemplo-sin-clase]
  (concat ejemplo-sin-clase [(if (match-CL concepto ejemplo-sin-clase) '+ '-)]))

(he-tardado 45 2.2)


;; Ejercicio 2.3
;; Mi entendimiento de la lectura del enunciado es que este ejercicio
;; no implica programar sino simplemente enunciar los coceptos más
;; incluyentes y excluyentes.

;; Como mis ejemplos tienen 7 atributos más la clase, debería ser
(def concepto-mas-general-posible '((*) (*) (*) (*) (*) (*) (*)))
(def concepto-mas-especifico-posible '(() () () () () () ()))
;; Un concepto que para mí podría suponer un buen día para salir al campo es
(def concepto-mas-cercano-para-mi '((soleado) (20 30) (50 55) (no) (contento) (*) (solvente)))

(he-tardado 20 2.3)

;; Ejercicio 2.4
(defn concepto-CL-mas-general
  "Devuelve el concepto CL más general posible dada la descripción de
  atributos de un conjunto de datos.
  Metadatos se entiende que es la cabecera de descripción de atributos"
  [metadatos]
  (vec (replicate (dec (count metadatos)) '(*))))

(he-tardado 20 2.4)

;; Ejercicio 2.5
(defn concepto-CL-mas-especifico
  "Devuelve el concepto CL más específico posible dada la descripción de
  atributos de un conjunto de datos.
  Metadatos se entiende que es la cabecera de descripción de atributos"
  [metadatos]
  (repeat (dec (count metadatos)) '()))

(he-tardado 3 2.5)
