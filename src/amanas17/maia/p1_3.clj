(ns amanas17.maia.p1-3
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]))

;; Ejercicio 1.8
(defn leer-ejemplos [f]
  (read-string (slurp f)))

(def ejemplos (leer-ejemplos "resources/maia/ejemplos.scm"))

(he-tardado 60 1.8)

;; Ejercicio 1.9
(defn anadir-ejemplo [xs x]
  (conj xs x))

(he-tardado 30 1.9)

;; Ejercicio 1.10
(defn atributo [a xs]
  (let [i  (->> xs first (map first)  (#(.indexOf % a)))]
    (->> xs rest (map (fn [v] (nth v i))))))

(he-tardado 30 1.10)

;; Por qué debemos escribir 'perspectiva y no simplemente perspectiva?
;; Porque referenciamos un símbolo, no una variable definida previamente.
;; En clojure utilizamos 'perspectiva o perspectiva

;; Ejercicio 1.11
(defn mezclar
  "Concatena dos listas
   Asume que las dos listas tienen descripción de atributos
   La lista devuelta también tiene cabecera con descripción de atributos"
  [xs1 xs2] (into xs1 (rest xs2)))

(he-tardado 60 1.11)
