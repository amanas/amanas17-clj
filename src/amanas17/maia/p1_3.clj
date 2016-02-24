(ns amanas17.maia.p1-3
  (:use [amanas17.maia.p1-1]))

;; ejercicio 8
(defn leer-ejemplos [f]
  (read-string (slurp f)))

(def ejemplos (leer-ejemplos "resources/maia/ejemplos.scm"))

(assert (= 21 (count ejemplos)))
(he-tardado 60 8)

;; ejercicio 9
(defn anadir-ejemplo [xs x]
  (conj xs x))

(assert (let [x [:diluvia 29 80 :fuerte :contento :normal :ajustado :- ]
              result (anadir-ejemplo ejemplos x)]
          (and (= 22 (count result))
               (= x (last result)))))
(he-tardado 30 9)

;; ejercicio 10
(defn atributo [a xs]
  (let [i  (->> xs first (map first)  (#(.indexOf % a)))]
    (->> xs rest (map (fn [v] (nth v i))))))

(assert (= [:ajustado :solvente] (take 2 (atributo :dinero ejemplos))))
(he-tardado 30 10)

;; Por qué debemos escribir 'perspectiva y no simplemente perspectiva?
;; Porque referenciamos un símbolo, no una variable definida previamente.
;; En clojure utilizamos 'perspectiva o :perspectiva

;; ejercicio 11
(defn mezclar
  "Concatena dos listas
   Asume que las dos listas tienen descripción de atributos
   La lista devuelta también tiene cabecera con descripción de atributos"
  [xs1 xs2] (into xs1 (rest xs2)))

(assert (let [atts [[:perspectiva [:nublado :soleado :lluvioso :niebla :diluvia]]
                    [:temperatura :numerico]
                    [:humedad :numerico]
                    [:viento [:fuerte :brisa :no]]
                    [:animo [:contento :normal :triste]]
                    [:estres [:relajado :normal :estresado]]
                    [:dinero [:solvente :ajustado :insuficiente]]
                    [:clase [:+ :-]]]
              xs2 [atts
                   [:diluvia 29 80 :fuerte :contento :normal :ajustado :- ]]
              m (mezclar ejemplos xs2)]
          (and (= ejemplos (butlast m))
               (= (second xs2) (last m)))))
(he-tardado 60 11)
