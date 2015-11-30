(ns amanas17.maia.p1-2)

(defn he-tardado [m e]
  (prn (str "He tardado " m " minutos en el ejercicio " e)))

; ejercicio 8
(defn leer-ejemplos [f]
  (read-string (slurp f)))

(def ejemplos (leer-ejemplos "resources/maia/ejemplos.scm"))

(assert (= 21 (count ejemplos)))
(he-tardado 60 7)
