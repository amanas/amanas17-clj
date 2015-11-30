(ns amanas17.maia.p1-2)

(defn he-tardado [m e]
  (prn (str "He tardado " m " minutos en el ejercicio " e)))

; ejercicio 8
(defn leer-ejemplos [f]
  (read-string (slurp f)))

(assert (= :perspectiva (first (ffirst (leer-ejemplos "resources/maia/ejemplos.scm")))))
(he-tardado 60 7)
