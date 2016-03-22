(ns amanas17.maia.p1-3-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]))


;; Ejercicio 1.8 - tests
(assert (= 21 (count ejemplos)))

;; Ejercicio 1.9 - tests
(assert (let [x [lluvioso 29 80 si contento relajado solvente - ]
              result (anadir-ejemplo ejemplos x)]
          (and (= 22 (count result))
               (= x (last result)))))

;; Ejercicio 1.10 - tests
(assert (= [insuficiente solvente] (take 2 (atributo dinero ejemplos))))

;; Ejercicio 1.11 - tests
(assert (let [atts [[perspectiva [nublado soleado lluvioso]]
                    [temperatura numerico]
                    [humedad numerico]
                    [viento [si no]]
                    [animo [contento triste]]
                    [estres [relajado estresado]]
                    [dinero [solvente insuficiente]]
                    [clase [+ -]]]
              xs2 [atts
                   [lluvioso 29 80 si contento relajado insuficiente - ]]
              m (mezclar ejemplos xs2)]
          (and (= ejemplos (butlast m))
               (= (second xs2) (last m)))))
