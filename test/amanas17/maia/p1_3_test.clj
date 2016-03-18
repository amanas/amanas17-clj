(ns amanas17.maia.p1-3-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]))


;; Ejercicio 1.8 - tests
(assert (= 21 (count ejemplos)))

;; Ejercicio 1.9 - tests
(assert (let [x [diluvia 29 80 :fuerte contento normal ajustado -- ]
              result (anadir-ejemplo ejemplos x)]
          (and (= 22 (count result))
               (= x (last result)))))

;; Ejercicio 1.10 - tests
(assert (= [ajustado solvente] (take 2 (atributo dinero ejemplos))))

;; Ejercicio 1.11 - tests
(assert (let [atts [[perspectiva [nublado soleado lluvioso niebla diluvia]]
                    [temperatura numerico]
                    [humedad numerico]
                    [viento [:fuerte :brisa no]]
                    [animo [contento normal triste]]
                    [estres [relajado normal estresado]]
                    [dinero [solvente ajustado insuficiente]]
                    [clase [++ --]]]
              xs2 [atts
                   [diluvia 29 80 :fuerte contento normal ajustado -- ]]
              m (mezclar ejemplos xs2)]
          (and (= ejemplos (butlast m))
               (= (second xs2) (last m)))))
