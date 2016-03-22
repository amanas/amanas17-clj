(ns amanas17.maia.p2-3-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]))

(def metadatos (first ejemplos))

;; Ejercicio 2.9 - tests
;; creo que el ejemplo del material de estudio está mal
(assert (= [[[nublado lluvioso]  [**] [20] [si]]
            [[soleado lluvioso]  [**] [20] [si]]
            [[soleado nublado]  [**] [20] [si]]]
           (especializaciones-atributo-nominal [[**][**][20][si]] 0
                                               metadatos)))
(assert (= [[[][**][20][si]]]
           (especializaciones-atributo-nominal [[soleado][**][20][si]] 0
                                               metadatos)))
(assert (= [[[][**][20][si]]]
           (especializaciones-atributo-nominal [[][**][20][si]] 0
                                               metadatos)))

;; Ejercicio 2.10 - tests
(assert (= [[[soleado][**][20][no]]
            [[soleado][**][20][si]]]
           (generalizaciones-atributo-nominal [[soleado][**][20][]] 3
                                              metadatos)))
(assert (= [[[soleado][**][20][**]]]
           (generalizaciones-atributo-nominal [[soleado][**][20][si]] 3
                                              metadatos)))
(assert (= [[[soleado][**][20][**]]]
           (generalizaciones-atributo-nominal [[soleado][**][20][**]] 3
                                              metadatos)))

;; ejercicio 2.11 - tests
(assert (= [[soleado][25][20][si]]
           (generalizacion-atributo-numerico [[soleado][][20][si]] 1
                                             [soleado 25 40 si +])))
(assert (= [[soleado][][20][si]]
           (generalizacion-atributo-numerico [[soleado][][20][si]] 1
                                             [soleado 25 40 si -])))
(assert (= [[soleado][15 30][20][si]]
           (generalizacion-atributo-numerico [[soleado][15 30][20][si]] 1
                                             [soleado 25 40 si +])))

;; Ejercicio 2.12 - tests
(assert (= [[[**][-inf 25][20][si]]
            [[**][25 +inf][20][si]]]
           (especializaciones-atributo-numerico [[**][**][20][si]] 1
                                                [soleado 25 40 si -])))
(assert (= [[[**][**][20][si]]]
           (especializaciones-atributo-numerico [[**][**][20][si]] 1
                                                [soleado 25 40 si +])))

(assert (= [[[**][10 15][20][si]]]
           (especializaciones-atributo-numerico [[**][10 15][20][si]] 1
                                                [soleado 25 40 si -])))


;; Ejercicio 2.13 - tests

(assert
 (= [[[**][**][**][**][**][**][**]]]
    (generalizaciones-CL [[soleado nublado][**][**][**] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente +])))
(assert
 (= [[[soleado lluvioso][**][**][**] [**] [**] [**]]]
    (generalizaciones-CL [[soleado lluvioso][**][**][**] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente -])))
(assert
 (= [[[**] [[19] [20]] [15] [**] [**] [**] [**]]
     [[**] [19] [[15] [90]] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][19][15][**] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente +])))
(assert
 (= [[[**] [19] [15] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][19][15][**] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente -])))
(assert
 (= [[[**][**][**][**][**][**][**]]]
    (generalizaciones-CL [[**][**][**][si] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 no contento relajado insuficiente +])))
(assert
 (= [[[**][**][**][si][**][**][**]]]
    (generalizaciones-CL [[**][**][**][si] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente +])))
(assert
 (= [[[**] [**] [**] [si] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][si] [**] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente -])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][**] [triste] [**] [**]]
                         metadatos
                         [lluvioso 20 90 si contento relajado insuficiente +])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][**] [**] [relajado] [**]]
                         metadatos
                         [lluvioso 20 90 si contento estresado insuficiente +])))
(assert
 (= [[[**] [**] [**] [**] [**] [relajado] [**]]]
    (generalizaciones-CL [[**][**][**][**] [**] [relajado] [**]]
                         metadatos
                         [lluvioso 20 90 si contento estresado insuficiente -])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][**] [**] [**] [insuficiente]]
                         metadatos
                         [lluvioso 20 90 si contento estresado solvente +])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [insuficiente]]]
    (generalizaciones-CL [[**][**][**][**] [**] [**] [insuficiente]]
                         metadatos
                         [lluvioso 20 90 si contento estresado solvente -])))


;; Ejercicio 2.14 - tests
(assert (=  [[[nublado] [10 30] [80 100] [si] [contento] [relajado] [insuficiente]]
             [[soleado] [10 30] [80 100] [si] [contento] [relajado] [insuficiente]]
             [[soleado nublado] [10 20] [80 100] [si] [contento] [relajado] [insuficiente]]
             [[soleado nublado] [20 30] [80 100] [si] [contento] [relajado] [insuficiente]]
             [[soleado nublado] [10 30] [80 90] [si] [contento] [relajado] [insuficiente]]
             [[soleado nublado] [10 30] [90 100] [si] [contento] [relajado] [insuficiente]]
             [[soleado nublado] [10 30] [80 100] [] [contento] [relajado] [insuficiente]]
             [[soleado nublado] [10 30] [80 100] [si] [] [relajado] [insuficiente]]
             [[soleado nublado] [10 30] [80 100] [si] [contento] [] [insuficiente]]
             [[soleado nublado] [10 30] [80 100] [si] [contento] [relajado] []]]
           (especializaciones-CL [[soleado nublado][10 30][80 100][si][contento][relajado][insuficiente]]
                                 metadatos
                                 [soleado 20 90 si contento relajado insuficiente -])))

(assert (= [[[soleado nublado][20][90][si][contento][relajado][insuficiente]]]
           (especializaciones-CL [[soleado nublado][20][90][si][contento][relajado][insuficiente]]
                                 metadatos
                                 [soleado 20 90 si contento relajado insuficiente +])))
