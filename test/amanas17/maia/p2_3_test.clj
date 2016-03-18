(ns amanas17.maia.p2-3-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]))

(def metadatos (first ejemplos))

;; Ejercicio 2.9 - tests
;; creo que el ejemplo del material de estudio está mal
(assert (= [[[niebla  nublado lluvioso diluvia]  [**] [20] [si]]
            [[soleado nublado lluvioso diluvia]  [**] [20] [si]]
            [[soleado niebla  lluvioso diluvia]  [**] [20] [si]]
            [[soleado niebla  nublado  diluvia]  [**] [20] [si]]
            [[soleado niebla  nublado  lluvioso] [**] [20] [si]]]
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
                                             [soleado 25 40 si ++])))
(assert (= [[soleado][][20][si]]
           (generalizacion-atributo-numerico [[soleado][][20][si]] 1
                                             [soleado 25 40 si --])))
(assert (= [[soleado][15 30][20][si]]
           (generalizacion-atributo-numerico [[soleado][15 30][20][si]] 1
                                             [soleado 25 40 si ++])))

;; Ejercicio 2.12 - tests
(assert (= [[[**][-inf 25][20][si]]
            [[**][25 +inf][20][si]]]
           (especializaciones-atributo-numerico [[**][**][20][si]] 1
                                                [soleado 25 40 si --])))
(assert (= [[[**][**][20][si]]]
           (especializaciones-atributo-numerico [[**][**][20][si]] 1
                                                [soleado 25 40 si ++])))

(assert (= [[[**][10 15][20][si]]]
           (especializaciones-atributo-numerico [[**][10 15][20][si]] 1
                                                [soleado 25 40 si --])))

;; Ejercicio 2.13 - tests

(assert
 (= [[[**][**][**][**] [**] [**] [**]]]
    (generalizaciones-CL [[soleado niebla nublado lluvioso][**][**][**] [**] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado ++])))
(assert
 (= [[[soleado niebla nublado lluvioso][**][**][**] [**] [**] [**]]]
    (generalizaciones-CL [[soleado niebla nublado lluvioso][**][**][**] [**] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado --])))
(assert
 (= [[[**] [[19] [20]] [[15] [90]] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][19][15][**] [**] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado ++])))
(assert
 (= [[[**] [19] [15] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][19][15][**] [**] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado --])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][si] [**] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado ++])))
(assert
 (= [[[**] [**] [**] [si] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][si] [**] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado --])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][**] [triste normal] [**] [**]]
                         metadatos
                         [diluvia 20 90 si contento relajado ajustado ++])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][**] [**] [relajado normal ] [**]]
                         metadatos
                         [diluvia 20 90 si contento estresado ajustado ++])))
(assert
 (= [[[**] [**] [**] [**] [**] [relajado normal] [**]]]
    (generalizaciones-CL [[**][**][**][**] [**] [relajado normal ] [**]]
                         metadatos
                         [diluvia 20 90 si contento estresado ajustado --])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [**]]]
    (generalizaciones-CL [[**][**][**][**] [**] [**] [ajustado insuficiente]]
                         metadatos
                         [diluvia 20 90 si contento estresado solvente ++])))
(assert
 (= [[[**] [**] [**] [**] [**] [**] [ajustado insuficiente]]]
    (generalizaciones-CL [[**][**][**][**] [**] [**] [ajustado insuficiente]]
                         metadatos
                         [diluvia 20 90 si contento estresado solvente --])))
