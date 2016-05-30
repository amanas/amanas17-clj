(ns amanas17.maia.p2-7-test
  (:use
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-7 :refer :all]))

(assert (= 2
           (traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)))

(assert (true? (apply = (map count (nuevo-conceptoUU (first ejemplos) 1)))))

(assert (match-LUU [(first ejemplos) '(0 1 1 0 30)] '(lluvioso 10 20 si)))

(assert (not (match-LUU [(first ejemplos) '(0 1 1 0 31)] '(lluvioso 10 20 si))))

(assert (= '(soleado 30 40 si +)
           (LUUi (list (first ejemplos) '(0 1 1 0 -30)) '(soleado 30 40 si))))
