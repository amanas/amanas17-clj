(ns amanas17.maia.p3-1
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-7]))


;; Ejercicio 3.1
(defn LDi
  "Intérprete de listas de decisión"
  [concepto-LD ejemplo-sin-clase]
  (if (empty? concepto-LD)
    ejemplo-sin-clase
    (let [[f c _ clazz] (first concepto-LD)]
      (if ((eval f) c ejemplo-sin-clase)
        (concat ejemplo-sin-clase [clazz])
        (LDi (rest concepto-LD) ejemplo-sin-clase)))))

(assert (= (LDi '((match-CL ((soleado) (30 40) (50 60) (*)) => +)
                   (match-CL ((*) (*) (*) (*)) => -))
                '(soleado 30 40 si))
           '(soleado 30 40 si -)))

(assert (= (LDi '((match-LUU ((first ejemplos) (0 1 1 0 -30)) => +)
                    (match-LUU ((first ejemplos) (0 1 -1 0 -20)) => -))
                '(soleado 30 40 si))
           '(soleado 30 40 si +)))

(~l)

(match-LUU '((first ejemplos) (0 1 1 0 -30))
           '(soleado 30 40 si))

(type (first '((first ejemplos) (0 1 1 0 -30))))
(type (first '(gg (0 1 1 0 -30))))
(he-tardado 60 3.1)