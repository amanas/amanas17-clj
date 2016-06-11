(ns amanas17.maia.p3-0
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-4]
        [amanas17.maia.p1-5]
        [amanas17.maia.p1-6]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-4]
        [amanas17.maia.p2-5]
        [amanas17.maia.p2-6]
        [amanas17.maia.p2-7]
        [amanas17.maia.p2-9-0]
        [amanas17.maia.p2-9-1]
        [amanas17.maia.p2-9-2]))

(defmacro LDim
  "Macro de ayuda para interpretar evaluaciones en listas de decisión"
  [concepto ejemplo-sin-clase]
  `(let [[f# c# _ clazz#] ~concepto
         f## (eval f#)
         c## (condp = (name f#)
               "match-LUU" [(try (eval (first c#)) (catch Exception ex# (first c#)))
                            (try (eval (second c#)) (catch Exception ex# (second c#)))]
               c#)
         e# ~ejemplo-sin-clase]
     (when (f## c## e#) clazz#)))

;; Ejercicio 3.1
(defn LDi
  "Intérprete de listas de decisión"
  [concepto-LD ejemplo-sin-clase]
  (if (empty? concepto-LD)
    ejemplo-sin-clase
    (let [concepto (first concepto-LD)]
      (if-let [c (LDim concepto ejemplo-sin-clase)]
        (concat ejemplo-sin-clase [c])
        (LDi (rest concepto-LD) ejemplo-sin-clase)))))

(he-tardado 240 3.1)

(assert (= (LDi '((match-CL ((soleado) (30 40) (50 60) (*) (*) (*) (*)) => +)
                   (match-CL ((*) (*) (*) (*) (*) (*) (*)) => -))
                '(soleado 30 40 si contento relajado solvente))
           '(soleado 30 40 si contento relajado solvente -)))

(assert (= (LDi '((match-LUU ((first ejemplos) (0 1 1 0 0 0 0 -30)) => +)
                   (match-LUU ((first ejemplos) (0 1 -1 0 0 0 0 -20)) => -))
                '(soleado 30 40 si contento relajado solvente))
           '(soleado 30 40 si contento relajado solvente +)))

(assert (= (LDi '((match-LUU ((first ejemplos) (0 1 1 0 0 0 0 30)) => +)
                   (match-LUU ((first ejemplos) (0 1 -1 0 0 0 0 20)) => -))
                '(soleado 10 10 si contento relajado solvente))
           '(soleado 10 10 si contento relajado solvente)))













