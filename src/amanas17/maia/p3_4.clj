(ns amanas17.maia.p3-4
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
        [amanas17.maia.p2-9-2]
        [amanas17.maia.p3-2]
        [amanas17.maia.p3-3]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Transformacion conceptos JC -> DL ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 3.18
(defn union-CL
  "Devuelve el <concepto-CL1> pero sustituyendo cada test (*) por aquel que aparezca en <concepto-CL2>"
  [concepto-CL1 concepto-CL2]
  (for [i (range (count concepto-CL1)) ]
    (let [t (nth concepto-CL1 i)]
      (if (= t '(*)) (nth concepto-CL2 i) t))))

(assert (= (union-CL '((soleado)(*)) '((*)(10 30)))
           '((soleado)(10 30))))

(he-tardado 30 3.18)

;; Ejercicio 3.19
(defn DTL
  "Transforma un arbol de decisión clásico en una lista de decisión"
  ([arbol-decision-clasico]
   (DTL arbol-decision-clasico [] []))
  ([N RULES LHS]
   (cond
     (= '=> (first N))  (concat RULES  [(list 'match-CL (reduce union-CL LHS) '=> (second N))])
     (= 'adc (first N)) (apply concat (for [[T _ S] (rest N)]
                                        (DTL S RULES (concat LHS [T])))))))



(assert (= (DTL '(adc
                  (((soleado)(*)) ->
                   (adc
                    (((*)(-inf 30)) -> (=> +))
                    (((*)((30) +inf)) -> (=> -))))
                  (((nublado)(*)) -> (=> -))
                  (((lluvioso)(*)) ->
                   (adc
                    (((*)(-inf 10)) -> (=> -))
                    (((*)((10) +inf)) -> (=> +))))))
           '((match-CL ((soleado)(-inf 30)) => +)
             (match-CL ((soleado)((30) +inf)) => -)
             (match-CL ((nublado)(*)) => -)
             (match-CL ((lluvioso)(-inf 10)) => -)
             (match-CL ((lluvioso)((10) +inf)) => +))))

(defn DTL-DDT
  [ejemplos]
  (DTL (DDT ejemplos)))

(he-tardado 200 3.19)
