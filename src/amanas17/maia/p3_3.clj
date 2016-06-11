(ns amanas17.maia.p3-3
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

;; Ejercicio 3.11
(defn dividir-ejemplos
  "Divide los ejemplos según el discriminante.
  En el caso de un discriminante nominal, la función devolverá una lista
  con varios conjuntos de ejemplos: uno por cada valor posible del atributo
  nominal correspondiente. El primer elemento de cada nuevo conjunto/lista
  de ejemplos comenzará por el valor (del atributo) con el que han sido dis-criminados
  En el caso de un discriminante num ́erico, la función devolverá una lista
  con dos conjuntos de ejemplos: uno en el que se cumpla que los valores
  numéericos sean superiores y otro en que los valores sean inferiores
  al umbral de discriminación. El primer elemento de cada nuevo conjunto/lista
  de ejemplos comenzará por (>= <umbral>) o (< <umbral>) según haya sido discriminado.
  Asume que los ejemplos NO tienen cabecera de definición de metadatos."
  [discriminante ejemplos-sin-metadatos]
  (if (= 3 (count discriminante))
    ;; nominal
    (let [[_ posicion valores] discriminante
          groups (group-by (fn [e] (nth e posicion)) ejemplos-sin-metadatos)]
      (for [valor valores]
        (concat [valor] (get groups valor))))
    ;; numérico
    (let [[_ posicion _ umbral] discriminante
          ge (list (list '>= umbral))
          lt (list (list '< umbral))
          groups (group-by (fn [e] (if (< (nth e posicion) umbral) lt ge)) ejemplos-sin-metadatos)]
      (list (concat ge (get groups ge))
            (concat lt (get groups lt))))))

(assert (= (dividir-ejemplos
             '(perspectiva 0 (soleado lluvioso))
             '((soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           '((soleado (soleado 10 -) (soleado 25 +))
              (lluvioso (lluvioso 30 -)))))

(assert (= (dividir-ejemplos
             '(temperatura 1 numerico 25)
             '((soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           '(((>= 25) (soleado 25 +) (lluvioso 30 -))
              ((< 25) (soleado 10 -)))))

(he-tardado 60 3.11)


;; Ejercicio 3.12
(defn generar-discriminantes
  "Devuelve un discriminante nominal por cada atributo nominal
  y un discriminante numérico por cada atributo y valor numérico diferente
  de todo el conjunto de ejemplos de entrenamiento"
  [metadatos ejemplos]
  (apply concat
         (for [indice (range (count metadatos))]
           (let [[nombre valor] (nth metadatos indice)]
             (if (= 'numerico valor)
               (->> ejemplos (map #(nth % indice)) distinct sort
                    (map (fn [v] (list nombre indice 'numerico v))))
               (list (list nombre indice valor)))))))

(assert (= (generar-discriminantes
             '((perspectiva (soleado lluvioso))
                (temperatura numerico))
             '((soleado 10 -)
                (soleado 25 +)
                (lluvioso 30 -)))
           '((perspectiva 0 (soleado lluvioso))
              (temperatura 1 numerico 10)
              (temperatura 1 numerico 25)
              (temperatura 1 numerico 30))))

(he-tardado 60 3.12)


;; Ejercicio 3.13
(defn capacidad-de-discriminacion1
  [discriminante [metadatos & ejemplos :as input]]
  (/ (reduce +
             (for [[nombre & ejemplos] (dividir-ejemplos discriminante ejemplos)]
               (let [esencia (A0 input)
                     extension (map (partial A0i esencia) (map butlast ejemplos))]
                 (->> (interleave ejemplos extension)
                      (partition 2)
                      (filter (fn [[ej ex]] (= (last ej) (last ex))))
                      count))))
     (count ejemplos)))

;; No puedo utilizar A0 a menos que incluya la cabecera de metadatos
;; Por eso modifico sutilmente el input de los ejemplos propuestos en el material
;; de estudio
(assert (= (capacidad-de-discriminacion1
             '(perspectiva 0 (soleado lluvioso))
             '(((perspectiva (soleado lluvioso))
                 (temperatura numerico)
                 (clase (+ -)))
                (soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           2/3))

(assert (= (capacidad-de-discriminacion1
             '(temperatura 1 numerico 25)
             '(((perspectiva (soleado lluvioso))
                 (temperatura numerico)
                 (clase (+ -)))
                (soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           2/3))

(assert (= (capacidad-de-discriminacion1
             '(temperatura 1 numerico 10)
             '(((perspectiva (soleado lluvioso))
                 (temperatura numerico)
                 (clase (+ -)))
                (soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           2/3))

(he-tardado 90 2.13)

;; Ejercicio 3.14
(defn mayor-discriminante
  "Devuelva una lista en la que el primer elemento es el discriminante que mayor valor
  obtuvo en la función capacidad-de-discriminacion.
  El resto de la lista son el resto de discriminantes."
  [lista-de-discriminantes ejemplos-disponibles]
  (let [mayor-discriminate (->> lista-de-discriminantes
                                (sort-by (fn [d] (capacidad-de-discriminacion1 d ejemplos-disponibles)))
                                last)]
    (concat [mayor-discriminate] (remove (fn [d] (= d mayor-discriminate)) lista-de-discriminantes))))

(assert (= (mayor-discriminante
             '((perspectiva 0 (soleado lluvioso))
                (temperatura 1 numerico 10)
                (temperatura 1 numerico 25)
                (temperatura 1 numerico 30))
             '(((perspectiva (soleado lluvioso))
                 (temperatura numerico)
                 (clase (+ -)))
                (soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           '((temperatura 1 numerico 30)
              (perspectiva 0 (soleado lluvioso))
              (temperatura 1 numerico 10)
              (temperatura 1 numerico 25))))

(he-tardado 40 3.14)

;; Ejercicio 3.15
(defn DDT0
  ""
  [lista-de-discriminantes-disponibles ejemplos-disponibles])

(DDT0
  '((perspectiva 0 (soleado lluvioso nublado))
     (temperatura 1 numerico 10)
     (temperatura 1 numerico 25)
     (temperatura 1 numerico 30))
  '((soleado 10 +) (lluvioso 25 -) (nublado 30 +)))
;Imaginando que se ha escogido "perspectiva" como
;más discriminante, entonces lo que devolvería esta llamada
;sería algo como lo siguiente, recursivamente...
'(adc
   (((soleado) (*)) -> (DDT0 '((temperatura 1 numerico 10)
                                (temperatura 1 numerico 25)
                                (temperatura 1 numerico 30))
                             (filter (lambda (ej) (match-CL '((soleado) (*)) (drop-right ej 1)))
                                     ejemplos-disponibles)))
   (((lluvioso) (*)) -> ...)
   (((nublado) (*)) -> ...))

