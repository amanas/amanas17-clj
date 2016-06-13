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
        [amanas17.maia.p2-9-2]
        [amanas17.maia.p3-2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Induccion conceptos JC-adc mediante DDT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Devuelve la capacidad de discriminaión de un discriminante.
  Asume que los ejemplos tienen cabecera de atributos."
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
  El resto de la lista son el resto de discriminantes.
  Asume que los ejemplos tienen cabecera de descripción de atributos"
  [lista-de-discriminantes ejemplos-disponibles]
  (let [mayor-d (->> lista-de-discriminantes
                     reverse
                     (sort-by (fn [d] (capacidad-de-discriminacion1 d ejemplos-disponibles)))
                     last)]
    (concat [mayor-d] (remove (fn [d] (= d mayor-d)) lista-de-discriminantes))))

(assert (= (mayor-discriminante
             '((perspectiva 0 (soleado lluvioso))
                (temperatura 1 numerico 10)
                (temperatura 1 numerico 25)
                (temperatura 1 numerico 30))
             '(((perspectiva (soleado lluvioso))
                 (temperatura numerico)
                 (clase (+ -)))
                (soleado 10 -) (soleado 25 +) (lluvioso 30 -)))
           '((perspectiva 0 (soleado lluvioso))
              (temperatura 1 numerico 10)
              (temperatura 1 numerico 25)
              (temperatura 1 numerico 30))))

(he-tardado 40 3.14)

;; Ejercicio 3.15

(defn division-desc->adc-desc
  "Transforma el resultado de dividir ejemplos con un discriminante a una secuencia
  de símbolos que puede consistente con adc y matchCL"
  [[a b c d :as discriminante] metadatos [desc & ejemplos :as division]]
  (let [*seq (repeat '(*))
        new-desc (if (= 'numerico c)
                   (let [[s v] desc]
                     (if (= '>= s) (list [v] '+inf) (list '-inf v)))
                   (list desc))]
    (concat (take b *seq) [new-desc] (take (- (count metadatos) b 2) *seq))))

(defn generate-DDT0-tree
  [discriminantes [metadatos & ejemplos :as input]]
  (if (empty? ejemplos)
    (list '=> 'UNKNOWN)
    (if (or (empty? discriminantes)
            (= 1 (count (distinct (map last ejemplos)))))
      (list '=> (last (first ejemplos)))
      (let [mayor-d (first (mayor-discriminante discriminantes input))
            rest-d (remove (partial = mayor-d) discriminantes)
            divisiones (dividir-ejemplos mayor-d ejemplos)]
        (map (fn [[desc & ejemplos :as division]]
               (list (division-desc->adc-desc mayor-d metadatos division)
                     '->
                     (generate-DDT0-tree rest-d (concat [metadatos] ejemplos))))
             divisiones)))))


(defn DDT0
  ""
  [discriminantes [metadatos & ejemplos :as input]]
  (concat ['adc] (generate-DDT0-tree discriminantes input)))

(assert (= (DDT0 '((perspectiva 0 (soleado lluvioso nublado))
                    (temperatura 1 numerico 10)
                    (temperatura 1 numerico 25)
                    (temperatura 1 numerico 30))
                 '(((perspectiva (soleado lluvioso nublado))
                     (temperatura numerico)
                     (clase (+ -)))
                    (soleado 10 +) (lluvioso 25 -) (nublado 30 +))))
        '(adc
           (((soleado) (*)) -> (=> +))
           (((lluvioso) (*)) -> (=> -))
           (((nublado) (*)) -> (=> +))))

(defn DDT
  [ejemplos]
  (DDT0 (generar-discriminantes (first ejemplos) (rest ejemplos)) ejemplos))


(DDT ejemplos)
