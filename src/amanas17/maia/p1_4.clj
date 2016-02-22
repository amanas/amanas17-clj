(ns amanas17.maia.p1-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-2]
        [amanas17.maia.p1-3]))

;; Para los siguientes ejercicios, primero traduzco a clojure la función A0
(defn A0
  "Algoritmo de aprendizaje -  devuelve el concepto que se corresponde con la clase mayoritaria en los ejemplos de entrenamiento"
  [ejemplos]
  (let [;;Asignacion de VARIABLES locales
        ;;==================================
        atributos (first ejemplos)
        casos (rest ejemplos)
        ;;el indice de :clase en la lista atributos.
        indice-clase (->> atributos
                          (map first)
                          (keep-indexed (fn [i v] (when (= :clase v) i)))
                          first)
        clases-posibles (second (nth atributos indice-clase))
        ;;variable que mantiene la cuenta de las apariciones de cada clase.
        ;;Como primer paso, se inicializa a 0 la cuenta de cada clase.
        clases-contabilizadas (atom (zipmap clases-posibles (repeat 0)))
        ;;variable sin asignacion, de momento
        concepto (atom nil)

        ;;Asignacion de FUNCIONES locales.
        ;;==================================
        ;;funcion que admite como parametro un ejemplo, el cual utiliza
        ;;para actualizar la contabilizacion de clases
        actualizar-contabilizacion (fn [ejemplo]
                                     (let [clase (nth ejemplo indice-clase)]
                                       (swap! clases-contabilizadas  assoc clase
                                              (inc (get @clases-contabilizadas clase)))))]
    ;;fin de las asignaciones let
    ;;Ahora, por cada ejemplo de entrenamiento,
    ;; se actualiza la contabilizacion de clases.
    (doall (map  actualizar-contabilizacion casos))
    ;;Finalmente se escoge la clase que mas veces ha aparecido:
    ;;primero, se obtiene el numero maximo;
    (reset! concepto (apply max (vals @clases-contabilizadas)))
    ;;segundo, se obtiene la clase con ese numero maximo
    ;; (almacenado temporalmente en la variable concepto).
    (reset! concepto
          (ffirst (filter (fn [[k v]] (= v @concepto)) @clases-contabilizadas)))
    ;;Y por ultimo se devuelve el concepto inducido por el algoritmo.
    @concepto))

(defn A0i
  "Intérprete del algoritmo A0"
  [concepto ejemplo-sin-clase]
  (concat ejemplo-sin-clase [concepto]))


(def esencia (A0 ejemplos))
(def ejemplos-sin-clase (map butlast ejemplos))
(def extension (map (partial A0i esencia) ejemplos-sin-clase))

;; Ejercicio 15
;; Tras ejecutar los comandos anteriores, comente las diferencias
;; existentes entre el contenido de la variable ejemplos y el contenido de
;; la variable extension. Existen errores? Es natural que existan?

;; La variable ejemplos contiene los ejemplos originales; la variable extensión contiene
;; los mismos ejemplos pero todos ellos teniendo como valor de la clase el mas común entre
;; todos los ejemplos.
;; Claro que existen herrores: todos aquellos ejemplos que tenían una clase que no era
;; la más común entre la población total han sido modificados. Ahora todos tienen la misma
;; clase
(he-tardado 90 15)

;; Ejercicio 16
(defn calcula-precision [ejemplos extension]
  (let [todos (count (rest ejemplos))
        aciertos (->> [(rest ejemplos) (rest extension)]
                      (apply interleave)
                      (partition 2)
                      (filter (fn [[ej ex]](= (last ej) (last ex))))                                         count)]
    (/ aciertos todos)))
(def precision (calcula-precision ejemplos extension))
(assert (= precision 13/20))

(defn calcula-error [ejemplos extension]
  (let [todos (count (rest ejemplos))
        fallos (->> [(rest ejemplos) (rest extension)] (apply interleave)
                    (partition 2)
                    (filter (fn [[ej ex]](not= (last ej) (last ex))))
                    count)]
    (/ fallos todos)))
(def error (calcula-error ejemplos extension))
(assert (= error 7/20))
(he-tardado 45 16)

;; Ejercicio 17
(def ejemplos2 (leer-ejemplos "resources/maia/ejemplos2.scm"))
(def ejemplos-sin-clase2 (map butlast ejemplos2))
(def extension2-A0-A0i (map (partial A0i esencia) ejemplos-sin-clase2))
(def precision2-A0-A0i (calcula-precision ejemplos2 extension2-A0-A0i))
(def error2-A0-A0i (calcula-error ejemplos2 extension2-A0-A0i))
(assert (= precision2-A0-A0i 3/5))
(assert (= error2-A0-A0i 2/5))

;; Comentarios:
;; La precisión y el error son muy distintos al ser calculados sobre estos nuevos ejemplos
;; respecto de la calculada sobre el conjunto original.

;; Básicamente, el intérprete A0i no tiene en cuenta la información aportada por las variables
;; de cada ejemplo, sino que emite su valor atendiendo a lo que indica que se hará por la mayoría
;; de los ejemplos. Por lo tanto, no es de esperar que aporte predicción válida.

;; Tanto la precisión como el error se corresponden con el hecho de que casualmante en 2
;; ejemplos sí salimos y en 3 no. Pero esto sólo tiene que ver con el azar propio de los 5
;; ejemplos elegidos en este caso, no con mayor análisis de las propiedades de cada ejemplo.

(he-tardado 90 17)

;; Ejercicio 18
(defn A1
  "Algortimo de aprendizaje que devuelve la contabilización de los distintos valores de la clase en los ejemplos"
  [ejemplos]
  (let [;;Asignacion de VARIABLES locales
        ;;==================================
        atributos (first ejemplos)
        casos (rest ejemplos)
        ;;el indice de :clase en la lista atributos.
        indice-clase (->> atributos
                          (map first)
                          (keep-indexed (fn [i v] (when (= :clase v) i)))
                          first)
        clases-posibles (second (nth atributos indice-clase))
        ;;variable que mantiene la cuenta de las apariciones de cada clase.
        ;;Como primer paso, se inicializa a 0 la cuenta de cada clase.
        clases-contabilizadas (atom (zipmap clases-posibles (repeat 0)))
        ;;variable sin asignacion, de momento
        concepto (atom nil)

        ;;Asignacion de FUNCIONES locales.
        ;;==================================
        ;;funcion que admite como parametro un ejemplo, el cual utiliza
        ;;para actualizar la contabilizacion de clases
        actualizar-contabilizacion (fn [ejemplo]
                                     (let [clase (nth ejemplo indice-clase)]
                                       (swap! clases-contabilizadas  assoc clase
                                              (inc (get @clases-contabilizadas clase)))))]
    ;;fin de las asignaciones let
    ;;Ahora, por cada ejemplo de entrenamiento,
    ;; se actualiza la contabilizacion de clases.
    (doall (map  actualizar-contabilizacion casos))
    (vec @clases-contabilizadas)))
(assert  (= (A1 ejemplos) [[:+ 7] [:- 13]]))

(defn A1i
  "Infiere la clase de acuerdo con la frecuencia que ésta presenta en los ejemplos"
  [concepto ejemplo-sin-clase]
  (concat ejemplo-sin-clase [(first (obtener-al-azar concepto))]))

(defn clasifica-con-A1-A1i [ejemplos2]
  (let [ejemplos-sin-clase2 (map butlast ejemplos2)
        extension2-A1-A1i (map (partial  A1i (A1 ejemplos)) ejemplos-sin-clase2)
        precision2-A1-A1i (calcula-precision ejemplos2 extension2-A1-A1i)
        error2-A1-A1i (calcula-error ejemplos2 extension2-A1-A1i)]
    [precision2-A1-A1i error2-A1-A1i]))

(comment "One classification"
         (clasifica-con-A1-A1i ejemplos2))

(he-tardado 60 18)


;; Ejercicio 19
;; Mediante los 5 ejemplos (días) anteriores compare la precisión de A0/A0i respecto a
;; la precisión de A1/A1i

;; Dado que en este caso, al depender de la función obtener-al-azar, el resultado puede
;; variar de una ejecución a otra, he encapsulado todo el experimente en una sola función.
;; Repitiéndolo 10 veces obtengo estos pares de precisión/error
(def diez-repeticiones-A1-A1i
  (take 10 (repeatedly #(clasifica-con-A1-A1i ejemplos2))))
;; y haciendo la media, sólo para poder comentar con más rigor este ejercicio,
(->> diez-repeticiones-A1-A1i
     ((juxt (partial map first) (partial map second)))
     (map (partial apply +))
     (map #(/ % 10)))
;; obtengo que la precisión media es 12/25
;; y el error medio es 13/25. Nótese que este ha sido el resultado de una ejecución
;; cualquiera y que cualquier otra podrá variar.

;; ¿Qué algoritmo considera que induce mejores conceptos?
;; No considero que este algoritmo aporte mejores predicciones puesto que sólo depende
;; de la frequencia con que se presentan los valores de la clase.
;; En A0/A0i, sin azar; todos se clasifican a la clase dominante.
;; Y en este caso A1/A1i, con azar, pero sin tener en cuenta otras potenciales relaciones
;; entre los distintos atributos de los ejemplos.

;; ¿Considera que ha sido rentable aumentar el lenguaje de descripción  de conceptos
;; de A0/A0i a A1/A1i haciendo el algoritmo A1/A1i más complejo?
;; Hasta donde llega mi conocimiento, diría que no, no creo que este algoritmo mejore
;; las predicciones.

(he-tardado 90 19)


;; Ejercicio 20
(def concepto-inicial [[:+ 0] [:- 0]])
(defn IIA1 [descripcion-atributos concepto ejemplo]
  (let [cm (apply hash-map (flatten concepto))]
    (seq (assoc cm (last ejemplo) (inc (get cm (last ejemplo)))))))

(assert (= [[:- 0] [:+ 1]] (IIA1 (first ejemplos) concepto-inicial [:nublado 25 80 :no :+])))

(he-tardado 30 20)

;; Ejercicio 21
(defn IA1
  "Implementación incremental del algoritmo A1"
  [ejemplos]
  (reduce (partial IIA1 (first ejemplos)) concepto-inicial (rest ejemplos)))

(assert (= [[:- 13] [:+ 7]] (IA1 ejemplos)))

(he-tardado 15 21)


;; Ejercicio 22

(def all-ejemplos (mezclar ejemplos ejemplos2))

(defn resustitution
  "Calcula la precición del algoritmo de aprendizaje utilizando
   todos los casos para entrenar y evaluar"
  [entrenador interprete ejemplos]
  (let [concepto (entrenador ejemplos)
        ejemplos-sin-clase (map butlast ejemplos)
        extension (map (partial interprete concepto) ejemplos-sin-clase)]
    (calcula-precision ejemplos extension)))

(comment "Precisión con resustitución  A0 A0i"
         (resustitution  A0 A0i all-ejemplos))
(comment "Precisión con resustitución  A1 A1i"
         (resustitution  A1 A1i all-ejemplos))
(comment "Precisión con resustitución IA1 A1i"
         (resustitution IA1 A1i all-ejemplos))

(defn leave-one-out
  "Por cada ejemplo:
    - se toma el ejemplo como prueba
    - se toma el conjunto complementario de ejemplos como entrenamiento
   Todas estas pruebas se agregan para conseguir el resultado final"
  ([entrenador interprete ejemplos]
   (let [precisiones (map (partial leave-one-out entrenador interprete ejemplos)
                          (->> ejemplos count dec range))]
     (/ (apply + precisiones) (count precisiones))))
  ([entrenador interprete ejemplos index]
   (let [desc (first ejemplos)
         body (rest ejemplos)
         prueba [desc (nth body index)]
         entrenamiento (concat [desc] (take index body) (drop (inc index) body))
         concepto (entrenador entrenamiento)
         extension (map (partial interprete concepto) (map butlast prueba))]
     (calcula-precision prueba extension))))


(comment "Precisión con leave-one-out  A0 A0i"
         (leave-one-out  A0 A0i all-ejemplos))
(comment "Precisión con leave-one-out  A1 A1i"
         (leave-one-out  A1 A1i all-ejemplos))
(comment "Precisión con leave-one-out IA1 A1i"
         (leave-one-out IA1 A1i all-ejemplos))

(he-tardado 360 22)


;; Ejercicio 23
(defn holdout
  "Técnica de aprendizaje consistente en entrenar con el conjunto entrenamiento
   y evaluar con el conjunto evaluacion"
  [entrenador interprete entrenamiento evaluacion]
  (let [concepto (entrenador entrenamiento)
        extension (map (partial interprete concepto) (map butlast evaluacion))]
    (calcula-precision evaluacion extension)))

(defn separar-desc-sensitive
  "Separa teniendo en cuenta la descripción en la lista de entrada
   y agregándola en las listas de salida"
  [p [desc & body :as ejemplos]]
  (->> body
       (separar p)
       (map (fn [fold] (into [desc] fold)))))

(let [[entrenamiento evaluacion] (separar-desc-sensitive 2/3 all-ejemplos)]
  (prn "Precisión con holdout  A0 A0i"
       (holdout  A0 A0i entrenamiento evaluacion))
  (prn "Precisión con holdout  A1 A1i"
       (holdout  A1 A1i entrenamiento evaluacion))
  (prn "Precisión con holdout IA1 A1i"
       (holdout IA1 A1i entrenamiento evaluacion)))

(he-tardado 90 23)


;; Ejercicio 24
(defn folds-desc-sensitive
  "Genera folds teniendo en cuenta la descripción en la lista de entrada
   y agregándola en las listas de salida"
  [[desc & body :as ejemplos] n]
  (->> (folds body  n)
       (map (fn [fold] (into [desc] fold)))))

(defn cross-validation
  "Técnica de apendizaje consistente en:
    - generar tantos folds en el conjunto entrenamiento como indica n-folds
    - para cada fold entrenar en el conjunto de ejemplos que no están en el fold
    - para cada fold evaluar con el conjunto de ejemplos del fold
    - devuelve la media de las precisiones"
  [entrenador interprete entrenamiento n-folds]
  (let [folds (folds-desc-sensitive entrenamiento n-folds)
        entre-eval-groups (for [n (range n-folds)]
                            (let [entrenamiento (->> (drop (inc n) folds)
                                                     (into (take n folds))
                                                     (reduce mezclar))
                                  evaluacion (nth folds n)]
                              [entrenamiento evaluacion]))
        precisiones (->> entre-eval-groups
                         (map (partial apply holdout entrenador interprete)))]
    (/ (apply + precisiones) (count precisiones))))

;; Se comprueba el test indicado en el material de la práctica, pero teniendo en
;; cuenta que tenemos 25 ejemplos en lugar de 3
(assert (= (cross-validation A0 A0i all-ejemplos 25)
           (leave-one-out A0 A0i all-ejemplos)))
