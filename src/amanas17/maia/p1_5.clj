(ns amanas17.maia.p1-5
  (:require [amanas17.maia.p1-1 :refer :all]
            [amanas17.maia.p1-3 :refer :all]
            [amanas17.maia.p1-4 :refer :all]))

;; Para los siguientes ejercicios, primero traduzco a clojure la función A0
(defn A0
  "Algoritmo de aprendizaje -  devuelve el concepto que se corresponde
   con la clase mayoritaria en los ejemplos de entrenamiento"
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

;; La variable ejemplos contiene los ejemplos originales; la variable extensión
;; contiene los mismos ejemplos pero todos ellos teniendo como valor de la clase
;  el mas común entre todos los ejemplos.
;; Claro que existen herrores: todos aquellos ejemplos que tenían una clase
;; que no era la más común entre la población total han sido modificados.
;; Ahora todos tienen la misma clase
(he-tardado 90 1.15)

;; Ejercicio 1.16
(defn calcula-precision
  "Calcula la precisión de una predicción
   Asume que ejemplos y extensión son listas con cabecera de descripción de atributos"
  [ejemplos extension]
  (let [todos (count (rest ejemplos))
        aciertos (->> [(rest ejemplos) (rest extension)]
                      (apply interleave)
                      (partition 2)
                      (filter (fn [[ej ex]](= (last ej) (last ex))))                                         count)]
    (/ aciertos todos)))

(defn calcula-error
  "Calcula el error de una predicción
   Asume que ejemplos y extensión son listas con cabecera de descripción de atributos"
  [ejemplos extension]
  (let [todos (count (rest ejemplos))
        fallos (->> [(rest ejemplos) (rest extension)] (apply interleave)
                    (partition 2)
                    (filter (fn [[ej ex]](not= (last ej) (last ex))))
                    count)]
    (/ fallos todos)))

(he-tardado 45 1.16)

;; Ejercicio 1.17
(def ejemplos2 (leer-ejemplos "resources/maia/ejemplos2.scm"))
(def ejemplos-sin-clase2 (map butlast ejemplos2))
(def extension2-A0-A0i (map (partial A0i esencia) ejemplos-sin-clase2))
(def precision2-A0-A0i (calcula-precision ejemplos2 extension2-A0-A0i))
(def error2-A0-A0i (calcula-error ejemplos2 extension2-A0-A0i))

;; Comentarios:
;; La precisión y el error son muy distintos al ser calculados sobre estos
;; nuevos ejemplos respecto de la calculada sobre el conjunto original.

;; Básicamente, el intérprete A0i no tiene en cuenta la información aportada
;; por las variables de cada ejemplo, sino que emite su valor atendiendo
;; a lo que indica que se hará por la mayoría de los ejemplos.
;; Por lo tanto, no es de esperar que aporte predicción válida.

;; Tanto la precisión como el error se corresponden con el hecho de que
;; casualmante en 2 ejemplos sí salimos y en 3 no.
;; Pero esto sólo tiene que ver con el azar propio de los 5 ejemplos elegidos
;; en este caso, no con mayor análisis de las propiedades de cada ejemplo.

(he-tardado 90 1.17)

;; Ejercicio 1.18
(defn A1
  "Algortimo de aprendizaje que devuelve la contabilización de los
   distintos valores de la clase en los ejemplos"
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

(he-tardado 60 1.18)


;; Ejercicio 1.19
;; Mediante los 5 ejemplos (días) anteriores compare la precisión de
;; A0/A0i respecto a la precisión de A1/A1i

;; Dado que en este caso, al depender de la función obtener-al-azar,
;; el resultado puede variar de una ejecución a otra, he encapsulado
;; todo el experimente en una sola función.

;; Repitiéndolo 10 veces obtengo estos pares de precisión/error
(def diez-repeticiones-A1-A1i
  (take 10 (repeatedly #(clasifica-con-A1-A1i ejemplos2))))

;; y haciendo la media, sólo para poder comentar con más rigor este ejercicio,
(->> diez-repeticiones-A1-A1i
     ((juxt (partial map first) (partial map second)))
     (map (partial apply +))
     (map #(/ % 10)))
;; obtengo que la precisión media es 12/25 y el error medio es 13/25.
;; Nótese que este ha sido el resultado de una ejecución cualquiera y que
;; cualquier otra podrá variar.

;; ¿Qué algoritmo considera que induce mejores conceptos?
;; No considero que este algoritmo aporte mejores predicciones puesto que
;; sólo depende de la frequencia con que se presentan los valores de la clase.
;; En A0/A0i, sin azar; todos se clasifican a la clase dominante.
;; Y en este caso A1/A1i, con azar, pero sin tener en cuenta otras
;; potenciales relaciones entre los distintos atributos de los ejemplos.

;; ¿Considera que ha sido rentable aumentar el lenguaje de descripción  de conceptos
;; de A0/A0i a A1/A1i haciendo el algoritmo A1/A1i más complejo?
;; Hasta donde llega mi conocimiento, diría que no, no creo que este algoritmo mejore
;; las predicciones.

(he-tardado 90 1.19)
