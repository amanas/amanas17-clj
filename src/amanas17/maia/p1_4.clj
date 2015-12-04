(ns amanas17.maia.p1-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-2]
        [amanas17.maia.p1-3]))

;; Para los siguientes ejercicios, primero traduzco a clojure la función A0
(defn A0 [ejemplos]
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

(defn A0i [concepto ejemplo-sin-clase]
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
(def precision (let [todos (count (rest ejemplos))
                     aciertos (->> [(rest ejemplos) (rest extension)] (apply interleave)
                                   (partition 2)
                                   (filter (fn [[ej ex]](= (last ej) (last ex))))
                                   count)]
                 (/ aciertos todos)))
(def error (let [todos (count (rest ejemplos))
                 aciertos (->> [(rest ejemplos) (rest extension)] (apply interleave)
                               (partition 2)
                               (filter (fn [[ej ex]](not= (last ej) (last ex))))
                               count)]
             (/ aciertos todos)))
(he-tardado 45 16)
(assert (= precision 13/20))
