(ns amanas17.maia.p1-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-2]
        [amanas17.maia.p1-3]))

(def a (atom {:a 0 :b 0}))
(swap! a assoc :a (inc (get @a :a)))


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
        clases-contabilizadas (atom (zipmap clases-posibles (constantly 0)))
        ;;variable sin asignacion, de momento
        concepto (atom nil)

        ;;Asignacion de FUNCIONES locales.
        ;;==================================
        ;;funcion que admite como parametro un ejemplo, el cual utiliza
        ;;para actualizar la contabilizacion de clases
        actualizar-contabilizacion (fn [ejemplo]
                                     (let [clase (nth ejemplo indice-clase)]
                                       (swap! clases-contabilizadas assoc clase (inc (get @clases-contabilizadas clase)))))]
    ;;fin de las asignaciones let*
    ;;Ahora, por cada ejemplo de entrenamiento,
    ;; se actualiza la contabilizacion de clases.
    (for-each actualizar-contabilizacion ejemplos)
    ;;Finalmente se escoge la clase que mas veces ha aparecido:
    ;;primero, se obtiene el numero maximo;
    (set! concepto (apply max (map cdr clases-contabilizadas)))
    ;;segundo, se obtiene la clase con ese numero maximo
    ;; (almacenado temporalmente en la variable concepto).
    (set! concepto
          (first (find (lambda(x) (= (cdr x) concepto)) clases-contabilizadas)))
    ;;Y por ultimo se devuelve el concepto inducido por el algoritmo.
    concepto))
