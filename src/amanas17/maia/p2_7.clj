(ns amanas17.maia.p2-7
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-2]))

;; Ejercicio 23
(defn traducir
  "Devuelve la traduccion de un atributo nominal o valor si
   el atributo es numérico"
  [meta-atributo valor]
  (if (= 'numerico (second meta-atributo)) valor
    (first (keep-indexed #(if (= valor %2) %1) (second meta-atributo)))))

(he-tardado 30 2.23)

;; Ejercicio 24
(defn nuevo-conceptoUU
  "Devuelve un conceptoUU en el cual el vector está inicializado
   a valores al azar entre -<init> y +<init>"
  [metadatos init]
  [metadatos (take (count metadatos) (repeatedly #(* init(- (* 2 (rand)) 1))))])

(he-tardado 20 2.24)


;; Ejercicio 25
(defn match-LUU
  "Devuelve true si el ejemplo supera o iguala el umbral del concepto"
  [[metadatos values :as conceptoUU] ejemplo-sin-clase]
  (let [traduccion (->> ejemplo-sin-clase
                        (interleave (butlast metadatos))
                        (partition 2)
                        (map (partial apply traducir)))
        producto (->> traduccion
                      (interleave values)
                      (partition 2)
                      (map (partial apply *))
                      (apply +))]
    (<= (last values) producto)))

(he-tardado 30 2.25)

;; Ejercicio 26
(defn LUUi
  "Intèrprete de conceptos LUU"
  [conceptoUU ejemplo-sin-clase]
  (concat ejemplo-sin-clase [(if (match-LUU conceptoUU ejemplo-sin-clase) '+ '-)]))

(he-tardado 15 2.26)
