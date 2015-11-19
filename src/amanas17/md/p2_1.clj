(ns amanas17.md.p2_1
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn xor [& more]
  (reduce not= more))

(def  vars [:A :B :C :D :E :Y])

(def file "resources/md/p2_1/p2_1.dat")

(defn load-data []
  (when (.exists (io/as-file file))
    (with-open [in-file (io/reader file)]
      (->>  (csv/read-csv in-file)
            doall
            rest
            (map (fn [l] (map load-string l)))
            (map (partial zipmap vars))))))

(defn save-data [data]
  (with-open [out-file (io/writer file)]
    (csv/write-csv out-file [(map name vars)])
    (csv/write-csv out-file (map #(map %1 vars) data))))

;; A B C random
;; D = b xor c
;; E
;; Y = a xor d
(defn generate-data []
  (->> #(let [a (< 0.5 (Math/round (rand)))
              b (< 0.5 (Math/round (rand)))
              c (< 0.5 (Math/round (rand)))
              d (xor b c)
              e (float (rand))
              y (xor a d)]
          (zipmap vars [a b c d e y]))
       (repeatedly)
       (take 100)))

(def data (or (load-data)
              (save-data (generate-data))
              (load-data)))


(defn submap? [sub m]
  (.containsAll (.entrySet m) (.entrySet sub)))

(defn instances [conds]
  (filter (partial submap? conds) data))
(prn (instances {:A true}))

(defn P [conds]
  (->> [conds {}] (map instances) (map count) (apply /)))
(prn (P {:A false}))

(defn Pcond [cond conds]
  (let [pos (instances (merge cond conds))
        all (instances conds)]
    (->> [pos all] (map count) (apply /))))
(prn (Pcond {:Y true} {:A false}))

(defn strong?
  ([cond conds] (or (= 0 (P (merge cond conds)))
                    (->> [(merge cond conds) conds]
                         (map (partial Pcond {:Y true}))
                         (apply not=)))))

(prn (strong? {:A true} {:C false :B true}))

;; Sean a=true, b=true, c=true y d=true
;; Vamos a demostrar la relevancia fuerte de A.
;; (assert (not= 0 (P [:a :b :c])))
;; (assert (not= (Pcond :y [:a :b :c]) (Pcond :y [:b :c])))

;; Vamos a demostrar la relevancia débil de B
;; (assert (not= 0 (P [:a :c :d])))
;; (Pcond :y [:a :c :d])
;; (Pcond :y [:a :d])
