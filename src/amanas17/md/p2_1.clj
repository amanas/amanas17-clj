(ns amanas17.md.p2_1
  (:require [clojure.math.combinatorics :as combo]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:use  [clj-ml.io ]
         [clj-ml.classifiers]
         [clj-ml.data]
         [clj-ml.utils]))

(defn xor [& more]
  (reduce (fn [a b] (if-not (= a b) 1 0)) more))

(def  vars [:A :B :C :D :E :Y])

(def file "resources/md-p1.dat")

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

;; A B C
;; D = b xor c
;; E
;; Y = a xor d
(defn generate-data []
  (->> #(let [a (Math/round (rand))
              b (Math/round (rand))
              c (Math/round (rand))
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
(prn (instances {:A 1}))

(defn P [conds]
  (->> [conds {}] (map instances) (map count) (apply /)))
(prn (P {:A 1}))

(defn Pcond [cond conds]
  (let [pos (instances (merge cond conds))
        all (instances conds)]
    (->> [pos all] (map count) (apply /))))
(prn (Pcond {:Y 1} {:A 0}))

(defn strong?
  ([cond conds] (or (= 0 (P (merge cond conds)))
                    (->> [(merge cond conds) conds]
                         (map (partial Pcond {:Y 1}))
                         (apply not=)))))

(prn (strong? {:A 1} {:C 0 :B 1}))

;; Sean a=true, b=true, c=true y d=true
;; Vamos a demostrar la relevancia fuerte de A.
;; (assert (not= 0 (P [:a :b :c])))
;; (assert (not= (Pcond :y [:a :b :c]) (Pcond :y [:b :c])))

;; Vamos a demostrar la relevancia débil de B
;; (assert (not= 0 (P [:a :c :d])))
;; (Pcond :y [:a :c :d])
;; (Pcond :y [:a :d])


;; data mining
(prn 1)
(def ds (-> (load-instances :csv file)
            (dataset-set-class :Y)))
(prn ds)
(count (dataset-seq ds))
;; (def classifier (-> (make-classifier :decision-tree :c45)
;;                     (classifier-train ds)))
;; (def evaluation (classifier-evaluate classifier :cross-validation ds 10))

;; (clojure.pprint/pprint (:summary evaluation))
