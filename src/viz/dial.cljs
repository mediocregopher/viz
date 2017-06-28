(ns viz.dial
  (:require [quil.core :as q :include-macros true]
            ))

(defn new-dial []
  {:val 0
   :min -1
   :max 1
   })

(defn scaled [dial min max]
  (let [new-val (q/map-range (:val dial) (:min dial) (:max dial) min max)]
    (assoc dial :min min :max max :val new-val)))

(defn floored [dial at]
  (if (< (:val dial) at)
    (assoc dial :val at)
    dial))

(defn invert [dial]
  (assoc dial :val (* -1 (:val dial))))
