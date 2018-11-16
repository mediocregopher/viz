(ns viz.ghost
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [viz.forest :as forest]
            [viz.grid   :as grid]
            clojure.set))

(defn new-ghost []
  {:active-node-poss #{}
   :color 0xFF000000
   })

(defn add-active-node [ghost pos]
  (update-in ghost [:active-node-poss] conj pos))

(defn- gen-new-poss [forest poss-fn pos]
  (poss-fn pos (forest/empty-adj-points forest pos)))

(defn incr [ghost forest poss-fn]
  (let [new-nodes
        (into {} (mapcat (fn [pos]
                           (map #(vector % [pos {:color (:color ghost)}])
                                (gen-new-poss forest poss-fn pos)))
                         (:active-node-poss ghost)))
        ]
    [(assoc ghost :active-node-poss (keys new-nodes))
     (reduce (fn [forest [pos [parent-pos node]]]
               (forest/spawn-child forest parent-pos pos node))
             forest new-nodes)]))
