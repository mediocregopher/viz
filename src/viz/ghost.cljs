(ns viz.ghost
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [viz.forest :as forest]
            [viz.grid   :as grid]
            clojure.set))

(defn new-ghost []
  {:active-node-poss #{}
   })

(defn add-active-node [ghost pos]
  (update-in ghost [:active-node-poss] conj pos))

(defn- gen-poss-colors [forest pos poss-fn color-fn]
  (let [adj-poss (poss-fn pos (forest/adj-empty-poss forest pos))]
    (map (fn [adj-pos adj-adj-nodes] [adj-pos (color-fn adj-adj-nodes)])
         adj-poss (map #(forest/adj-nodes forest %) adj-poss))))

(defn incr [ghost forest poss-fn color-fn]
  (let [new-nodes
        (mapcat (fn [parent-pos]
                  (map (fn [[pos color]]
                         [pos [parent-pos {:color color}]])
                       (gen-poss-colors forest parent-pos poss-fn color-fn)))
                (:active-node-poss ghost))]
    [(assoc ghost :active-node-poss (map first new-nodes))
     (reduce (fn [forest [pos [parent-pos node]]]
               (forest/spawn-child forest parent-pos pos node))
             forest new-nodes)]))
