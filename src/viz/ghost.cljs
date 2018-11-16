(ns viz.ghost
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [viz.forest :as forest]
            [viz.grid   :as grid]
            clojure.set))

(defn new-ghost []
  {:active-node-ids #{}
   :color 0xFF000000
   })

(defn add-active-node [ghost id]
  (update-in ghost [:active-node-ids] conj id))

(defn rm-active-node [ghost id]
  (update-in ghost [:active-node-ids] disj id))


(defn- gen-new-poss [forest poss-fn id]
  "generates new positions branching from the given node"
  (let [pos (:pos (forest/get-node forest id))
        adj-poss (forest/empty-adj-points forest pos)
        ]
    (poss-fn pos adj-poss)))

(defn incr [ghost forest poss-fn]
  (let [active-node-ids (:active-node-ids ghost)
        [forest new-ids]
        (reduce (fn [[forest new-ids] [pos node-def]]
                  (let [[forest new-id]
                        (forest/spawn-child forest
                                            (:parent node-def)
                                            pos
                                            (:color node-def))]
                    [forest (conj new-ids new-id)]))
                [forest #{}]
                (into {}
                      (mapcat (fn [id]
                                (map #(vector % {:parent id
                                                 :color (:color ghost)})
                                     (gen-new-poss forest poss-fn id)))
                              active-node-ids)))
        ]
    [(assoc ghost :active-node-ids new-ids) forest]))

(defn- eg-poss-fn [pos adj-poss]
  (take 2 (random-sample 0.6 adj-poss)))
