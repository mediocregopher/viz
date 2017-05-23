(ns quil-test.ghost
  (:require [quil-test.forest :as forest]
            [quil-test.grid   :as grid]))


(defn new-ghost [init-pos]
  (let [[forest init-id] (forest/add-node (forest/new-forest) init-pos)]
    { :grid (grid/add-point (grid/new-grid grid/euclidean) init-pos)
      :forest forest
      :active-node-ids #{init-id}
     }))

(defn- gen-new-poss [ghost id]
  "generates new positions branching from the given node"
  (->> id
       (#(forest/get-node (:forest ghost) %))
       (:pos)
       (#(grid/empty-adjacent-points (:grid ghost) %))
       (random-sample 0.75)
       ))

(defn- spawn-children [ghost id]
  (reduce (fn [[ghost new-ids] pos]
            (let [[forest new-id] (forest/spawn-child (:forest ghost) id pos)
                  grid (grid/add-point (:grid ghost) pos)]
              [(assoc ghost :forest forest :grid grid) (conj new-ids new-id)]))
          [ghost #{}]
          (gen-new-poss ghost id)))

(defn- spawn-children-multi [ghost ids]
  (reduce (fn [[ghost new-ids] id]
            (let [[ghost this-new-ids] (spawn-children ghost id)]
              [ghost (clojure.set/union new-ids this-new-ids)]))
          [ghost #{}]
          ids))

(defn incr [ghost]
  (let [[ghost new-ids] (spawn-children-multi ghost (:active-node-ids ghost))]
    (assoc ghost :active-node-ids new-ids)))

(-> (new-ghost [0 0])
    (incr)
    (incr)
    (incr)
    (:forest)
    (forest/lines)
    )
