(ns quil-test.ghost
  (:require [quil-test.forest :as forest]
            [quil-test.grid   :as grid]))

(defn new-ghost [grid-def init-pos]
  (let [[forest init-id] (forest/add-node (forest/new-forest) init-pos)]
    { :grid (grid/add-point (grid/new-grid grid-def) init-pos)
      :forest forest
      :active-node-ids #{init-id}
     }))

(defn- pick-nodes [nodes]
  (take 2 (random-sample 0.6 nodes)))

(defn- gen-new-poss [ghost id]
  "generates new positions branching from the given node"
  (->> id
       (#(forest/get-node (:forest ghost) %))
       (:pos)
       (#(grid/empty-adjacent-points (:grid ghost) %))
       (pick-nodes)
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

(defn active-nodes [ghost]
  (map #(get-in ghost [:forest :nodes %]) (:active-node-ids ghost)))

(defn filter-active-nodes [ghost pred]
  (assoc ghost :active-node-ids
         (reduce #(if (pred %2) (conj %1 (:id %2)) %1) #{}
                 (active-nodes ghost))))

(defn remove-roots [ghost]
  (let [roots (forest/roots (:forest ghost))
        root-ids (map :id roots)
        root-poss (map :pos roots)
        ]
    (-> ghost
        (update-in [:active-node-ids] #(reduce disj %1 root-ids))
        (update-in [:forest] #(reduce forest/remove-node %1 root-ids))
        (update-in [:grid] #(reduce grid/rm-point %1 root-poss))
        )))


(-> (new-ghost grid/euclidean [0 0])
    (incr)
    (incr)
    (incr)
    (remove-roots)
    )
