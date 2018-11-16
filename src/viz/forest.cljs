(ns viz.forest
  (:require [viz.grid :as grid])
  )

(defn new-forest [grid-def]
  {:grid (grid/new-grid grid-def)
   :nodes {}
   :roots #{}
   :leaves #{}})

(defn- unset-parent [forest pos parent-pos]
  (-> forest
      (update-in [:nodes pos] dissoc :parent)
      (update-in [:nodes parent-pos :children] disj pos)
      (update-in [:roots] conj pos)
      (#(if (empty? (get-in % [:nodes parent-pos :children]))
          (update-in % [:leaves] conj parent-pos) %))
      ))

(defn- set-parent [forest pos parent-pos]
  (let [prev-parent-pos (get-in forest [:nodes pos :parent])]
    (-> forest
        (assoc-in [:nodes pos :parent] parent-pos)
        (update-in [:nodes parent-pos :children] #(if %1 (conj %1 pos) #{pos}))
        (update-in [:roots] disj pos)
        (update-in [:leaves] disj parent-pos)
        ;; If there was a previous parent of the child, unset that shit
        (#(if prev-parent-pos (unset-parent %1 pos prev-parent-pos) %1))
        )))

(defn empty-adj-points [forest pos]
  (grid/empty-adj-points (:grid forest) pos))

(defn add-node [forest pos node]
  (-> forest
      (update-in [:grid] grid/add-point pos)
      (assoc-in [:nodes pos] node)
      (update-in [:roots] conj pos)
      (update-in [:leaves] conj pos)
      ))

(defn remove-node [forest pos]
  (let [node      (get-in forest [:nodes pos])
        children (:children node)
        parent-pos (:parent node)]
    (-> forest
        (update-in [:grid] grid/rm-point pos)
        ;; unset this node's parent, if it has one
        (#(if parent-pos (unset-parent %1 pos parent-pos) %1))
        ;; unset this node's children, if it has any
        ((fn [forest] (reduce #(unset-parent %1 %2 pos) forest children)))
        ;; remove from all top-level sets
        (update-in [:nodes] dissoc pos)
        (update-in [:roots] disj pos)
        (update-in [:leaves] disj pos)
        )))

(defn get-node [forest pos]
  (get-in forest [:nodes pos]))

(defn spawn-child [forest parent-pos pos node]
  (-> forest
      (add-node pos node)
      (set-parent pos parent-pos)))

(defn roots [forest] (-> forest :nodes (select-keys (:roots forest))))
(defn root? [node] (not (boolean (:parent node))))

(defn leaves [forest] (-> forest :nodes (select-keys (:leaves forest))))
(defn leaf? [node] (empty? (:children node)))
