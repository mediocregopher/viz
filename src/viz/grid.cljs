(ns viz.grid)

;; grid     set of points relative to a common origin

(def euclidean [       [0 -1]
                [-1 0]   ,    [1 0]
                       [0 1]       ])

(def isometric [[-1 -1] [0 -2] [1 -1]
                          ,
                [-1 1]  [0 2]  [1 1]])

(def hexagonal [        [0 -1]
                          ,
                [-1 1]         [1 1]])

(defn new-grid [grid-def]
  { :grid-def grid-def
    :points   #{} })

(defn- debug [& args]
  (.log js/console (clojure.string/join " " (map str args))))

(defn add-point [grid point]
  (update-in grid [:points] conj point))

(defn rm-point [grid point]
  (update-in grid [:points] disj point))

(defn adj-points [grid point]
  (map #(map + %1 point) (:grid-def grid)))
