(ns quil-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-test.forest :as forest]
            [quil-test.grid :as grid]
            [quil-test.ghost :as ghost]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn setup []
  (q/frame-rate 15)
  (let [ghost (ghost/incr (ghost/new-ghost grid/isometric [0 0]))]
    {:ghost ghost
     }))

(def scale 5) ;; TODO make part of state?

; each bound is a position vector
(defn- in-bounds? [min-bound max-bound pos]
  (let [pos-k (keep-indexed #(let [mini (min-bound %1)
                                   maxi (max-bound %1)]
                               (when (and (>= %2 mini) (<= %2 maxi)) %2)) pos)]
    (= (count pos) (count pos-k))))

(defn- quil-bounds [scale buffer]
  (let [w (/ (- (/ (q/width) 2) buffer) scale)
        h (/ (- (/ (q/height) 2) buffer) scale)
        ]
    [[(- w) (- h)] [w h]]))

(defn update-state [state]
  (assoc state :ghost
         (ghost/filter-active-nodes (ghost/incr (:ghost state))
                                    #(let [[minb maxb] (quil-bounds scale (* 2.5 scale))]
                                       (in-bounds? minb maxb (:pos %1))))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0xFFFFFFFF)
  (q/stroke 0xFF000000)
  ;(q/fill 0xFFFF0000)

  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (let [ghost (if (:incr state) (:ghost-incr state) (:ghost state))]
      (doseq [line (forest/lines (:forest ghost))]
        (apply q/line (map #(* scale %1) line)))))

  ; Calculate x and y coordinates of the circle.
  ; (let [angle (:angle state)
  ;       x (* 150 (q/cos angle))
  ;       y (* 150 (q/sin angle))]
  ;   ; Move origin point to the center of the sketch.
  ;   (q/with-translation [(/ (q/width) 2)
  ;                        (/ (q/height) 2)]
  ;     ; Draw the circle.
  ;     (q/ellipse x y 100 100)))
  ; (q/line 0 0 (q/width) (q/height))
  )

(defn main []
  (q/defsketch quil-test
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(main)
