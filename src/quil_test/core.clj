(ns quil-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-test.forest :as forest]
            [quil-test.grid :as grid]
            [quil-test.ghost :as ghost]
            [gil.core :as gil]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn setup []
  (let [state { :frame-rate 15
                :exit-wait-frames 15
                :frame 0
                :ghost (ghost/new-ghost grid/euclidean [0 0])
                }]
    (q/frame-rate (:frame-rate state))
    state))

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

(defn- incr-ghost [state]
  (assoc state :ghost
         (ghost/filter-active-nodes (ghost/incr (:ghost state))
                                    #(let [[minb maxb] (quil-bounds scale (* 2.5 scale))]
                                       (in-bounds? minb maxb (:pos %1))))))

(defn- maybe-exit [state]
  (if (empty? (get-in state [:ghost :active-node-ids]))
    (if (zero? (:exit-wait-frames state)) (do (q/exit) state)
      (update-in state [:exit-wait-frames] dec))
    state))

(defn update-state [state]
  (-> state
      (update-in [:frame] inc)
      (incr-ghost)
      (maybe-exit)))

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

  (gil/save-animation "/tmp/quil.gif" 150 0)
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
