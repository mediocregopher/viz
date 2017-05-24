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
                :tail-len 10
                :ghost (ghost/new-ghost grid/isometric [0 0])
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

(defn- ghost-incr [state]
  (assoc state :ghost
         (ghost/filter-active-nodes (ghost/incr (:ghost state))
                                    #(let [[minb maxb] (quil-bounds scale (* 2.5 scale))]
                                       (in-bounds? minb maxb (:pos %1))))))

(defn- ghost-expire-roots [state]
  (if-not (< (:tail-len state) (:frame state)) state
    (update-in state [:ghost] ghost/remove-roots)))

(defn- maybe-exit [state]
  (if (empty? (get-in state [:ghost :active-node-ids]))
    (if (zero? (:exit-wait-frames state)) (do
                                            ;(q/exit)
                                            state
                                            )
      (update-in state [:exit-wait-frames] dec))
    state))

(defn update-state [state]
  (-> state
      (update-in [:frame] inc)
      (ghost-incr)
      (ghost-expire-roots)
      (maybe-exit)))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0xFFFFFFFF)

  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (let [lines (forest/lines (get-in state [:ghost :forest]))
          leaves (forest/leaves (get-in state [:ghost :forest]))
          active (ghost/active-nodes (:ghost state))
          roots (forest/roots (get-in state [:ghost :forest]))
          ]

      (q/stroke 0xFF000000)
      (doseq [line lines]
        (apply q/line (map #(* scale %1) line)))

      (q/stroke 0xFF000000)
      (q/fill 0xFFFFFFFF)
      (doseq [leef leaves]
        (let [pos (:pos leef)]
          (q/ellipse (* scale (first pos)) (* scale (second pos)) 3 3)))

      (q/stroke 0xFF000000)
      (q/fill 0xFF000000)
      (doseq [active-node active]
        (let [pos (:pos active-node)]
          (q/ellipse (* scale (first pos)) (* scale (second pos)) 3 3)))
      ))

  (gil/save-animation "/tmp/quil.gif" 120 0)
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
