(ns quil-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-test.forest :as forest]
            [quil-test.grid :as grid]
            [quil-test.ghost :as ghost]
            [gil.core :as gil]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def window-size [1000 1000])

(defn- new-state []
  {:frame-rate 30
   :exit-wait-frames 15
   :tail-length 20
   :frame 0
   :gif-seconds 0
   :grid-size [50 50] ; width/height from center
   :poss-fn (fn [pos adj-poss]
              (if (zero? (rand-int 30))
                (take 2 (shuffle adj-poss))
                (take 1 (shuffle adj-poss))))
   :ghost (-> (ghost/new-ghost grid/isometric)
              (ghost/new-active-node [0 0])
              )
   })


(defn setup []
  (let [state (new-state)]
    (q/frame-rate (:frame-rate state))
    state))

(defn- scale [state xy]
  (map-indexed #(* %2 (/ (/ (window-size %1) 2)
                         (get-in state [:grid-size %1]) )) xy))

; each bound is a position vector
(defn- in-bounds? [min-bound max-bound pos]
  (let [pos-k (keep-indexed #(let [mini (min-bound %1)
                                   maxi (max-bound %1)]
                               (when (and (>= %2 mini) (<= %2 maxi)) %2)) pos)]
    (= (count pos) (count pos-k))))

(defn- quil-bounds [state buffer]
  (let [[w h] (apply vector (map #(- % buffer) (:grid-size state)))]
    [[(- w) (- h)] [w h]]))

(defn- ghost-incr [state]
  (assoc state :ghost
         (ghost/filter-active-nodes (ghost/incr (:ghost state) (:poss-fn state))
                                    #(let [[minb maxb] (quil-bounds state 2)]
                                       (in-bounds? minb maxb (:pos %1))))))

(defn- ghost-expire-roots [state]
  (if-not (< (:tail-length state) (:frame state)) state
    (update-in state [:ghost] ghost/remove-roots)))

(defn- maybe-exit [state]
  (if (empty? (get-in state [:ghost :active-node-ids]))
    (if (zero? (:exit-wait-frames state)) (do
                                            (q/exit)
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

(defn- ellipse [state pos size] ; size is [w h]
  (let [scaled-pos (scale state pos)
        scaled-size (map int (scale state size))]
    (apply q/ellipse (concat scaled-pos scaled-size))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0xFFFFFFFF)
  (q/with-translation [(/ (window-size 0) 2)
                       (/ (window-size 1) 2)]
    (let [lines (forest/lines (get-in state [:ghost :forest]))
          leaves (forest/leaves (get-in state [:ghost :forest]))
          active (ghost/active-nodes (:ghost state))
          roots (forest/roots (get-in state [:ghost :forest]))
          ]

      (q/stroke 0xFF000000)
      (doseq [line lines]
        (apply q/line (apply concat (map #(scale state %) line))))

      (q/stroke 0xFF000000)
      (q/fill 0xFFFFFFFF)
      (doseq [leef leaves]
        (let [pos (:pos leef)]
          (ellipse state pos [0.5 0.5])
          ))

      (q/stroke 0xFF000000)
      (q/fill 0xFF000000)
      (doseq [active-node active]
        (let [pos (:pos active-node)]
          (ellipse state pos [0.5 0.5])
          ))
      ))

    (when-not (zero? (:gif-seconds state))
      (let [anim-frames (* (:gif-seconds state) (:frame-rate state))]
        (gil/save-animation "quil.gif" anim-frames 0)
        (when (> (:frame state) anim-frames) (q/exit))))
  )

(defn main []
  (q/defsketch quil-test
    :title ""
    :size window-size
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
