(ns viz.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [viz.forest :as forest]
            [viz.grid :as grid]
            [viz.ghost :as ghost]
            [goog.string :as gstring]
            [goog.string.format]
            ;[gil.core :as gil]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def window-size [800 800])
(def window-half-size (apply vector (map #(float (/ %1 2)) window-size)))

(defn- new-state []
  {:frame-rate 30
   :exit-wait-frames 40
   :tail-length 40
   :frame 0
   :gif-seconds 0
   :grid-size [50 50] ; width/height from center
   :ghost (-> (ghost/new-ghost grid/euclidean)
              (ghost/new-active-node [0 0])
              )
   })

(defn- curr-second [state]
  (float (/ (:frame state) (:frame-rate state))))

(defn- positive [n] (if (> 0 n) (- n) n))

(defn- spawn-chance [state]
  (let [period-seconds 1
        period-frames (* (:frame-rate state) period-seconds)]
  (if (zero? (rem (:frame state) period-frames))
    1 100)
    ))

  ;(let [period-seconds 1
  ;      rad-per-second (float (/ (/ Math/PI 2) period-seconds))
  ;      rad (* rad-per-second (curr-second state))
  ;      chance-raw (positive (q/sin rad))
  ;      ]
  ;    (if (> chance-raw 0.97) 3 50)
  ;  ))


(defn- mk-poss-fn [state]
  (let [chance (spawn-chance state)]
    (fn [pos adj-poss]
      (if (zero? (rand-int chance))
        adj-poss
        (take 1 (shuffle adj-poss))))
    ))

(defn setup []
  (let [state (new-state)]
    (q/frame-rate (:frame-rate state))
    state))

(defn- scale [state xy]
  (map-indexed #(* %2 (float (/ (window-half-size %1)
                                (get-in state [:grid-size %1])))) xy))

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
         (ghost/filter-active-nodes (ghost/incr (:ghost state) (mk-poss-fn state))
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
      (ghost-incr)
      (ghost-expire-roots)
      (update-in [:frame] inc)
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

    ;(when-not (zero? (:gif-seconds state))
    ;  (let [anim-frames (* (:gif-seconds state) (:frame-rate state))]
    ;    (gil/save-animation "quil.gif" anim-frames 0)
    ;    (when (> (:frame state) anim-frames) (q/exit))))

    ;(q/text (clojure.string/join
    ;          "\n"
    ;          (list
    ;            (gstring/format "frame:%d" (:frame state))
    ;            (gstring/format "second:%f" (curr-second state))
    ;            (gstring/format "spawn-chance:%d" (spawn-chance state))))
    ;        30 30)
  )

(q/defsketch viz
  :title ""
  :host "viz"
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
  :middleware [m/fun-mode])
