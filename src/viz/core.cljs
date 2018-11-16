(ns viz.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [viz.forest :as forest]
            [viz.grid :as grid]
            [viz.ghost :as ghost]
            [viz.dial :as dial]
            [goog.string :as gstring]
            [goog.string.format]
            ))

(defn- debug [& args]
  (.log js/console (clojure.string/join " " (map str args))))
(defn- observe [v] (debug v) v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization

(defn- window-partial [k]
  (int (aget js/document "documentElement" k)))

(def window-size
  (let [w (int (min 1024 (window-partial "clientWidth")))]
    [w (int (min (* w 0.75) (window-partial "clientHeight")))]))

(def window-half-size (apply vector (map #(float (/ %1 2)) window-size)))

(defn- set-grid-size [state]
  (let [h (int (* (window-size 1)
                  (float (/ (:grid-width state) (window-size 0)))))]
    (assoc state :grid-size [(:grid-width state) h])))

(defn- add-ghost [state ghost-def]
  (let [forest (forest/add-node (:forest state)
                                (:start-pos ghost-def)
                                {:color ((:color-fn ghost-def) state)})
        ghost       (-> (ghost/new-ghost)
                        (ghost/add-active-node (:start-pos ghost-def))
                        (assoc :ghost-def ghost-def))
        ]
    (assoc state
           :forest forest
           :ghosts (cons ghost (:ghosts state)))))

(defn- new-state []
  (-> {:frame-rate 30
       :color-cycle-period 3
       :background-color 0xFFFFFFFF
       :tail-length 7
       :frame 0
       :grid-width 100 ; from the center
       :forest (forest/new-forest grid/isometric)
       }
      (set-grid-size)
      (add-ghost {:start-pos [-1 -1]
                  :color-fn (fn [state]
                              (let [frames-per-color-cycle
                                    (* (:color-cycle-period state) (:frame-rate state))]
                                (q/color
                                  (/ (mod (:frame state) frames-per-color-cycle)
                                     frames-per-color-cycle)
                                  1 1)))
                  })
      ))

(defn setup []
  (let [state (new-state)]
    (q/smooth 0)
    (q/color-mode :hsb 1 1 1)
    (q/frame-rate (:frame-rate state))
    (q/background (:background-color state))
    state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scaling and unit conversion related

(defn- curr-second [state]
  (float (/ (:frame state) (:frame-rate state))))

(defn- scale [grid-size xy]
  (map-indexed #(* %2 (float (/ (window-half-size %1)
                                (grid-size %1)))) xy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; poss-fn

(def bounds-buffer 1)

(defn- in-bounds? [grid-size pos]
  (let [[w h] (apply vector (map #(- % bounds-buffer) grid-size))]
    (every?
      #(and (>= (% 1) (- (% 0))) (<= (% 1) (% 0)))
      (map vector [w h] pos))))

(defn- dist-from-sqr [pos1 pos2]
  (reduce + (map #(* % %) (map - pos1 pos2))))

(defn- dist-from [pos1 pos2]
  (q/sqrt (dist-from-sqr pos1 pos2)))

(defn take-adj-poss [grid-width pos adj-poss]
  (let [dist-from-center-sqr (dist-from-sqr [0 0] pos)
        width-sqr (* grid-width grid-width)
        dist-ratio (/ (- width-sqr dist-from-center-sqr) width-sqr)
        ]
    (take
      (int (* (q/map-range (rand) 0 1 0.1 1)
              dist-ratio
              (count adj-poss)))
      adj-poss)))

(defn- mk-poss-fn [state]
  (let [grid-size (:grid-size state)]
    (fn [pos adj-poss]
      (->> adj-poss
           (filter #(in-bounds? grid-size %))
           (shuffle)
           ;(sort-by #(dist-from-sqr % [0 0]))
           (take-adj-poss (grid-size 0) pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update

(defn color-fn [adj-nodes]
  (q/color (+ (q/map-range (rand) 0 1 -0.1 0.1)
              (/ (reduce + (map #(get-in [:color 0] %) adj-nodes))
                 (count adj-nodes)))
           1 1))

(defn- update-ghost-forest [state update-fn]
  (let [[ghosts forest]
        (reduce (fn [[ghosts forest] ghost]
                  (let [[ghost forest] (update-fn ghost forest)]
                    [(cons ghost ghosts) forest]))
                [nil (:forest state)]
                (:ghosts state))]
    (assoc state :ghosts (reverse ghosts) :forest forest)))

(defn- ghost-incr [state]
  (let [poss-fn (mk-poss-fn state)
        state (update-ghost-forest state #(ghost/incr %1 %2 poss-fn))
        forest (:forest state)
        new-nodes (reduce #(assoc %1 %2 (forest/get-node forest %2)) {}
                          (mapcat :active-node-poss (:ghosts state)))]
    (assoc-in state [:delta-state :add-nodes] new-nodes)))

(defn- rm-old-tails [state]
  (if (>= (:tail-length state) (:frame state)) state
    (let [nodes (forest/roots (:forest state))]
      (-> state
          (update-in [:forest] #(reduce forest/remove-node % (keys nodes)))
          (assoc-in [:delta-state :rm-nodes] nodes)))))

(defn- ghost-set-color [state]
  (update-ghost-forest state (fn [ghost forest]
                               (let [color ((get-in ghost [:ghost-def :color-fn]) state)]
                                 [(assoc ghost :color color) forest]))))

(defn update-state [state]
  (-> state
      (assoc :delta-state {})
      (ghost-set-color)
      (rm-old-tails)
      (ghost-incr)
      (update-in [:frame] inc)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw

(defn- draw-ellipse [pos size scale-fn] ; size is [w h]
  (let [scaled-pos (scale-fn pos)
        scaled-size (map int (scale-fn size))]
    (apply q/ellipse (concat scaled-pos scaled-size))))

(defn- draw-node [pos node scale-fn]
  (let [stroke (get-in node [:color])]
    (q/stroke stroke)
    (q/fill stroke)
    (draw-ellipse pos [1 1] scale-fn)))

(defn- clear-node [pos bg-color scale-fn]
  (q/stroke bg-color)
  (q/fill bg-color)
  (draw-ellipse pos [1.5 1.5] scale-fn))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/with-translation window-half-size

    (let [grid-size (:grid-size state)
          scale-fn #(scale grid-size %)
          ]

      (doseq [[pos _] (get-in state [:delta-state :rm-nodes])]
        (clear-node pos (:background-color state) scale-fn))

      (doseq [[pos node] (get-in state [:delta-state :add-nodes])]
        (draw-node pos node scale-fn))

      ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def

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
