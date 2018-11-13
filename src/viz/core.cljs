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

(defn- positive [n] (if (> 0 n) (- n) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization

(defn- window-partial [k]
  (int (aget js/document "documentElement" k)))

(def window-size
  (let [w (int (min 1024 (window-partial "clientWidth")))]
    [w (int (min (* w 0.75) (window-partial "clientHeight")))]))

(def window-half-size (apply vector (map #(float (/ %1 2)) window-size)))

(def frame-rate 15)

(defn- mk-ghost [state ghost-def]
  (let [[forest id] (forest/add-node (:forest state) (:start-pos ghost-def))
        ghost       (ghost/add-active-node (ghost/new-ghost) id)
        ]
    [(assoc state :forest forest) (assoc ghost :ghost-def ghost-def)]))

(defn- mk-ghosts [state]
  (let [[state ghosts] (reduce (fn [[state ghosts] ghost-def]
                                    (let [[state ghost] (mk-ghost state ghost-def)]
                                      [state (cons ghost ghosts)]))
                               [state '()]
                               (:ghost-defs state))]
        (assoc state :ghosts ghosts)))

(defn- new-state []
  (-> {:frame-rate frame-rate
       :color-cycle-period 8
       :exit-wait-frames 40
       :tail-length 7
       :frame 0
       :grid-width 45 ; from the center
       :forest (forest/new-forest grid/isometric)
       :ghost-defs [;{:start-pos [2 2]
                    ; :color-fn (fn [state] (q/color 0 1 1))
                    ; ;:color-fn (fn [state]
                    ; ;            (q/color (mod (:frame state) (frames-per-color-cycle state)) 1 1))
                    ; }
                    {:start-pos [-2 -2]
                     :color-fn (fn [state]
                                 (let [frames-per-color-cycle
                                       (* (:color-cycle-period state) (:frame-rate state))]
                                   (q/color
                                     (/ (mod (:frame state) frames-per-color-cycle)
                                        frames-per-color-cycle)
                                     1 1)))
                     }
                    ]
       }
      (mk-ghosts)
      ))

(defn setup []
  (q/color-mode :hsb 1 1 1)
  (new-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scaling and unit conversion related

(defn- curr-second [state]
  (float (/ (:frame state) (:frame-rate state))))

(defn- grid-size [state]
  (let [h (int (* (window-size 1)
                  (float (/ (:grid-width state) (window-size 0)))))]
             [(:grid-width state) h]))

(defn- scale [state xy]
  (map-indexed #(* %2 (float (/ (window-half-size %1)
                                ((grid-size state) %1)))) xy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; poss-fn

(def bounds-buffer 1)

(defn- in-bounds? [grid-size pos]
  (let [[w h] (apply vector (map #(- % bounds-buffer) grid-size))
        min-bound [(- w) (- h)]
        max-bound [w h]
        pos-k (keep-indexed #(let [mini (min-bound %1)
                                   maxi (max-bound %1)]
                               (when (and (>= %2 mini) (<= %2 maxi)) %2)) pos)]
    (= (count pos) (count pos-k))))


(defn- dist-from-sqr [pos1 pos2]
  (reduce + (map #(* % %) (map - pos1 pos2))))

(defn- dist-from [pos1 pos2]
  (q/sqrt (dist-from-sqr pos1 pos2)))

(defn take-adj-poss [adj-poss]
  (if
    (or (empty? adj-poss)
          (< (rand) 0.15)) nil
    (take
      (max 1 (int (* (rand) (count adj-poss))))
      adj-poss)))

(defn- mk-poss-fn [state]
  (let [grid-size (grid-size state)]
    (fn [pos adj-poss]
      (->> adj-poss
           (filter #(in-bounds? grid-size %))
           (sort-by #(dist-from-sqr % [0 0]))
           (take-adj-poss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update

(defn- update-ghost-forest [state update-fn]
  (let [[ghosts forest]
        (reduce (fn [[ghosts forest] ghost]
                  (let [[ghost forest] (update-fn ghost forest)]
                    [(cons ghost ghosts) forest]))
                [nil (:forest state)]
                (:ghosts state))]
    (assoc state :ghosts (reverse ghosts) :forest forest)))

(defn- ghost-incr [state poss-fn]
  (update-ghost-forest state #(ghost/incr %1 %2 poss-fn)))

(defn rm-nodes [state node-ids]
  (update-ghost-forest state (fn [ghost forest]
                               [(reduce ghost/rm-active-node ghost node-ids)
                                (reduce forest/remove-node forest node-ids)])))

(defn- maybe-remove-roots [state]
  (if (>= (:tail-length state) (:frame state))
    state
    (rm-nodes state (map :id (forest/roots (:forest state))))))

(defn- ghost-set-color [state]
  (update-ghost-forest state (fn [ghost forest]
                               (let [color ((get-in ghost [:ghost-def :color-fn]) state)]
                                 [(assoc ghost :color color) forest]))))

(defn update-state [state]
  (let [poss-fn (mk-poss-fn state)]
    (-> state
        (ghost-set-color)
        (ghost-incr poss-fn)
        (maybe-remove-roots)
        (update-in [:frame] inc)
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw

(defn- draw-ellipse [pos size scale-fn] ; size is [w h]
  (let [scaled-pos (scale-fn pos)
        scaled-size (map int (scale-fn size))]
    (apply q/ellipse (concat scaled-pos scaled-size))))

(defn- in-line? [& nodes]
  (apply = (map #(apply map - %1)
                (partition 2 1 (map :pos nodes)))))

(defn- draw-node [node active? scale-fn]
  (let [pos (:pos node)
        stroke (get-in node [:meta :color])
        fill   (if active? stroke 0xFFFFFFFF)
        ]
    (q/stroke stroke)
    (q/fill fill)
    (draw-ellipse pos [0.30 0.30] scale-fn)))

(defn- draw-line [node parent scale-fn]
  (let [node-color (get-in node [:meta :color])
        parent-color (get-in node [:meta :color])
        color (q/lerp-color node-color parent-color 0.5)
        ]
    (q/stroke color)
    (q/stroke-weight 1)
    (apply q/line (map scale-fn (map :pos (list parent node))))))

(defn- draw-lines [forest parent node scale-fn]
  "Draws the lines of all children leading from the node, recursively"
  (let [children (map #(forest/get-node forest %) (:child-ids node))]

    (if-not parent
      (doseq [child children] (draw-lines forest node child scale-fn))
      (let [in-line-child (some #(if (in-line? parent node %) %) children)
            ]
        (doseq [child children]
          (if (and in-line-child (= in-line-child child))
            (draw-lines forest parent child scale-fn)
            (draw-lines forest node child scale-fn)))
        (when-not in-line-child
          (draw-line node parent scale-fn))
        ))

    ; we also take the opportunity to draw the leaves
    (when (empty? children)
      (draw-node node false scale-fn))
    ))

(defn draw-dial [state dial posL posR]
  (let [dial-norm (q/norm (:val dial) (:min dial) (:max dial))
        dial-pos (map #(q/lerp %1 %2 dial-norm) posL posR)]
    (q/stroke 0xFF000000)
    (q/stroke-weight 1)
    (q/fill   0xFF000000)
    (apply q/line (concat posL posR))
    (apply q/ellipse (concat dial-pos [5 5]))
    ))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0xFFFFFFFF)
  (q/with-translation [(/ (window-size 0) 2)
                       (/ (window-size 1) 2)]

    (let [scale-fn #(scale state %)
          ghost (:ghost state)
          forest (:forest state)
          roots (forest/roots forest)
          ]

      (doseq [root roots]
        (draw-lines forest nil root scale-fn))

      (doseq [ghost (:ghosts state)]
        (doseq [active-node (map #(forest/get-node forest %)
                                 (:active-node-ids ghost))]
          (draw-node active-node true scale-fn)))

      ))

    ;(draw-dial state (:dial state) [30 30] [100 30])

    ;(q/text (clojure.string/join
    ;          "\n"
    ;          (list
    ;            (gstring/format "frame:%d" (:frame state))
    ;            (gstring/format "second:%f" (curr-second state))
    ;            (gstring/format "spawn-chance:%d" (spawn-chance state))))
    ;        30 30)
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
