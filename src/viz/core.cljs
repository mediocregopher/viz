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

(defn- debug [& args]
  (.log js/console (clojure.string/join " " (map str args))))

(defn- positive [n] (if (> 0 n) (- n) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization

(defn- window-partial [k]
  (int (* (aget js/document "documentElement" k) 0.95)))

(def window-size [ (min 1025 (window-partial "clientWidth"))
                   (int (* (window-partial "clientHeight") 0.75))
                   ])
(def window-half-size (apply vector (map #(float (/ %1 2)) window-size)))

(defn- new-state []
  {:frame-rate 15
   :exit-wait-frames 40
   :tail-length 7
   :frame 0
   :dial 0
   :gif-seconds 0
   :grid-width 35 ; from the center
   :ghost (-> (ghost/new-ghost grid/isometric)
              (ghost/new-active-node [0 0])
              )
   })

(defn setup []
  (let [state (new-state)]
    (q/frame-rate (:frame-rate state))
    state))

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

(def bounds-buffer 1)

(defn- in-bounds? [state pos]
  (let [[w h] (apply vector (map #(- % bounds-buffer) (grid-size state)))
        min-bound [(- w) (- h)]
        max-bound [w h]
        pos-k (keep-indexed #(let [mini (min-bound %1)
                                   maxi (max-bound %1)]
                               (when (and (>= %2 mini) (<= %2 maxi)) %2)) pos)]
    (= (count pos) (count pos-k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dials

(def dial-period-seconds 2)
(def dial-offset-seconds 0.5)
(def dial-width-frames 1)

(defn- set-dial [state]
  (let [period-frames (* (:frame-rate state) dial-period-seconds)
        frame-in-second (rem (:frame state) (int period-frames))
        offset-frame (int (* (:frame-rate state) dial-offset-seconds))]
    (assoc state :dial
           (if (or (and (>= frame-in-second 0) (< frame-in-second dial-width-frames))
                   (and (>= frame-in-second offset-frame) (< frame-in-second (+ dial-width-frames offset-frame))))
             1
             0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; poss-fn

  ;(let [period-seconds 1
  ;      rad-per-second (float (/ (/ Math/PI 2) period-seconds))
  ;      rad (* rad-per-second (curr-second state))
  ;      chance-raw (positive (q/sin rad))
  ;      ]
  ;    (if (> chance-raw 0.97) 3 50)
  ;  ))

(defn- dist-from [pos1 pos2]
  (reduce + (map #(* % %) (map - pos1 pos2))))

(def mk-poss-fns {
                  :random (fn [state]
                            (fn [pos adj-poss]
                              (take 2 (random-sample 0.6 adj-poss))))

                  :centered (fn [state]
                              (let [spawn-all? (= 1 (:dial state))]
                                (fn [pos adj-poss]
                                  (let [to-take (if spawn-all? (count adj-poss) 1)]
                                    (take to-take (sort-by #(dist-from % [0 0]) adj-poss))
                                    ))))
               })

(defn- mk-poss-fn [state]
  (let [chosen-poss-fn :centered
        inner-fn ((chosen-poss-fn mk-poss-fns) state)]
    (fn [pos adj-poss] (inner-fn pos (filter #(in-bounds? state %) adj-poss)))))

;; ghost

(defn- ghost-incr [state poss-fn]
  (update-in state [:ghost] ghost/incr poss-fn))

(defn- maybe-remove-roots [state]
  (if-not (< (:tail-length state) (:frame state)) state
    (update-in state [:ghost] ghost/remove-roots)))

(defn- ghost-update-node-meta [state id f]
  (update-in state [:ghost :forest] forest/update-node-meta id f))

(defn- ghost-set-nodes-color [state]
  (let [color (if (zero? (:dial state)) 0xFF000000 0xFFFF0000)]
    (reduce #(ghost-update-node-meta %1 %2 (fn [ma] (assoc m :color color)))
            state (get-in state [:ghost :active-node-ids]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update

(defn- maybe-exit [state]
  (if (empty? (get-in state [:ghost :active-node-ids]))
    (if (zero? (:exit-wait-frames state)) (new-state)
      (update-in state [:exit-wait-frames] dec))
    state))

(defn update-state [state]
  (let [poss-fn (mk-poss-fn state)]
    (-> state
        (set-dial)
        (ghost-incr poss-fn)
        (ghost-set-nodes-color)
        (maybe-remove-roots)
        (update-in [:frame] inc)
        (maybe-exit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw

(defn- draw-ellipse [state pos size] ; size is [w h]
  (let [scaled-pos (scale state pos)
        scaled-size (map int (scale state size))]
    (apply q/ellipse (concat scaled-pos scaled-size))))

(defn- in-line? [& nodes]
  (apply = (map #(apply map - %1)
                (partition 2 1 (map :pos nodes)))))

(defn- draw-node [state node active?]
  (let [pos (:pos node)
        stroke (-> node :meta :color)
        fill   (if active? stroke 0xFFFFFFFF)
        size   (if active? 0.35 0.3)]
    (q/stroke stroke)
    (q/fill fill)
    (draw-ellipse state pos [size size])))

(defn- draw-line [state node parent]
  ; TODO take the averate of the colors
  (let [color (-> node :meta :color)]
    (q/stroke color)
    (q/fill color)
    (apply q/line (apply concat
                         (map #(scale state %)
                              (map :pos (list parent node)))))))

(defn draw-lines [state forest parent node]
  "Draws the lines of all children leading from the node, recursively"
  (let [children (map #(forest/get-node forest %) (:child-ids node))]

    (if-not parent
      (doseq [child children] (draw-lines state forest node child))
      (let [in-line-child (some #(if (in-line? parent node %) %) children)
            ]
        (doseq [child children]
          (if (and in-line-child (= in-line-child child))
            (draw-lines state forest parent child)
            (draw-lines state forest node child)))
        (when-not in-line-child
          (draw-line state node parent))
        ))

    ; we also take the opportunity to draw the leaves
    (when (empty? children)
      (draw-node state node false))

    ))

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
      (doseq [root roots]
        (draw-lines state (get-in state [:ghost :forest]) nil root))

      (q/stroke 0xFF000000)
      (q/fill 0xFF000000)
      (doseq [active-node active]
        (draw-node state active-node true))
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
