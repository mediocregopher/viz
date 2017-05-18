(ns quil-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-test.grid :as grid]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  ; (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0xFFFF0000
   :angle 0})

(defn update-state [state] state)

;(defn update-state [state]
;  ; Update sketch state by changing circle color and position.
;  {:color (mod (+ (:color state) 0.7) 255)
;   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0xFF000000)
  ; Set circle color.
  (q/stroke 0xFFFF0000)
  (q/fill 0xFFFF0000)

  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    ; (q/ellipse 0 0 200 200)
    (doseq [line-obj (tree-to-lines mytree 0 0 0)]
      (apply q/line (map #(* 50 %1) (:line line-obj)))))

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
    ; :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(main)
