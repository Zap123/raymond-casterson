(ns raycasting.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [raycasting.ray-caster :refer [make-ray-caster]]
            [raycasting.utils :refer [vsum, is-wall?]]
            [raycasting.world :refer [world-map]]
            [clojure.core.matrix :refer [mmul mul]]))
(defn setup []
  (q/frame-rate 60)
  {:player-position [15 15]
   :playerAngle (* 0.5 (Math/PI)) ;unused
   :fov 2 ;unused
   :direction [-1 0]
   :plane [0 0.66] ;  2 * atan(0.66/1.0) = 66 deg POV
   :worldMap world-map
   :timestamp 0
   :old-timestamp 0})

;; => Syntax error compiling at (core.clj:10:1).
;;    Unable to resolve symbol: defn in this context


(defn update-state [state]
  ; Update sketch state.
  (let [{:keys [timestamp]} state]
    (assoc state
           :old-timestamp timestamp
           :timestamp (System/currentTimeMillis))))

(defn render-wall
  "Render the wall at the center of the screen based on the distance computed in the ray-cast 
   function. Assign the wall height proportionally to the inverse of the distance."
  [distance-to-wall x wall-type side*]

  (let [wall-height (if (zero? distance-to-wall) (q/height) (int (/ (q/height) distance-to-wall)))
        line-bottom (+ (/ (- wall-height) 2) (/ (q/height) 2)) ; set the plane at the middle of the screen
        line-top (+ (/ wall-height 2) (/ (q/height) 2))
        draw-start (if (< line-bottom 0) 0 line-bottom)
        draw-end (if (> line-top (q/height)) (q/height) line-top)
        shading (if (= :side side*) 0.5 1)
        color (* (- 255 (/ 255 wall-type)) shading)]

    ; assign a color to the wall based on the wall type
    (q/stroke color color color)
    (q/line [x draw-start] [x draw-end])))

(defn draw-state
  "drawing loop"
  [state]
  ; set default background color 
  (q/background 219 31 72)

  ; draw floor 
  (q/fill 158 102 100)
  (q/rect 0 (/ (q/height) 2) (q/width) (/ (q/height) 2))
  ; compute ray casting and draw walls
  (let [{:keys [player-position direction worldMap plane]} state]
    (doseq [x (range (q/width))]
      (let [[distance-to-wall wall-type side*] (make-ray-caster x worldMap direction player-position plane (q/width))]
        (render-wall distance-to-wall x wall-type side*)))))

(defn safe-movement
  "Check if the next position is a wall. If it is, return the current position"
  [player-position delta-movement worldMap]

  (let [newPosition (vsum player-position delta-movement)]
    (if (is-wall? newPosition worldMap)
      player-position
      newPosition)))

(defn key-press
  "Handle player movements"
  [state e]
  (let [{:keys [player-position plane direction]} state
        key (e :key)
        frame-time (/ (- (:timestamp state) (:old-timestamp state)) 1000.0)
        ; determine direction based on angle for forward/backward
        rotation-matrix #(let [c (Math/cos %)
                               s (Math/sin %)]
                           [[c (- s)]
                            [s c]])

        rotationSpeed (* frame-time 3.0)
        movementSpeed (* frame-time 3.0)

        ; compute the update vector based on the key pressed
        update-vector (cond
                        (contains? #{:up :w} key) [(mul direction movementSpeed) direction plane]
                        (contains? #{:down :s} key) [(mul direction (- movementSpeed)) direction plane]
                        (contains? #{:left :a} key) [[0  0] (mmul (rotation-matrix  rotationSpeed) direction)  (mmul (rotation-matrix  rotationSpeed) plane)]
                        (contains? #{:right :d} key) [[0  0] (mmul (rotation-matrix  (- rotationSpeed)) direction) (mmul (rotation-matrix  (- rotationSpeed)) plane)]
                        :else [[0 0] direction plane])]
    ; update position
    (assoc state
           :player-position (safe-movement player-position (first update-vector) world-map)
           :direction (second update-vector)
           :plane (last update-vector))))

(q/defsketch raymond-casterson
  :title "Raymond Casterson"
  :size [640 480]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed key-press
  :middleware [m/fun-mode])

(defn -main [& args])