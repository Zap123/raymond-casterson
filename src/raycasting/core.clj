(ns raycasting.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [raycasting.ray-caster :refer [make-ray-caster]]
            [raycasting.utils :refer [vsum]]
            [raycasting.world :refer [world-map]]))

(defn setup []
  (q/frame-rate 60)
  {:playerPos [15 15]
   :playerAngle (* 0.5 (Math/PI))
   :fov 2
   :plane [0 0.66] ;  2 * atan(0.66/1.0) = 66 deg POV
   :worldMap world-map})
;; => Syntax error compiling at (core.clj:10:1).
;;    Unable to resolve symbol: defn in this context


(defn update-state [state]
  ; Update sketch state.
  state)


(defn draw-minimap
  "Draw the 2D map"
  [state]
  (let [{:keys [worldMap playerPos playerAngle fov]} state
        rows (count worldMap)
        cols (count (first worldMap))
        ray-angle (+ (- playerAngle (/ 2 fov))  (* (q/width) fov))
        ray-angle-end (- playerAngle fov)]
    (q/stroke 0)
    (q/rect (first playerPos) (second playerPos) 1 1)
    (q/stroke 255 255 255)
    ; direction line
    (q/line playerPos  (vsum playerPos [(* 10 (Math/cos ray-angle)) (* 10 (Math/sin ray-angle))]))
    (q/line playerPos  (vsum playerPos [(* 10 (Math/cos ray-angle-end)) (* 10 (Math/sin ray-angle-end))]))

    (q/stroke 0)

    (doseq [i (range rows)
            j (range cols)]
      (let [element (get-in worldMap [i j]), color (* 100 element), blockWidth  1, blockHeight 1]
        (q/fill color color color)
        (when (> element 0)
          (q/rect (* j blockWidth) (* i blockHeight) blockWidth blockHeight))))))



(defn render-wall
  "Render the wall at the center of the screen based on the distance computed in the ray-cast 
   function. Assign the wall height proportionally to the inverse of the distance."
  [distance-to-wall x wall-type]

  (let [wall-height (/ (q/height) distance-to-wall)
        line-bottom (+ (/ (- wall-height) 2) (/ (q/height) 2)) ; set the plane at the middle of the screen
        line-top (+ (/ wall-height 2) (/ (q/height) 2))
        draw-start (if (< line-bottom 0) 0 line-bottom)
        draw-end (if (> line-top (q/height)) (q/height) line-top)]
    (q/stroke (- 255 (/ 255 wall-type)) (- 255 (/ 255 wall-type)) (- 255 (/ 255 wall-type)))
    (q/line [x draw-start] [x draw-end])))

(defn draw-state
  "drawing loop"
  [state]
  ; set default background color 
  (q/background 219 31 72)

  ; draw floor 
  (q/fill 158 102 100)
  (q/rect 0 (/ (q/height) 2) (q/width) (/ (q/height) 2)  )
  ; compute ray casting and draw walls
  (let [{:keys [fov playerPos playerAngle worldMap]} state]
    (doseq [x (range (q/width))]
      (let [[distance-to-wall wall-type] (make-ray-caster x fov worldMap playerPos playerAngle (q/width))]
        (render-wall distance-to-wall x wall-type))))
  (draw-minimap state))

(defn safe-movement
  "Check if the next position is a wall. If it is, return the current position"
  [playerPos updateVector worldMap]
  (let [newPosition (vsum playerPos (first updateVector))]
    (if (> (get-in worldMap [(int (second newPosition)) (int (first newPosition))]) 0)
      playerPos
      newPosition)))

(defn key-press
  "Handle player movements"
  [state e]
  (let [{:keys [playerPos playerAngle]} state
        key (e :key)
        ; determine direction based on angle for forward/backward
        rotation [(Math/cos playerAngle) (Math/sin playerAngle)]
        rotationSpeed (/ 2 (/  180 (Math/PI)))
        updateVector (cond (contains? #{:up :w} key) [rotation 0]
                           (contains? #{:down :s} key) [(mapv *  rotation [-1 -1]) 0]
                           (contains? #{:left :a} key) [[0  0] (- rotationSpeed)]
                           (contains? #{:right :d} key) [[0  0] rotationSpeed]
                           :else [[0 0] 0])]
    ; update position
    (assoc state
           :playerPos (safe-movement playerPos updateVector (:worldMap state))
           :playerAngle (mod (+ playerAngle (second updateVector)) (* 2 (Math/PI))))))

(q/defsketch raymond-casterson
  :title "Raymond Casterson"
  :size [1200 600]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed key-press
  :middleware [m/fun-mode])
