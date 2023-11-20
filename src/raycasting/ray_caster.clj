(ns raycasting.ray-caster)

(defn cast-ray
  "Cast a ray to determine wall position (non-zero element)
   returns a vector with the distance to the wall and the wall type"
  [world-map player angle-vector distance-to-wall step-size ray-angle]
  (let [ray-vector [(+ (first player) (* (first angle-vector) distance-to-wall)) (+ (second player) (* (second angle-vector) distance-to-wall))]
        ray-position (get-in world-map [(int (first ray-vector)) (int (second ray-vector))])]

    (if (> ray-position 0)
      [distance-to-wall ray-position]
      (recur world-map player angle-vector (+ distance-to-wall step-size) step-size ray-angle))))

(defn make-ray-caster
  "computes angle and cast a ray for the current position. it returns the distance from the first wall"
  [x fov world-map player playerAngle width]
  (let [; compute ray angle based on the position, player angle and fov
        ; it stretches the angle to the width and then reposition with playerAngle
        ray-angle (+ (- playerAngle (/ 2 fov))  (* (/ x width) fov))
        angle-vector [(Math/cos ray-angle) (Math/sin ray-angle)]
        step-size 0.1]
    ;(println ray-angle angle-vector playerAngle)
    ; cast a ray and advance by step-size. start with a non zero value otherwise 
    ; it zeros everything and won't advance
    (cast-ray world-map player angle-vector step-size step-size ray-angle)))

