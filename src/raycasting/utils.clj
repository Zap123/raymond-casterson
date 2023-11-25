(ns raycasting.utils)
(defn vsum
  "element wise vector sum"
  [v1 v2]
  (mapv + v1 v2))

(defn is-wall? [position world-map]
  (let [tile (get-in world-map [(int (second position)) (int (first position))])]
    (not (zero? tile))))

(defn get-tile [position world-map] 
  (get-in world-map [(int (second position)) (int (first position))]))