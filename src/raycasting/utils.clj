(ns raycasting.utils)
(defn vsum
  "element wise vector sum"
  [v1 v2]
  (mapv + v1 v2))