(ns raycasting.ray-caster
  (:require [raycasting.utils :refer [is-wall? get-tile]]))

(defn dda-step!
  "Advance the ray in the map by one step and return new coordinates.
   Returns a vector with the new coordinates, the distance to the next side and the side type"
  [side-distances* delta-distances map-coordinates* step side]

  (let [[side-dist-x side-dist-y] side-distances* 
        [delta-dist-x delta-dist-y] delta-distances
        [map-x map-y] map-coordinates* 
        [step-x step-y] step]
    (if (< @side-dist-x @side-dist-y)
      (do
        (swap! side-dist-x #(+ % delta-dist-x))
        (swap! map-x #(+ % step-x))
        (reset! side :front))
      (do
        (swap! side-dist-y #(+ % delta-dist-y))
        (swap! map-y #(+ % step-y))
        (reset! side :side)))))

(defn projected-distance
  "Compute the distance projected on camera plane"
  [side-distances delta-distances side]
  (if (= :front side)
    (- (first side-distances) (first delta-distances))
    (- (second side-distances) (second delta-distances))))

(defn cast-ray
  "Cast a ray to determine wall position (non-zero element)
   returns a vector with the distance to the wall and the wall type"
  [world-map side-distances* delta-distances step map-coordinates*]
  (let [side* (atom :front)]  (loop []
                               ; jump by shorter direction  
                               (dda-step! side-distances* delta-distances map-coordinates* step side*)
                               ; check if the ray has hit a wall
                               (if (is-wall? (map deref map-coordinates*) world-map)
                                 [(projected-distance (map deref side-distances*) delta-distances @side*) (get-tile (map deref map-coordinates*) world-map) @side*]
                                 (recur)))))

(defn init-step-variables
  "Initialize the step variables for the ray casting algorithm"
  [ray-direction position map-position]
  (let
   [[map-x map-y] map-position
    [pos-x pos-y] position
    [ray-direction-x ray-direction-y] ray-direction
    step-x (if (neg? ray-direction-x) -1  1)
    step-y (if (neg? ray-direction-y) -1  1)
    delta-distance-x (if (zero? ray-direction-x) 1e30 (abs (/ 1 ray-direction-x)))
    delta-distance-y (if (zero? ray-direction-y) 1e30 (abs (/ 1 ray-direction-y)))

    side-distance-x (if (neg? ray-direction-x)
                      (* (- pos-x map-x) delta-distance-x)
                      (* (- (+ map-x 1) pos-x) delta-distance-x))
    side-distance-y (if (neg? ray-direction-y)
                      (* (- pos-y map-y) delta-distance-y)
                      (* (- (+ map-y 1) pos-y) delta-distance-y))]

    [[step-x step-y] (map atom [side-distance-x side-distance-y]) [delta-distance-x delta-distance-y]]))


(defn make-ray-caster
  "computes angle and cast a ray for the current position. it returns the distance from the first wall"
  [x world-map direction position plane width]
  (let [camera-x (- (* 2 (/ x width)) 1) ; x-coordinate in camera space
        [direction-x direction-y] direction
        [plane-x plane-y] plane
        ray-direction-x (+ direction-x (* plane-x camera-x))
        ray-direction-y (+ direction-y (* plane-y camera-x))
        map-coordinates [(atom (int (first position))) (atom (int (second position)))]
        ; step to the next map square
        [step side-distances delta-distances] (init-step-variables [ray-direction-x ray-direction-y] position (map deref map-coordinates))]
    ; cast a ray 
    (cast-ray world-map side-distances delta-distances step map-coordinates)))

