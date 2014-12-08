(ns fp1.core
	(:gen-class)
  (:use clojure-csv.core)
)

; constants
(def ra 3)
(def rb (* ra 1.5))
(def alpha (/ 4 (Math/pow ra 2)))
(def beta (/ 4 (Math/pow rb 2)))
(def bottomEpsilon 0.15)
(def topEpsilon 0.5)

; distance functions
(defn euclideanDistance
  "The Euclidean distance function"
  [point anotherPoint]
  (Math/pow
    (apply +
      (for [index (range (min (count point) (count anotherPoint)))]
        (Math/pow
          (-
            (get (vec point) index)
            (get (vec anotherPoint) index)
          )
          2
        )
      )
    )
    0.5
  )
)

;; (defn hammingDistance
;;   "The Hamming distance function"
;;   [point anotherPoint]
;;   (apply +
;;     (for [index (range (min (count point) (count anotherPoint)))]
;;       (Math/abs (- (get (vec point) index) (get (vec anotherPoint) index)))
;;     )
;;   )
;; )

(defn hammingDistance
  "The Hamming distance function"
  [point anotherPoint]
  (reduce
    +
    (map
      (fn [base1 base2]
        (if (= base1 base2) 1 0))
      point
      anotherPoint
    )
  )
)

; functions
(defn convertToInputData [data]
  "Convert input data to numbers"
  (map
    (fn [params]
      { :values
        (map
          (fn [param] (-> param Double/parseDouble))
          (take (- (count params) 1) params)
        )
        :label
        (last params)
      }
    )
    data)
)

(defn findPotentialRelativeToPoint
  "Point potential relative to another point"
  [point anotherPoint coefficient distanceFunction]
  (Math/pow Math/E (- (* coefficient (Math/pow (distanceFunction (:values point) (:values anotherPoint)) 1))))
)

(defn findPotentialRelativeToAllPoints
  "Point potential relative to all the other point"
  [points basePoint coefficient distanceFunction]
  (reduce +
    (for [point points]
      (findPotentialRelativeToPoint basePoint point alpha distanceFunction)
    )
  )
)

(defn calcPotentials
  "Calculate the initial values of potentials"
  [points coefficient distanceFunction]
  (reverse
    (sort-by :potential
      (for [point points]
        (let [potential (findPotentialRelativeToAllPoints points point coefficient distanceFunction)]
          (assoc
            point
            :potential potential
            ;:absPotential potential
          )
        )
      )
    )
  )
)

(defn recalcPotentials
  "Calculate the potentials by reducing the initial value and reset potential for resetIndex point if it is set"
  [points centerPoint coefficient distanceFunction & resetIndex]
  (reverse
    (sort-by :potential
      (for [index (range (count points))]
        (let [point (get (vec points) index)
              isResetIndex (= index (first resetIndex))
              potential
                (- (:potential point) (* (:potential centerPoint) (findPotentialRelativeToPoint centerPoint point coefficient distanceFunction)))]
          (assoc
            point
            :potential (if isResetIndex 0 potential)
            ;:absPotential (if isResetIndex 0 (:absPotential point))
          )
        )
      )
    )
  )
)

(defn findShortestDistance
  [point points distanceFunction]
  (apply
    min
    (for [center points]
      (distanceFunction (:values point) (:values center))
    )
  )
)

(defn clusterizeRecursive
  [revisedPoints clusterCenters distanceFunction & resetPotentialIndex]
  ;(println)
  ;(println clusterCenters)
  ;(println revisedPoints)
  ;(println)
  ;(print "*0")
  (if
    (> (count revisedPoints) 0)
    (let [updatedPotentialPoints (recalcPotentials revisedPoints (-> clusterCenters last) beta distanceFunction (first resetPotentialIndex))
          lastCenterPotential (-> clusterCenters last :potential)
          currentCenterPotential (-> updatedPotentialPoints first :potential)]
      ;(print "*1")
      (if
        (> currentCenterPotential (* topEpsilon lastCenterPotential))
        (recur (rest updatedPotentialPoints) (conj (vec clusterCenters) (first updatedPotentialPoints)) distanceFunction nil)
        (if
          (< currentCenterPotential (* bottomEpsilon lastCenterPotential))
          clusterCenters
          (let [dMin (findShortestDistance (-> updatedPotentialPoints first) clusterCenters distanceFunction)]
            ;(print "*2")
            (if
              (>= (+ (/ dMin ra) (/ (-> updatedPotentialPoints first :potential) lastCenterPotential)))
              (recur (rest updatedPotentialPoints) (conj (vec clusterCenters) (first updatedPotentialPoints)) distanceFunction nil)
              (recur revisedPoints clusterCenters distanceFunction 0)
            )
          )
        )
      )
    )
    clusterCenters
  )
)

(defn clusterize
  "Performs clustering"
  [points distanceFunction]
  (let [revisedPoints (calcPotentials points alpha distanceFunction)]
    (map
      (fn [point]
        (dissoc point :potential :absPotential)
      )
      (clusterizeRecursive (rest revisedPoints) [(first revisedPoints)] distanceFunction)
    )
  )
)

(defn -main [& args]
  (if
    (.exists (clojure.java.io/as-file (first args)))
    (let [filename (first args)
          distanceFunction (if (= (second args) "h") hammingDistance euclideanDistance)
          points (convertToInputData (parse-csv (slurp filename) :delimiter \,))
          centerPoints (clusterize points distanceFunction)]
      (for [point centerPoints]
        (println (:label point) " - " (:values point))
      )
      (println centerPoints)
      centerPoints
    )
    (println "file isn't exist:" (first args))
  )
)

;(println "----------")
(def filenames
  (map (fn [x] (str "./task/" x)) ["butterfly.txt" "glass.txt" "irises.txt"])
)
;(-main (last filenames) "e")
;(println "----------")
