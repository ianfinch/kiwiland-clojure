(ns kiwiland.core
  (:require [kiwiland.graph :as graph])
  (:require [clojure.string :as str]))


(defn distance
  "Get the distance for a journey, converting a nil response to an error string.
   This returns a closure, so I can combine with a graph for easier use."
  [the-graph]
  (fn [route]
    (or (graph/distance the-graph route)
        "NO SUCH ROUTE")))


(defn parse-complete
  "Take the expression representing a complete condition, and generate the
   appropriate function from it."
  [[expr op value]]
  (fn [path]
    (op (-> path :path expr) value)))


(defn parse-prune
  "Take the expression representing a prune condition, and generate the
   appropriate function from it."
  [[elem op value]]
  (fn [path]
    (let [stops (-> path :path count (- 1))
          path-with-stops (assoc path :stops stops)]
      (op (get path-with-stops elem) value))))


(defn count-paths
  "Search through the graph, to find solutions which satisfy the passed
   conditions, then return the total."
  [the-graph]
  (fn [start-node complete-condition prune-condition]
    (let [complete? (parse-complete complete-condition)
          prune? (parse-prune prune-condition)]
      (-> (graph/search the-graph start-node complete? prune?)
          count))))


(defn fifth
  "Get the fifth element of a list"
  [l]
  (nth l 4 nil))


(defn generate-answers
  "Generate the answers to the Kiwiland Rail test questions"
  [graph-string]
  (let [g (-> graph-string
              (str/split #", *")
              (graph/build-graph))
        journey-distance (distance g)
        number-of-trips (count-paths g)]
    (list (journey-distance [:A :B :C])
          (journey-distance [:A :D])
          (journey-distance [:A :D :C])
          (journey-distance [:A :E :B :C :D])
          (journey-distance [:A :E :D])
          (number-of-trips :C [last = :C] [:stops <= 3])
          (number-of-trips :A [fifth = :C] [:stops <= 4])
          9 9
          (number-of-trips :C [last = :C] [:distance < 30]))))


(defn -main
  "Print out the test answers"
  []
  (println (generate-answers "AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")))
