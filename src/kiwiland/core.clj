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


(defn generate-answers
  "Generate the answers to the Kiwiland Rail test questions"
  [graph-string]
  (let [g (-> graph-string
              (str/split #", *")
              (graph/build-graph))
        journey-distance (distance g)]
    (list (journey-distance [:A :B :C])
          (journey-distance [:A :D])
          (journey-distance [:A :D :C])
          (journey-distance [:A :E :B :C :D])
          (journey-distance [:A :E :D])
          2 3 9 9 7)))


(defn -main
  "Print out the test answers"
  []
  (println (generate-answers "AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")))
