(ns kiwiland.graph
  (:require [clojure.string :as str]))


(defn add-node
  "Add a node to the graph"
  [the-graph node-key]
  (cond (get the-graph node-key) the-graph
        :else (assoc the-graph node-key nil)))


(defn add-edge
  "Add an edge to the graph"
  [the-graph source-key target-key value]
  (assoc
    (add-node the-graph target-key)
    source-key
    (-> (get the-graph source-key)
        (assoc target-key value))))


(defn add-edge-from-valid-string
  "Add an edge to the graph, provided as string of format AB5"
  [the-graph edge-def]
  (let [character (str/split edge-def #"")
        source-name (first character)
        target-name (second character)
        distance (Integer/parseInt (last character))]
    (add-edge the-graph (keyword source-name) (keyword target-name) distance)))


(defn skip-too-many-characters
  "Display an error message, and return the passed in graph"
  [the-graph edge-def]
  (println "Skipping" edge-def "since it has more than 3 characters")
  the-graph)


(defn try-add-edge-from-string
  "Add an edge from a string, trapping parsing errors"
  [the-graph edge-def]
  (try (add-edge-from-valid-string the-graph edge-def)
    (catch Exception e (do (println "Skipping" edge-def "could not parse:" (.getClass e))
                           the-graph))))


(defn add-edge-from-string
  "Add an edge to the graph, provided as string, after running validation checks"
  [the-graph edge-def]
  (cond (= (count edge-def) 3) (try-add-edge-from-string the-graph edge-def)
        :else (skip-too-many-characters the-graph edge-def)))


(defn build-graph
  "Builds a graph from an array of string expressions"
  [edge-list]
  (reduce add-edge-from-string nil edge-list))


(defn one-stop
  "Gets the distance for one stop from the starting node"
  [the-graph start-node stop-node]
  (-> the-graph
      (get start-node)
      (get stop-node)))


(defn distance
  "Calculate the distance for a multi-stop journey"
  [the-graph route]
  (let [starts (drop-last 1 route)
        finishes (drop 1 route)
        segments (map #(one-stop the-graph %1 %2) starts finishes)]
    (cond (some nil? segments) nil
          :else (apply + segments))))
