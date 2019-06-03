(ns kiwiland.search)

(defn add-step
  "Add a step to a path"
  [existing-steps new-step existing-distance new-distance]
  (hash-map :path (conj existing-steps new-step)
            :distance (+ (or existing-distance 0) new-distance)))


(defn add-next-steps
  "Adds the possible next steps to a path"
  [the-graph the-path]
  (let [next-steps (get the-graph (-> the-path :path last))]
    (map #(add-step (:path the-path) %
                    (:distance the-path) (get next-steps %))
         (keys next-steps))))


(defn expand
  "Expand a set of paths, by finding the next steps for each one"
  [the-graph paths]
  (-> (map #(cond (:complete? %) %
                  :else (add-next-steps the-graph %)) paths)
      flatten))


(defn prune-paths
  "Can we prune down our search space?"
  [paths prune?]
  (filter prune? paths))


(defn check-complete
  "Check whether any journeys are complete, based on the completion test."
  [paths complete?]
  (-> (map #(cond (:complete? %) %
                  (complete? %) (vector (assoc % :complete? true) %)
                  :else %)
           paths)
      flatten))


(defn all-paths-are-complete?
  "Check whether all paths have been marked as complete"
  [paths]
  (empty? (remove #(:complete? %) paths)))


(defn search
  "Walk the graph using a breadth-first approach.  A complete function is
   supplied which is used to flag when journeys are complete.  A prune condition
   is also supplied, to remove any paths that should no longer be pursued."
  [the-graph the-start complete? prune?]
  (let [paths (cond (keyword? the-start) [{:path [the-start]}]
                    :else the-start)
        expanded-paths (-> the-graph
                           (expand paths)
                           (prune-paths prune?)
                           (check-complete complete?))]
    (cond (all-paths-are-complete? expanded-paths) expanded-paths
          :else (recur the-graph expanded-paths complete? prune?))))
