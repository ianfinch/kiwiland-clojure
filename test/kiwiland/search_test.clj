(ns kiwiland.search-test
  (:require [clojure.test :refer :all]
            [kiwiland.graph :as graph]
            [kiwiland.search :refer :all]))

(deftest added-step
  (testing "Adding a step to a path"
    (is (= (add-step [:A :B] :C 5 2)
           {:path [:A :B :C] :distance 7}))))

(deftest add-first-step
  (testing "Adding the first step to a node"
    (is (= (add-step [:A] :B nil 1)
           {:path [:A :B] :distance 1}))))

(def test-graph (graph/build-graph ["ab1" "bc1" "cb1" "bd1" "dc1"]))

(deftest check-next-steps
  (testing "Find next steps in a path"
     (is (= (add-next-steps test-graph {:path [:a :b] :distance 1})
            [{:path [:a :b :c] :distance 2}
             {:path [:a :b :d] :distance 2}]))))

(deftest check-expand
  (testing  "Check expanding the set of search paths"
    (is (= (expand test-graph [{:path [:a]} {:path [:a :b] :distance 1}])
           [{:path [:a :b], :distance 1}
            {:path [:a :b :c], :distance 2}
            {:path [:a :b :d], :distance 2}]))))

(deftest check-expand-with-complete
  (testing "Check expand when some paths are complete"
    (is (= (expand test-graph [{:path [:a :b :c] :distance 2 :complete? true}
                               {:path [:a :b :d] :distance 2}])
           [{:path [:a :b :c] :distance 2 :complete? true}
            {:path [:a :b :d :c] :distance 3}]))))

(deftest check-pruning
  (testing "Check paths are pruned correctly"
    (let [prune? (fn [path] (< (:distance path) 5))
          inputs [{:path [:a :b] :distance 1}
                  {:path [:a :b :c] :distance 3 :complete? true}
                  {:path [:a :b :c :d] :distance 5 :complete? true}
                  {:path [:a :b :c :d  :e] :distance 7}]
          outputs [{:path [:a :b] :distance 1}
                   {:path [:a :b :c] :distance 3 :complete? true}]]
      (is (= (prune-paths inputs prune?)
             outputs)))))

(deftest check-completion
  (testing "Check completions are correctly identified"
    (let [complete? (fn [path] (= (-> path :path last) :c))
          inputs [{:path [:a :b] :distance 1}
                  {:path [:a :b :c] :distance 2 :complete? true}
                  {:path [:a :b :c :d] :distance 3}
                  {:path [:a :b :c :d :c] :distance 4}]
          outputs [{:path [:a :b] :distance 1}
                   {:path [:a :b :c] :distance 2 :complete? true}
                   {:path [:a :b :c :d] :distance 3}
                   {:path [:a :b :c :d :c] :distance 4 :complete? true}
                   {:path [:a :b :c :d :c] :distance 4}]]
      (is (= (check-complete inputs complete?)
             outputs)))))

(deftest check-all-completed-true
  (testing "Check positive outcome for all paths being complete"
    (is (= (all-paths-are-complete? [{:path [:a :b] :complete? true :distance 1}
                                     {:path [:a :b :c] :complete? true :distance 2}])
           true))))

(deftest check-all-completed-false
  (testing "Check negative outcome for all paths being complete"
    (is (= (all-paths-are-complete? [{:path [:a :b] :complete? true :distance 1}
                                     {:path [:a :b :c] :distance 2}])
           false))))

(deftest check-search
  (testing "Testing the top-level search function"
    (is (= (search test-graph :a #(= (-> % :path last) :c) #(< (-> % :distance) 5))
           [{:path [:a :b :c], :complete? true, :distance 2}
            {:path [:a :b :c :b :c], :complete? true, :distance 4}
            {:path [:a :b :d :c], :complete? true, :distance 3}]))))
