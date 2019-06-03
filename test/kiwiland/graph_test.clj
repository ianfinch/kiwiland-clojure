(ns kiwiland.graph-test
  (:require [clojure.test :refer :all]
            [kiwiland.graph :refer :all]))

(deftest created-first-node
  (testing "Creating the first node in a graph"
    (is (= (add-node nil :a)
           {:a nil}))))

(deftest add-a-node-to-graph
  (testing "Adding a node to an existing graph"
    (is (= (add-node {:a nil} :b)
           {:a nil :b nil}))))

(deftest add-an-existing-node
  (testing "Trying to add an existing node to a graph"
    (is (= (add-node {:a {:b 1} :b nil} :a)
           {:a {:b 1} :b nil}))))

(deftest add-an-edge-to-empty
  (testing "Adding an edge to an empty graph"
    (is (= (add-edge nil :a :b 1)
           {:a {:b 1} :b nil}))))

(deftest add-edge-from-existing-source
  (testing "Adding an edge from an existing source node"
    (is (= (add-edge {:a {:c 1} :c nil} :a :b 1)
           {:a {:b 1 :c 1} :b nil :c nil}))))

(deftest add-edge-to-existing-target
  (testing "Adding an edge to an existing target node"
    (is (= (add-edge {:b {:c 1} :c nil} :a :b 1)
           {:a {:b 1} :b {:c 1} :c nil}))))

(deftest add-edge-between-existing-nodes
  (testing "Adding an edge between two existing nodes"
    (is (= (add-edge {:a {:c 1} :b {:c 1} :c nil} :a :b 1)
           {:a {:b 1 :c 1} :b {:c 1} :c nil}))))

(deftest add-disjoint-edge-into-existing
  (testing "Adding two new nodes and an edge into an existing graph"
    (is (= (add-edge {:a {:b 1} :b nil} :c :d 4)
           {:a {:b 1} :b nil :c {:d 4} :d nil}))))

(deftest update-existing-edge
  (testing "Updating an edge which already exists"
    (is (= (add-edge {:a {:b 1} :b {:c 1} :c nil} :a :b 2)
           {:a {:b 2} :b {:c 1} :c nil}))))

(deftest add-an-edge-string-to-empty
  (testing "Adding an edge to an empty graph"
    (is (= (add-edge-from-string nil "ab1")
           {:a {:b 1} :b nil}))))

(deftest add-edge-string-from-existing-source
  (testing "Adding an edge from an existing source node"
    (is (= (add-edge-from-string {:a {:c 1} :c nil} "ab1")
           {:a {:b 1 :c 1} :b nil :c nil}))))

(deftest add-edge-string-to-existing-target
  (testing "Adding an edge to an existing target node"
    (is (= (add-edge-from-string {:b {:c 1} :c nil} "ab1")
           {:a {:b 1} :b {:c 1} :c nil}))))

(deftest add-edge-string-between-existing-nodes
  (testing "Adding an edge between two existing nodes"
    (is (= (add-edge-from-string {:a {:c 1} :b {:c 1} :c nil} "ab1")
           {:a {:b 1 :c 1} :b {:c 1} :c nil}))))

(deftest add-disjoint-edge-string-into-existing
  (testing "Adding two new nodes and an edge into an existing graph"
    (is (= (add-edge-from-string {:a {:b 1} :b nil} "cd4")
           {:a {:b 1} :b nil :c {:d 4} :d nil}))))

(deftest update-existing-edge-string
  (testing "Updating an edge which already exists"
    (is (= (add-edge-from-string {:a {:b 1} :b {:c 1} :c nil} "ab2")
           {:a {:b 2} :b {:c 1} :c nil}))))

(deftest add-incorrect-edge-string
  (testing "Adding an edge from an incorrectly formed string"
    (is (= (add-edge-from-string {:a {:c 1} :c nil} "1ab")
           {:a {:c 1} :c nil}))))

(deftest add-edge-string-with-extra-digit
  (testing "Adding an edge with an extra digit"
    (is (= (add-edge-from-string {:a {:c 1} :c nil} "ab11")
           {:a {:c 1} :c nil}))))

(deftest build-complete-graph
  (testing "Building the graph from an array of edges"
    (is (= (build-graph '("AB5" "BC4" "CD8" "DC8" "DE6" "AD5" "CE2" "EB3" "AE7"))
           {:A {:B 5, :D 5, :E 7},
            :B {:C 4},
            :C {:D 8, :E 2},
            :D {:C 8, :E 6},
            :E {:B 3}}))))

(deftest distance-between-two-stops
  (testing "Calculating the distance between two stops"
    (let [g (build-graph ["ab1"])]
      (is (= (one-stop g :a :b) 1)))))

(deftest distance-two-stops-no-start
  (testing "The distance between two stops, when the start does not exist"
    (let [g (build-graph ["ab1"])]
      (is (= (one-stop g :x :b) nil)))))

(deftest distance-two-stops-no-finish
  (testing "The distance between two stops, when the finish does not exist"
    (let [g (build-graph ["ab1"])]
      (is (= (one-stop g :a :x) nil)))))

(deftest distance-two-stops-none-exist
  (testing "The distance between two stops, when neither exist"
    (let [g (build-graph ["ab1"])]
      (is (= (one-stop g :x :y) nil)))))

(deftest distance-two-stops-no-connection
  (testing "The distance between two stops, when there is no connection"
    (let [g (build-graph ["ab1" "bc1"])]
      (is (= (one-stop g :a :c) nil)))))

(deftest distance-for-journey
  (testing "The distance for a multi-stop journey is correctly calculated"
    (let [g (build-graph ["ab1" "bc1" "cd1" "de1"])]
      (is (= (distance g '(:a :b :c :d)) 3)))))

(deftest distance-broken-journey
  (testing "The distance for an impossible journey is returned as nil"
    (let [g (build-graph ["ab1" "bc1" "cd1" "de1"])]
      (is (= (distance g [:a :c :b :d]) nil)))))

(deftest distance-one-stop-journey
  (testing "The distance for a one stop is correctly calculated"
    (let [g (build-graph ["ab1" "bc1" "cd1" "de1"])]
      (is (= (distance g [:a :b]) 1)))))

(def test-graph (build-graph ["ab1" "bc1" "cb1" "bd1" "dc1"]))

(deftest check-search
  (testing "Testing the imported search function"
    (is (= (search test-graph :a #(= (-> % :path last) :c) #(< (-> % :distance) 5))
           [{:path [:a :b :c], :complete? true, :distance 2}
            {:path [:a :b :c :b :c], :complete? true, :distance 4}
            {:path [:a :b :d :c], :complete? true, :distance 3}]))))
