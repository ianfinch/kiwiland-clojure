(ns kiwiland.core-test
  (:require [clojure.test :refer :all]
            [kiwiland.core :refer :all]))

(deftest check-generated-answers
  (testing "Check generated answers"
    (is (= (generate-answers "AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
           '(9 5 13 22 "NO SUCH ROUTE" 2 3 9 9 7)))))


(deftest check-main
  (testing "The main function"
    (is (= (with-out-str (-main))
           "(9 5 13 22 NO SUCH ROUTE 2 3 9 9 7)\n"))))

(deftest check-completion-parser
  (testing "The completion parser"
    (is (= ((parse-complete [last = :B]) {:path [:A :B]})
           true))
    (is (= ((parse-complete [last = :C]) {:path [:A :B]})
           false))))

(deftest check-prune-parser
  (testing "The prune parser"
    (is (= ((parse-prune [:stops < 3]) {:path [:A :B :C]})
           true))
    (is (= ((parse-prune [:stops < 3]) {:path [:A :B :C :D]})
           false))))

(deftest check-fifth
  (testing "Function to get fifth element"
    (is (= (fifth nil) nil))
    (is (= (fifth []) nil))
    (is (= (fifth [1 2 3 4]) nil))
    (is (= (fifth [1 2 3 4 5]) 5))
    (is (= (fifth [1 2 3 4 5 6]) 5))))
