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
