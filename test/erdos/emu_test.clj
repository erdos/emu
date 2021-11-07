(ns erdos.emu-test
  (:require [clojure.test :refer :all]
            [erdos.emu :refer :all]))


(deftest test-long
  (is (= 12 (->long "12"))))


(deftest test-clusters
  (is (= {} (clusters {})))
  (is (= {} (clusters nil)))
  (is (= {1 1, 2 2} (clusters {1 [1] 2 [2]})))
  (is (= {1 1
          2 1
          3 1
          5 1
          6 6
          7 6}
         (clusters {1 [2 3] 2 [5] 6 [7]})))
  (is (= {1 1 2 2 3 3}
         (clusters {1 [] 2 [2] 3 []})))
  (is (= {1 3 2 3 3 3} ;; order
         (clusters {3 [2] 1 [1] 2 [1]})))
  (is (= {} (clusters {}))))


(deftest test-merge-cluster
  (is (= {1 1 2 1} (merge-cluster {1 1 2 2} 1 2)))
  (is (= {1 1 2 2} (merge-cluster {1 1 2 2} 1 1))))


(deftest test-fill-graph
  (is (= {} (fill-graph {})))
  (is (= {} (fill-graph nil)))
  (is (= {1 #{1}} (fill-graph {1 [1]})))
  (is (= {1 #{2} 2 #{}} (fill-graph {1 [2]}))))


(deftest test-invert-graph
  (is (= {} (invert-graph {})))
  (is (= {1 #{1}} (invert-graph {1 #{1}})))
  (is (= {1 #{3} 2 #{3}} (invert-graph {3 [1 2]}))))


(deftest test-topsort
  (is (= [1 2 3] (topsort {3 [2] 2 [1] 1 []}))))
