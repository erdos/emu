(ns erdos.emu-test
  (:require [clojure.test :refer :all]
            [erdos.emu :refer :all]
            [clojure.spec.alpha :as spec]))


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
  (is (= [1 2 3] (topsort {3 [2] 2 [1] 1 []})))
  (is (empty? (topsort {})))
  (is (= [1 2 3] (topsort {1 [] 2 [] 3 []}))))

(deftest test-repeated-spec
  (is (= {:count 3 :numbers [1 2 3]}
         (spec/conform (repeated-spec :numbers integer?)
                       [3 1 2 3])))
  (is (= {:count 0}
         (spec/conform (repeated-spec :numbers integer?)
                       [0]))))

(comment
  (println :>>!0
           (-> (repeated-spec :letters string?)
               (spec/conform [7 "a" "b" "c" "d" "e" "f" "g"])))


  (println :>>!1
           (-> (spec/+ (repeated-spec :letters string?))
               (spec/conform [3 "a" "b" "c" 2 "x" "y"])))

  (println :>>!2
           (-> (spec/+ (repeated-spec :numbers int?))
               (spec/conform [3 11 12 13, 2 21 22])))

  (println :>>!3
           (-> (repeated-spec :test-count (repeated-spec :numbers int?))
               (spec/conform [2, 5 11 12 13 14 15, 4 21 22 23 24])))

  comment)
