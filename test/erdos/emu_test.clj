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

(deftest test-proprity-queue
  (let [pk (->PriorityQueue {:z 3 :y 2 :x 1 :v 2})]
    (is (= 1 (get pk :x)))
    (is (= 1 (:x pk)))
    (is (= 1 (pk :x) (pk :x -1)))
    (is (= [[:x 1] [:y 2] [:v 2] [:z 3]]
           (seq pk)))
    (is (= (str {:x 1 :y 2 :v 2 :z 3})
           (str pk)))
    (is (not (empty? pk)))
    (is (= [:x 1] (first pk)))
    (is (= [:z 3] (last pk)))
    (let [pk2 (assoc pk :x 4)]
      (is (= [[:y 2] [:v 2] [:z 3] [:x 4]] (seq pk2))))))


(deftest test-dijkstra
  (is (= 6 (:end-cost (dijkstra {:a {:b 1 :c 200} :b {:c 2 :d 100} :c {:d 3}} :a :d))))
  (is (= 3 (:end-cost (dijkstra {:a {:b 1} :b {:a 1 :c 1} :c {:a 1 :d 1}} :a :d)))))


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
