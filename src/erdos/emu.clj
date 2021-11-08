(ns erdos.emu
  (:require [clojure.spec.alpha :as spec]))


(defn ->long [s] (Long/parseLong (str s)))


(defn add-edge [graph [a b]] (update graph a conj b))


(defn merge-cluster [cluster a b]
  (let [c1 (cluster a)
        c2 (cluster b)]
    (into {} (for [[k v] cluster] (if (= c2 v) [k c1] [k v])))))


(defn clusters [graph]
  (let [all-nodes (set (apply concat (keys graph) (vals graph)))]
    (reduce-kv (fn [cs k vs]
                 (reduce (fn [cs v] (merge-cluster cs k v)) cs vs))
               (zipmap all-nodes all-nodes)
               graph)))


;; convert values to sets, add empty set values
(defn fill-graph [graph]
  (into (zipmap (keys graph) (map set (vals graph)))
        (for [vs (vals graph) v vs :when (not (graph v))] [v #{}])))


(defn invert-graph [graph]
  (reduce-kv (fn [graph k vs] (reduce (fn [graph v] (update graph v (fnil conj #{}) k)) graph vs)) {} graph))


(defn topsort [graph]
  (assert (map? graph))
  (let [g-rev (invert-graph graph)
        graph (fill-graph graph)]
    ((fn f [graph checkable i]
       (if-let [[checked & checkable] (seq (drop-while (comp not-empty graph) checkable))]
         (let [dependants (g-rev checked)
               graph (reduce (fn [graph r] (update graph r disj checked)) graph dependants)]
           (cons checked
                 (lazy-seq
                  (f graph
                     (concat (for [c dependants :when (empty? (graph c))] c) checkable)
                     (dec i)))))
         (when (pos? i)
           (throw (ex-info "Graph is cyclic!" {})))))
     graph
     (for [[k v] graph :when (empty? v)] k)
     (count graph))))


;; a regex spec that reads an integer n and then n elements conforming to f.
(defmacro repeated-spec [kw f]
  (assert (keyword? kw))
  ;; (assert (spec? f))
  `(spec/& (spec/cat :count int? ~kw (spec/* ~f))
           (fn [x#] (= (:count x#) (count (~kw x#))))))


:OK
