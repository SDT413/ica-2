(ns project1.Graph)
(defrecord Vertex [label lat lon neighbors status distance])
(defrecord Edge [from to label weight])
(defrecord Graph [vertices edges])
(defrecord Path [edges])

(defn make-graph [] (Graph. (ref `()) (ref `())))

(defn graph-add-vertex! [graph label lat lon]
  (dosync (ref-set (:vertices graph) (conj (deref (:vertices graph)) (Vertex. label lat lon (ref `()) (ref 0) (ref 0)))))
  label)

(defn graph-add-edge! [graph from to label weight]
  (dosync (ref-set (:edges graph) (conj (deref (:edges graph)) (Edge. from to label weight))))
  label)

(defn vertex-unseen? [neighbor]
  (= 0 @(:status neighbor)))

(defn add-to-queue
  ([queue graph neighbors]
   (add-to-queue queue graph neighbors false))
  ([queue graph neighbors bfs?]
   (loop [queue (if bfs? (reverse queue) queue)
          neighbors neighbors]
     (if (empty? neighbors)
       (if bfs? (reverse queue) queue)
       (let [neighbor-name (first neighbors)
             neighbor (get @(:vertices graph) neighbor-name)]
         (if (vertex-unseen? neighbor)
           (do
             (dosync
               (ref-set (:status neighbor) 1))
             (recur (conj queue neighbor-name) (rest neighbors)))
           (recur queue (rest neighbors))))))))

(defn graph-dfc! [graph start-point]
  (loop[queue (list start-point)]
    (when (not (empty? queue)))
    (let [current-label (first queue)
          current-vertex (get @(:vertices graph) current-label)]
      (dosync (ref-set (:status current-vertex) 2))
      (println current-label)
      (dosync (ref-set (:status current-vertex) 3))
      (recur (add-to-queue (rest queue) graph @(:neighbors current-vertex)))
      ))
  )
(defn dijkstra-best [graph label]
  (let [vertices @(:vertices graph)
        vertex (get vertices label)]
    (loop [neighbors-labels @(:neighbors vertex)
           best-distance nil
           best-label nil]
      (if (empty? neighbors-labels)
        best-label (let [neighbor-label (first neighbors-labels)
                         neighbor-vertex (get vertices neighbor-label)
                         neighbor-distance @(:distance neighbor-vertex)]
                     (if (or (nil? best-label)
                             (< neighbor-distance best-distance))
                       (recur (rest neighbors-labels)
                              neighbor-distance
                              neighbor-label)
                       (recur (rest neighbors-labels)
                              best-distance
                              best-label)))))))
(defn graph-iter! ([graph start]
                   (graph-iter! graph start false))
  ([graph start bfs?]
   (graph-iter! graph start bfs? (fn [x] nil)))
  ([graph start bfs? proc]
   (loop [queue (list start)]
     (when (not (empty? queue)))
     (let [current-label (first queue)
           current-vertex (get @(:vertices graph) current-label)]
       (dosync (ref-set (:status current-vertex) 2)
               (proc current-vertex) )
       (dosync (ref-set  (:status current-vertex) 3)
               (recur (add-to-queue (rest queue) graph @(:neighbors current-vertex bfs?))))))))

(defn graph-reset! [vertex]
  (ref-set (:status vertex) 0)
  (ref-set (:distance vertex) 0))

(defn print-graph [graph]
  (println "Vertices:")
  (doseq [vert (deref (:vertices graph))]
    (println (:label vert) " " (:lat vert) " " (:lon vert)))
  (println "Edges:")
  (doseq [ed (deref (:edges graph))]
    (println (:from ed) " " (:to ed))))

(defn dijkstra-mark! [graph finish]
  (graph-reset! graph)
  (dosync
    (ref-set (:distance (get @(:vertices graph) finish)) 0))
  )

(DEFN great-circle-distance ([graph (defn great-circle-distance
                                      ([graph label1 label2]
                                       (great-circle-distance (get @(:vertices graph) label1)
                                                              (get @(:vertices graph) label2)))
                                      )])
      )

(def graph (make-graph))
(graph-add-vertex! graph "A" 0 0)
(graph-add-vertex! graph "B" 0 0)
(graph-add-vertex! graph "C" 0 0)
(graph-add-edge! graph "A" "B" "AB" 1)
(graph-add-edge! graph "A" "C" "AC" 1)
(graph-add-edge! graph "B" "C" "BC" 1)
(dijkstra-mark! graph "AB")
(print-graph graph)
