(ns clojure-projects.ica2-v16)

(defrecord Graph [vertices vertices-map size])
(defrecord Vertex [label lat lon status neighbours distance])
(defrecord Neighbour [index weight])

(defn make-graph []  (Graph. (ref []) (ref {}) (ref 0)))

;;;;;;Support functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-index-by-label [graph label]
  (get @(:vertices-map graph) label))

(defn get-vertex-by-label [graph label]
  (get @(:vertices graph) (get-index-by-label graph label)))

(defn get-distance-from-index [graph index]
  (:distance (get @(:vertices graph) index)))

(defn set-vertex-status! [graph index val]
  (let [vertex (get @(:vertices graph) index)]
    (dosync
      (ref-set (:status vertex) val))))

;;;;;;End of supporting functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;Graph Construction;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        vertices-map (:vertices-map graph)
        index @(:size graph)
        vertex (Vertex. label lat lon (ref false) (ref '()) (ref Integer/MAX_VALUE))]
    (dosync
      (ref-set (:size graph) (inc index))
      (ref-set vertices-map (assoc @vertices-map label index))
      (ref-set vertices (conj @vertices vertex)))))

(defn graph-add-edge! [graph from to label weight]
  (let [
        from-vertex (get-vertex-by-label graph from)
        to-vertex (get-vertex-by-label graph to)
        neighbours-from (:neighbours from-vertex)
        neighbours-to (:neighbours to-vertex)]
    (dosync
      (ref-set neighbours-from
               (conj @neighbours-from
                     (Neighbour. (get-index-by-label graph to) weight)))
      (ref-set neighbours-to
               (conj @neighbours-to
                     (Neighbour. (get-index-by-label graph from) weight))))) label)

(defn graph-reset! [graph]
  (doseq [vertex (deref (:vertices graph))]
    (dosync
      (ref-set (:status vertex) false)
      (ref-set (:distance vertex) Integer/MAX_VALUE))))

;;;;;;End of Graph Construction;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;queue;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Queue [head tail])
(defrecord QueueNode [prev queue-index next])

(defn queue-make []
  (Queue. (ref nil) (ref nil)))

(defn queue-empty? [queue]
  (nil? @(:head queue)))

(defn queue-append! [queue val]
  (let [head (:head queue)
        tail (:tail queue)]
    (if (queue-empty? queue)
      (dosync
        (let [newNode (QueueNode. (ref nil) val (ref nil))]
          (ref-set head newNode)
          (ref-set tail newNode)))
      (dosync
        (let [newNode (QueueNode. (ref @tail) val (ref nil))]
          (ref-set (:next @tail) newNode)
          (ref-set tail newNode))))))

(defn queue-prepend! [queue val]
  (let [head (:head queue)
        tail (:tail queue)]
    (if (queue-empty? queue)
      (dosync
        (let [newNode (QueueNode. (ref nil) val (ref nil))]
          (ref-set head newNode)
          (ref-set tail newNode)))
      (dosync
        (let [newNode (QueueNode. (ref nil) val (ref @head))]
          (ref-set (:prev @head) newNode)
          (ref-set head newNode))))))

(defn queue-remove-first! [queue]
  (let [head (:head queue)
        tail (:tail queue)]
    (if (not (queue-empty? queue))
      (if (nil? @(:next @head))
        (dosync (ref-set head nil)
                (ref-set tail nil))
        (dosync (ref-set head @(:next @head))
                (ref-set (:prev @head) nil))))))

(defn queue-delete-all! [queue]
  (dosync
    (ref-set (:head queue) nil)
    (ref-set (:tail queue) nil)))

(defn insert-by-distance! [graph queue index]
  (let [head (:head queue)
        tail (:tail queue)
        vertex (get @(:vertices graph) index)
        distance (:distance vertex)]
    (if (queue-empty? queue)
      (queue-prepend! queue index)
      (dosync
        (if (<= @distance @(:distance (get @(:vertices graph) (:queue-index @head))))
          (queue-prepend! queue index)
          (if (>= @distance @(:distance (get @(:vertices graph) (:queue-index @tail))))
            (queue-append! queue index)
            (loop [LoopVertex @head]
              (when (not (nil? LoopVertex))
                (let [next-LoopVertex @(:next LoopVertex)
                      Potential-node (QueueNode. (ref LoopVertex) index (ref next-LoopVertex))
                      node-LoopVertex (get @(:vertices graph) (:queue-index LoopVertex))
                      distance-LoopVertex (:distance node-LoopVertex)
                      next-node-LoopVertex (get @(:vertices graph) (:queue-index next-LoopVertex))
                      distance-next-node-LoopVertex (:distance  next-node-LoopVertex)]
                  (if (and (>= @distance @distance-LoopVertex) (<= @distance @distance-next-node-LoopVertex))
                    (do
                      (ref-set (:next LoopVertex) Potential-node)
                      (ref-set (:prev next-LoopVertex) Potential-node))
                    (recur @(:next LoopVertex))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end of queue;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;marking distance;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-vertex [graph queue start]
  (let [index-start (get-index-by-label graph start)]
    (queue-prepend! queue index-start)
    (dosync (ref-set (get-distance-from-index graph index-start) 0))))

(defn finish-vertex [graph queue finish]
  (let [index-finish (get-index-by-label graph finish)]
    (queue-append! queue index-finish)))

(defn set-distance! [graph current neighbour weighted]
  (let [weight (if weighted (:weight neighbour) 1)
        distance-current (:distance current)
        distance-neighbour (get-distance-from-index graph (:index neighbour))
        distance-sum (+ @distance-current weight)]
    (when (< distance-sum @distance-neighbour)
      (dosync (ref-set distance-neighbour distance-sum)))))

(defn is-connected? [graph label]
  (not (= @(:status (get-vertex-by-label graph label)) false)))

(defn dijkstra-mark [graph queue finish weighted]
  (loop []
    (when (and (not (queue-empty? queue))
               (not (is-connected? graph finish)))
      (let [current (get @(:vertices graph) (:queue-index @(:head queue)))]
        (queue-remove-first! queue)
        (doseq [neighbour @(:neighbours current)]
          (let [status-neighbour @(:status (get @(:vertices graph) (:index neighbour)))]
            (when (= status-neighbour false)
              (set-distance! graph current neighbour weighted)
              (insert-by-distance! graph queue (:index neighbour))
              (set-vertex-status! graph (:index neighbour) true))))
        (recur))))
  (when (is-connected? graph finish)
    (queue-delete-all! queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;end of marking distance;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;Dijkstra;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dijkstra-trace [graph queue start weighted]
  (loop [current (get @(:vertices graph) (:queue-index @(:head queue)))]
    (if (not (= (:label current) start))
      (do
        (doseq [neighbour @(:neighbours current)]
          (let [distance-current (:distance current)
                distance-neighbour (get-distance-from-index graph (:index neighbour))
                weight (if weighted (:weight neighbour) 1)
                difference (- @distance-current @distance-neighbour)]
            (when (= weight difference)
              (queue-prepend! queue (:index neighbour)))))
        (recur (get @(:vertices graph) (:queue-index @(:head queue))))))))

(defn shortest-path [graph queue]
  (loop [resVertex @(:head queue)]
    (when (not (nil? resVertex))
      (let [vertex (get @(:vertices graph) (:queue-index resVertex))]
        (println (:label vertex) "(" @(:distance vertex) ")")
        (recur @(:next resVertex))))))

(defn dijkstra [graph start finish weighted]
  (let [queue (queue-make)]
    (graph-reset! graph)
    (start-vertex graph queue start)
    (dijkstra-mark graph queue finish weighted)
    (if (is-connected? graph finish)
      (do (finish-vertex graph queue finish)
          (dijkstra-trace graph queue start weighted)
          (shortest-path graph queue))
      (println start "is not connected with" finish))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;end of Dijkstra;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "e_roads_2020_full.clj")
(println "Trip from Samsun to Prague with weighted roads")
(dijkstra g "Samsun", "Prague", true)
(println "---------------------")
(println "Trip from Samsun to Prague with unweighted roads")
(println "---------------------")
(dijkstra g "Samsun", "Prague", false)

