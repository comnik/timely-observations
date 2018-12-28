(ns observations.graph
  "Facilities for laying out a dataflow graph."
  (:require
   [singult.core :as html]))

(defn node [{:operator/keys [name address]}]
  (let [label (html/render [:.label
                            [:span.name name]
                            " (" [:span.addr (str address)] ")"])]
    #js {:labelType "html"
         :label     label
         :rx        5
         :ry        5
         :width     150
         :height    40}))

(defn scope [address]
  #js {:label (str "Subgraph: " address)
       :style "fill: none; stroke: #000; stroke-width: 3px; stroke-dasharray: 2, 2;"
       :rx    5
       :ry    5})

(defn edge [{:channel/keys [id from to subgraph?]}]
  #js {:label (str id)
       :style (if subgraph?
                "stroke: #33f; stroke-width: 1px; stroke-dasharray: 5, 2; fill: none;"
                "")})

(def channel-query
  '[:channel/id :channel/from :channel/to :channel/subgraph?])

(def operator-query
  '[:db/id :operator/address :operator/name])

(defn graph [scopes operators channels]
  (let [g (doto (js/dagreD3.graphlib.Graph.
                 #js {:compound true})
            (.setGraph #js {:nodesp 50 :ranksep 50}))
        _ (doseq [address scopes]
            (.setNode g (str address) (scope address)))
        _ (doseq [{:operator/keys [address] :as op} operators]
            (.setNode g (str address) (node op))
            (when (> (count address) 1)
              (.setParent g (str address) (str (butlast address)))))
        _ (doseq [chan channels]
            (let [from (:channel/from chan)
                  to   (:channel/to chan)]
              (.setEdge g (str from) (str to) (edge chan))))]
    g))

(def sample-graph
  (let [scopes    []
        operators [#:operator{:name "map" :address "A"}
                   #:operator{:name "filter" :address "B"}]
        channels     []]
    (doto (graph scopes operators channels)
      (.setEdge "A" "B" #js {:label "floweth"}))))

(def render (js/dagreD3.render))
