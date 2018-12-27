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
         :ry        5}))

(defn parent [addr]
  (when (> (count addr) 2)
    (let [parent-addr (first addr)])))

(defn edge [{:channel/keys [id from to]}]
  #js {:label ""
       :style ""})

(def channel-query
  '[:channel/id :channel/from :channel/to :channel/subgraph?])

(def operator-query
  '[:operator/address :operator/name])

(defn graph [operators channels]
  (let [g (doto (js/dagreD3.graphlib.Graph. #js {:compound true})
            (.setGraph #js {:nodesp 50 :ranksep 50}))
        _ (doseq [op operators]
            (.setNode g (str (:operator/address op)) (node op)))
        _ (doseq [chan channels]
            (let [from (:channel/from chan)
                  to   (:channel/to chan)]
              (.setEdge g (str from) (str to) (edge chan))))]
    g))

(def sample-graph
  (doto (graph
         [#:operator{:name "map" :address "A"}
          #:operator{:name "filter" :address "B"}]
         [])
    (.setEdge "A" "B" #js {:label "floweth"})))

(def render (js/dagreD3.render))
