(ns observations.graph
  "Facilities for laying out a dataflow graph.")

(def sample-graph
  (doto (js/dagreD3.graphlib.Graph. #js {:compound true})
    (.setGraph #js {:nodesp 10 :ranksep 10})
    (.setNode "A" #js {:labelType "html"
                       :label     "<em>A</em>"
                       :rx        5
                       :ry        5})
    (.setNode "B" #js {:labelType "html"
                       :label     "<em>B</em>"
                       :rx        5
                       :ry        5})
    (.setEdge "A" "B" #js {:label "floweth"})))

(def render (js/dagreD3.render))
