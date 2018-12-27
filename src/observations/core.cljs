(ns observations.core
  (:require
   [clojure.set :as set]
   [datascript.core :as d]
   [rum.core :as rum]
   [singult.core :refer [render]]
   [observations.graph :as graph]))

(enable-console-print!)

(def conn
  (-> (d/empty-db {}) (d/conn-from-db)))

(defn select-static [$ id]
  (-> (.selectAll $ id) (.data (into-array [id]))))

(rum/defc graph-canvas
  < {:did-mount
     (fn [state]
       (let [[graph] (:rum/args state)
             svg     (js/d3.select "#graph-canvas")
             zoom    (doto (js/d3.zoom)
                       (.on "zoom" #(.attr svg "transform" js/d3.event.transform)))]
         (doto svg
           (graph/render graph)
           (.call zoom))
         state))
     
     :did-update
     (fn [state]
       (let [[graph] (:rum/args state)
             svg     (js/d3.select "#graph-canvas")]
         (graph/render svg graph)
         state))}
  [graph]
  [:svg#graph-canvas {:width 960 :height 1080}])

(defonce graph
  (atom
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
     (.setEdge "A" "B" #js {:label "floweth"}))))

(def ^:private ^:dynamic *reactions*)

(def selections
  (atom {:enter #{} :update #{} :exit #{}}))

;; @TODO key-fn
(defn update-selections [selections diffs]
  (->> diffs
       (map (fn [[x d]] {:value x :diff d}))
       (reduce
        (fn [selections {:keys [value diff]}]
          (let [add?      (pos? diff)
                retract?  (neg? diff)
                exiting?  (contains? (:exit selections) value)
                updating? (contains? (:update selections) value)
                entering? (contains? (:enter selections) value)
                move      (fn [v from to]
                            (-> selections
                                (update from disj v)
                                (update to conj v)))]
            (cond
              (and add? exiting?)      (move value :exit :update)
              (and add? updating?)     (move value :update :enter)
              (and add? entering?)     selections
              (and retract? exiting?)  selections
              (and retract? updating?) (move value :update :exit)
              (and retract? entering?) (move value :enter :update)
              add?                     (update selections :enter conj value)
              retract?                 (update selections :exit conj value))))
        (-> selections
            (assoc :exit #{})
            (assoc :enter #{})
            (update :update set/union (:enter selections))))))

(def differential
  {:init
   (fn [state props]
     (assoc state :differential/key (random-uuid)))
   
   :wrap-render
   (fn [render-fn]
     (fn [state]
       (binding [*reactions* (volatile! #{})]
         (let [comp             (:rum/react-component state)
               old-reactions    (:differential/refs state #{})
               [dom next-state] (render-fn state)
               new-reactions    @*reactions*
               key              (:differential/key state)]
           (doseq [ref old-reactions]
             (when-not (contains? new-reactions ref)
               (remove-watch ref key)))
           (doseq [ref new-reactions]
             (when-not (contains? old-reactions ref)
               (add-watch ref key
                          (fn [_ _ _ _]
                            (rum/request-render comp)))))
           [dom (assoc next-state :differential/refs new-reactions)]))))
   
   :will-unmountp
   (fn [state]
     (let [key (:differential/key state)]
       (doseq [ref (:differential/refs state)]
         (remove-watch ref key)))
     (dissoc state :differential/refs :differential/key))})

(defn diff-react
  [ref]
  (assert *reactions* "rum.core/react is only supported in conjunction with rum.core/reactive")
  (vswap! *reactions* conj ref)
  @ref)

(rum/defc root
  < differential
  [conn]
  (let [{:keys [enter update exit]} (diff-react conn)
        items                       (->> (set/union enter update exit)
                                         (sort))]
    [:div#app-container
     [:h1 "Timely Observations"]
     [:ul
      (for [i items]
        [:li {:key   i
              :class (cond
                       (contains? enter i)  "enter"
                       (contains? update i) "update"
                       (contains? exit i)   "exit")}
         (str "ITEM " i)])]
     #_(graph-canvas @graph)]))

(rum/mount (root selections) (.getElementById js/document "app-container"))

(comment
  
  (swap! selections update-selections [[1 1] [2 1] [3 1] [4 1] [5 1]])

  (swap! selections update-selections [[3 -1]])

  (swap! graph (fn [g]
                 (doto g
                   (.setNode "C" #js {:labelType "html"
                                      :label     "<em>C</em>"
                                      :rx        5
                                      :ry        5}))))
  )
