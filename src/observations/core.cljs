(ns observations.core
  (:require
   [clojure.set :as set]
   [datascript.core :as d]
   [rum.core :as rum]
   [observations.graph :as graph]))

(enable-console-print!)

(def schema
  {:operator/name     {}
   :operator/address  {:db/unique :db.unique/identity}
   :channel/id        {:db/unique :db.unique/identity}
   :channel/from      {}
   :channel/to        {}
   :channel/subgraph? {}})

(defonce conn
  (-> (d/empty-db schema)
      #_(d/db-with [#:operator{:name "map" :address "A"}
                    #:operator{:name "filter" :address "B"}
                    #:channel{:from "A" :to "B"}])
      (d/conn-from-db)))

(rum/defc graph-canvas
  < {:did-mount
     (fn [state]
       (let [[graph] (:rum/args state)
             svg     (js/d3.select "#graph-canvas")
             g       (.selectAll svg "g")
             zoom    (doto (js/d3.zoom)
                       (.on "zoom" #(.attr g "transform" js/d3.event.transform)))]
         (do
           (graph/render g graph)
           (.call svg zoom))
         state))
     
     :did-update
     (fn [state]
       (let [[graph] (:rum/args state)
             g       (-> (js/d3.select "#graph-canvas")
                         (.selectAll "g"))]
         (graph/render g graph)
         state))}
  [graph]
  [:svg#graph-canvas {:height 1080}
   [:g]])

(rum/defc root
  < rum/reactive
  [conn]
  (let [db        (rum/react conn)
        operators (d/q '[:find [(pull ?e q) ...]
                         :in $ q
                         :where [?e :operator/name _]] db graph/operator-query)
        channels  (d/q '[:find [(pull ?e q) ...]
                         :in $ q
                         :where
                         [?e :channel/from ?from]
                         [?e :channel/to ?to]
                         [_ :operator/address ?from]
                         [_ :operator/address ?to]] db graph/channel-query)
        graph     (graph/graph operators channels)]
    [:div
     [:nav
      [:h1 "Timely Observations"]]
     [:main
      (graph-canvas graph)]]))

(rum/mount (root conn) (.getElementById js/document "app-container"))

(let [json->operator (fn [obj]
                       (let [obj (.-Operate obj)]
                         {:operator/address (js->clj (.-addr obj))
                          :operator/name    (.-name obj)}))
      json->channel  (fn [obj]
                       (let [obj       (.-Channel obj)
                             from-addr (js->clj (.-from_addr obj))
                             to-addr   (js->clj (.-to_addr obj))]
                         {:channel/id        (.-id obj)
                          :channel/from      from-addr
                          :channel/to        to-addr
                          :channel/subgraph? (.-subgraph obj)}))
      operate?       #(.hasOwnProperty % "Operate")
      channel?       #(.hasOwnProperty % "Channel")
      on-message     (fn [event]
                       (let [updates  (-> event
                                          (.-data)
                                          (js/JSON.parse)
                                          (.-updates))
                             operates (->> updates
                                           (filter operate?)
                                           (map json->operator))
                             channels (->> updates
                                           (filter channel?)
                                           (map json->channel))]
                         (d/transact! conn (concat operates channels))))]
  (doto (js/WebSocket. "ws://localhost:9000/ws/")
    (.addEventListener "message" on-message)))



(comment

  (d/transact! conn [#:operator{:name "filter" :address [0 1 2 3]}])

  (d/transact! conn [#:channel{:from "A" :to "C"}])

  (into [] (d/datoms @conn :aevt :operator/address))
  
  )
