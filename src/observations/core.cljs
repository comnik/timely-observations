(ns observations.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [datascript.core :as d]
   [rum.core :as rum]
   [observations.graph :as graph]))

(enable-console-print!)

(def schema
  {:operator/name    {}
   :operator/address {:db/unique :db.unique/identity}

   :channel/id        {:db/unique :db.unique/identity}
   :channel/from      {:db/index true}
   :channel/to        {:db/index true}
   :channel/subgraph? {}

   :ui/query {}})

(defonce conn
  (-> (d/empty-db schema)
      #_(d/db-with [#:operator{:name "map" :address "A"}
                    #:operator{:name "filter" :address "B"}
                    #:channel{:from "A" :to "B"}])
      (d/conn-from-db)))

(defonce history (atom [@conn]))

(defn prevent-default [e] (.preventDefault e) e)

(defn change-version [idx]
  (d/reset-conn! conn (nth @history idx)))

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
         (try
           (graph/render g graph)
           (catch js/Error ex (.error js/console ex)))
         state))}
  [graph]
  [:svg#graph-canvas {:height 1080}
   [:g]])

(rum/defc root
  < rum/reactive
  [conn history]
  (let [db           (rum/react conn)
        history      (rum/react history)
        ui           (d/entity db :ui)
        query        (:ui/query ui)
        matches?     (if (some? query)
                       (fn [name] (str/starts-with? (str/lower-case name) (str/lower-case query)))
                       (constantly true))
        operators    (d/q '[:find [(pull ?e q) ...]
                            :in $ q matches?
                            :where
                            [?e :operator/name ?name]
                            [(matches? ?name)]] db graph/operator-query matches?)
        operator-ids (into #{} (map :db/id) operators)
        channels     (d/q '[:find [(pull ?e q) ...]
                            :in $ q operator-ids
                            :where
                            [?e :channel/from ?from]
                            [?e :channel/to ?to]
                            [?from-id :operator/address ?from]
                            [(contains? operator-ids ?from-id)]
                            [?to-id :operator/address ?to]
                            [(contains? operator-ids ?to-id)]] db graph/channel-query operator-ids)
        scopes       (->> (d/q '[:find [?address ...]
                                 :where
                                 [?e :operator/address ?address]
                                 [(> (count ?address) 1)]] @conn)
                          (map butlast)
                          (remove nil?)
                          (into #{}))
        graph        (graph/graph scopes operators channels)

        handle-query-change   (comp (fn [e] (d/transact! conn [{:db/ident :ui :ui/query (.. e -target -value)}])) prevent-default)
        handle-history-change (comp (fn [e] (change-version (js/parseInt (.. e -target -value)))) prevent-default)]
    [:div
     [:nav
      [:h1 "Timely Observations"]
      [:#tools
       [:#search
        [:input {:type        "text"
                 :placeholder "Filter"
                 :value       (:ui/query ui "")
                 :on-change   handle-query-change}]]
       [:#history-slider
        [:input {:type      "range"
                 :min       0
                 :max       (dec (count history))
                 :on-change handle-history-change}]]]]
     [:main
      (when (some? graph)
        (graph-canvas graph))]]))

(rum/mount (root conn history) (.getElementById js/document "app-container"))

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
                                           (map json->channel))
                             log      (d/transact! conn (concat operates channels))]
                         (swap! history conj (:db-after log))))]
  (doto (js/WebSocket. "ws://localhost:9000/ws/")
    (.addEventListener "message" on-message)))



(comment

  (d/transact! conn [#:operator{:name "filter" :address [0 1 2 3]}])

  (d/transact! conn [#:channel{:from "A" :to "C"}])

  (into [] (d/datoms @conn :aevt :operator/address))
  
  )
