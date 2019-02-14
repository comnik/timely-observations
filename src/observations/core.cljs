(ns ^:figwheel-hooks observations.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [datascript.core :as d]
   [observations.graph :as graph])
  (:require-macros [observations.core :refer [html]]))

(enable-console-print!)

(defn dbg! [x] (println x) x)

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

(defn use-atom
  [a]
  (let [[x set-x] (js/React.useState @a)]
    (js/React.useEffect
     (fn []
       (let [key (gensym "use-atom")]
         (add-watch a key (fn [_ _ x x']
                            (set-x x')))
         (fn []
           (remove-watch a key))))
     [a])
    x))

(defn use-db
  [conn]
  (let [[db set-db] (js/React.useState @conn)]
    (js/React.useEffect
     (fn []
       (let [key (gensym "use-db")]
         (d/listen! conn key (fn [tx-log]
                               (set-db (:db-after tx-log))))
         (fn []
           (d/unlisten! conn key))))
     [conn])
    db))

(defn use-query
  [conn q]
  (let [[result set-result] (js/React.useState nil)]
    (js/React.useEffect
     (fn []
       (let [key (gensym "use-query")]
         (d/listen! conn key (fn [tx-log]
                               (let [result' (d/q q (:db-after tx-log))]
                                 (set-result result'))))
         (fn []
           (d/unlisten! conn key))))
     [q])
    result))

   ;; :did-update
   ;; (fn [state]
   ;;   (let [[graph] (:rum/args state)
   ;;         g       (-> (js/d3.select "#graph-canvas")
   ;;                     (.selectAll "g"))]
   ;;     (try
   ;;       (graph/render g graph)
   ;;       (catch js/Error ex (.error js/console ex)))
   ;;     state))

(defn use-mount-effect
  [f]
  (js/React.useEffect f []))

(defn GraphCanvas
  [props]
  (let [graph (.-graph props)
        _     (use-mount-effect
               (fn []
                 (let [svg  (js/d3.select "#graph-canvas")
                       g    (.selectAll svg "g")
                       zoom (doto (js/d3.zoom)
                              (.on "zoom" #(.attr g "transform" js/d3.event.transform)))]
                   (.call svg zoom)
                   (fn []))))
        _     (js/React.useEffect
               (fn []
                 (let [svg (js/d3.select "#graph-canvas")
                       g   (.selectAll svg "g")]
                   (graph/render g graph)
                   (fn []))))]
    (html
     [:svg#graph-canvas {:height 1080}
      [:g]])))

(defn Root
  []
  (let [db           (use-db conn)
        history      (use-atom history)
        ui           (d/entity db :ui)
        query        (:ui/query ui)
        matches?     (if (some? query)
                       (fn [name] (str/starts-with? (str/lower-case name) (str/lower-case query)))
                       (constantly true))
        operators    (d/q '[:find [(pull ?e q) ...]
                            :in $ q matches?
                            :where
                            [?e :operator/name ?name]
                            [(not= ?name "ArrangedSource")]
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
    (html
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
         [:> GraphCanvas {:graph graph}])]])))

(defn mount
  [component node]
  (js/ReactDOM.render (js/React.createElement component) node))

(defn ^:after-load setup []
  (println "setup")
  (mount Root (.getElementById js/document "app-container")))

(defn ^:before-load teardown []
  (println "teardown"))

(setup)

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
  (into [] (d/datoms @conn :aevt :channel/from))
  
  
  )
