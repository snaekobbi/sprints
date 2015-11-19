(ns app.long-term
  (:require [jayq.core :as $])
  (:use [clojure.string :only [join]]
        [crate.core :only [html]]))

(defn- keyword-map [map]
  (into {} (for [[k v] map] [(keyword k) v])))

(defn- change-val [map key fun]
  (if-let [val (key map)]
    (assoc map key (fun val))
    map))

(defn- get-issues [page callback]
  ($/ajax
   "https://api.github.com/repos/snaekobbi/sprints/issues"
   {:data (clj->js {:page page
                    :state "all"})
    :success
    (fn [issues]
      (if-let [issues (not-empty (js->clj issues))]
        (let [issues (for [issue issues]
                       (let [issue
                             (-> issue
                                 (select-keys ["number" "title" "milestone" "labels" "state"])
                                 (keyword-map)
                                 (change-val :milestone
                                             (fn [milestone]
                                               (-> milestone
                                                   (select-keys ["title" "due_on"])
                                                   (keyword-map)
                                                   (change-val :due_on #(.parse js/Date %)))))
                                 (change-val :labels
                                             (fn [labels]
                                               (->> labels
                                                    (map #(get % "name"))))))
                             issue
                             (if-let [milestone (:title (:milestone issue))]
                               (if-let [sprint (first (rest (re-matches #"sprint#([0-9]+)" milestone)))]
                                 (assoc issue :sprint (js/parseInt sprint))
                                 issue)
                               issue)
                             issue
                             (let [pass (boolean (some #{"PASS"} (:labels issue)))
                                   fail (boolean (and (not pass) (some #{"FAIL"} (:labels issue))))
                                   closed (= (:state issue) "closed")]
                               (assoc issue :pass pass :fail fail :closed closed))
                             issue
                             (if-let [priority (->> (:labels issue)
                                                    (map #(re-matches #"priority:([123])" %))
                                                    (remove nil?)
                                                    first
                                                    rest
                                                    first)]
                               (assoc issue :priority priority)
                               issue)
                             issue
                             (if-let [state (->> (:labels issue)
                                                 (map #(re-matches #"([0-4]) - .+" %))
                                                 (remove nil?)
                                                 first
                                                 rest
                                                 first)]
                               (assoc issue :state
                                      (get [:todo :doing :ready-for-test :testing :done]
                                           (js/parseInt state)))
                               issue)]
                         issue))
              issues (filter #(re-matches #"\[[0-9]+\.[0-9]+\:[0-9]+[A-Z]?\] .+" (:title %)) issues)]
          (get-issues (+ page 1) #(callback (concat issues %))))
        (callback nil)))}))

(defn- get-features [callback]
  (get-issues
   1
   (fn [issues]
     (let [now (.now js/Date)
           current-sprint (->> issues
                              (filter :sprint)
                              (map :milestone)
                              (into #{})
                              (sort-by :due_on)
                              (drop-while #(< (:due_on %) now))
                              first
                              :title
                              (re-matches #"sprint#([0-9]+)")
                              rest
                              first
                              js/parseInt)
           features
           (->> issues
                (map (fn [issue]
                       (let [[number sort title]
                             (rest (re-matches #"\[([0-9]+\.[0-9]+\:([0-9]+[A-Z]?))\] (.+)" (:title issue)))]
                         {:number number
                          :title title
                          :sort (js/parseInt sort)
                          :issue issue})))
                (group-by :number)
                (map val)
                (map #(assoc (dissoc (first %) :issue) :issues (map :issue %)))
                (sort-by :sort)
                (map (fn [feature]
                       (let [sometime (.parse js/Date "2017-01-01")
                             issues (sort-by #(or (:due_on (:milestone %)) sometime) (:issues feature))]
                         (assoc feature
                           :issues issues
                           :state (if (some #{"wontfix"} (flatten (map :labels issues)))
                                    :cancelled
                                    (if (some #(and (not (:closed %)) (:sprint %)) issues)
                                      (if (<= (:sprint (first issues)) current-sprint)
                                        (if (some #(or (:fail %) (:pass %)) issues)
                                          :testing
                                          :doing)
                                        :planned)
                                      (if (:pass (last issues))
                                        :done
                                        (if (some #(:sprint %) issues)
                                          (if (or (some #(not (:sprint %)) issues)
                                                  (:fail (last issues))
                                                  (= (:state (last issues)) :todo)
                                                  (= (:state (last issues)) :doing))
                                            :on-hold
                                            :ready-for-test)
                                          :todo)))))))))]
       (callback current-sprint features)))))

(defn draw [current-sprint features]
  (let [sprints (sort (into #{} (remove nil? (flatten (map #(map :sprint (:issues %)) features)))))]
    (.append
     ($/$ "body")
     ($/$ (html
           [:div
            [:div#heading
             [:table
              [:tr
               [:th {:colspan 2} "Feature"]
               [:th {:colspan (+ 1 (count sprints))} "Sprint"]
               [:th "State"]]
              [:tr
               [:th {:colspan 2}]
               (for [sprint sprints]
                 [:th (when (= sprint current-sprint) {:class "current-sprint"})
                  [:a {:href (str "https://github.com/snaekobbi/sprints/milestones/sprint%23" sprint)
                       :target "blank_"} sprint]])
               [:th "?"]
               [:th]]]]
            [:div#features
             [:table
              [:colgroup
               [:col]
               [:col]
               (repeat (- current-sprint 1) [:col])
               [:col {:class "current-sprint"}]
               (repeat (- (count sprints) current-sprint) [:col])
               [:col]
               [:col]]
              [:tbody
               (for [{number :number title :title state :state issues :issues} features]
                 [:tr {:class (join " " ["feature" (name state)])}
                  [:td [:a {:href (str "http://snaekobbi.github.io/requirements/#" number)
                            :target "_blank"}
                        number]]
                  [:td title]
                  (for [sprint (concat sprints [nil])]
                    [:td
                     (when-let [issue (first (filter #(= (:sprint %) sprint) issues))]
                       (let [{number :number pass :pass fail :fail priority :priority closed :closed} issue]
                         [:span {:class (join " "
                                              (remove nil?
                                                      ["issue"
                                                       (when pass "pass")
                                                       (when fail "fail")
                                                       (when closed "closed")
                                                       (when priority (str "priority_" priority))]))}
                          "[" [:a {:href (str "https://github.com/snaekobbi/sprints/issues/" number)
                                   :target "_blank"}] "]"]))])
                  [:td (case state
                         :todo "To do"
                         :planned (str "Planned for sprint " (:sprint (first issues)))
                         :doing "Under development"
                         :ready-for-test "Ready for test"
                         :on-hold "On hold"
                         :testing "Under test"
                         :done "Done"
                         :cancelled "Cancelled")]])]]]])))))

(defn view []
  (get-features draw))
