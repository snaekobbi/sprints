(ns app.view.text
  (:require [app.github :as gh])
  (:use [app.util :only [elems-by-tag elem-by-id remove-elem]]
        [clojure.string :only [join split]]
        [crate.core :only [html]]
        [jayq.core :only [$]]))

(defn get-task [id tasks]
  (first (filter #(= (:id %) id) tasks)))

(defn task-to-string [task]
  (concat (if (:time task) "Milestone: " "Task: ") (:desc task)))

(defn clear []
  (.remove ($ "#tasks")))
  
(defn draw [tasks]
  (clear)
  (.append
   ($ "body")
   ($ (html
       [:div#tasks
        [:ul
         (for [task tasks]
           [:li {:id (:id task)}
            [:h2 (task-to-string task)]
            (when-let [assignees (not-empty (:assignees task))]
              [:div "Assigned to: "
               [:b (join ", " assignees)]])
            (when-let [after (not-empty
                              (concat (map #(get-task % tasks) (:after task))
                                      (filter #(= (:id task) (:before %)) tasks)))]
              [:div "Comes after:"
               [:ul
                (for [task after]
                  [:li
                   [:a {:href (str "#" (:id task))}
                    (task-to-string task)]
                   (when-let [assignees (not-empty (:assignees task))]
                     `(" (" ~(join ", " assignees) ")"))])]])
            (when-let [before (not-empty
                               (concat (filter #(some #{(:id task)} (:after %)) tasks)
                                       (map #(get-task % tasks) (remove nil? [(:before task)]))))]
              [:div "Comes before:"
               [:ul
                (for [task before]
                  [:li
                   [:a {:href (str "#" (:id task))}
                    (task-to-string task)]
                   (when-let [assignees (not-empty (:assignees task))]
                     `(" (" ~(join ", " assignees) ")"))
                   (when-let [time (:time task)]
                     (str " (" time ")"))])]])
            (when-let [issues (not-empty (:issues task))]
              [:div "More info:"
               [:ul
                (for [issue issues]
                  [:li
                   (let [[_ _ _ owner repo _ number] (split issue #"/")]
                     [:a {:href issue
                          :target "_blank"
                          :class "ghi"
                          :owner owner
                          :repo repo
                          :number number}
                      issue])])]])])]])))
  (.each ($ "a.ghi")
   (fn [] (this-as this
     (let [this ($ this)
           owner (.attr this "owner")
           repo (.attr this "repo")
           number (.attr this "number")]
       (gh/issue owner repo number
                 #(do (.text this (str "[" owner "/" repo "] " (:title %)))
                      (when-let [assignee (:assignee %)]
                        (.after this (str " (" assignee ")")))))))))
  nil)

