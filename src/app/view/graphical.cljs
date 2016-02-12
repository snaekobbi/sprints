(ns app.view.graphical
  (:require [app.github :as gh])
  (:use [app.util :only [elems-by-tag elem-by-id remove-elem]]
        [clojure.string :only [join split]] ;; why sometimes not working?
        [jayq.core :only [$]]
        ))

(defn get-task [id tasks]
  (first (filter #(= (:id %) id) tasks)))

(defn is-milestone? [task]
  (contains? task :time))

;; (defn join [separator coll]
;;   (loop [acc (str (first coll))
;;          more (next coll)]
;;     (if more
;;       (recur (str acc separator (str (first more)))
;;              (next more))
;;       acc)))

(def bg-todo {:light "#eee" :bright "#bbb" :dark "#707070"})
(def bg-done {:light "#dfd" :bright "#7d0" :dark "#24b224"})
(def bg-ready {:light "#ffd" :bright "#fd0" :dark "#d6ba00"})
(def bg-cancelled {:light "#ffdddd" :bright "#ff7a7a" :dark "#cd8989"})

(defn bg-color
  ([task] (bg-color :light task))
  ([flavor task]
     (if (:done task)
       (bg-done flavor)
       (if (:cancelled task)
         (bg-cancelled flavor)
         (if (:ready task)
           (bg-ready flavor)
           (bg-todo flavor))))))

;; TODO detect circular deps!
;; TODO detect duplicate ids
;; TODO detect tasks without :after
;; TODO detect tasks and with no :before or next task 
(defn draw [tasks]
  (let [now (.now js/Date)
        div-top (elem-by-id "top")
        div-mid (elem-by-id "middle")
        div-bottom (elem-by-id "bottom")]
    (set! (.-innerHTML div-top) "")
    (set! (.-innerHTML div-mid) "")
    (set! (.-innerHTML div-bottom) "")
    (let [{milestones true tasks false} (group-by is-milestone? tasks)
          milestones
          (for [milestone milestones]
            (assoc milestone
              :done (and (> now (:time milestone))
                         (every? #(or (:done %) (:cancelled %)) (filter #(= (:id milestone) (:before %)) tasks)))))
          tasks
          (for [task tasks]
            (assoc task
              :ready (every? #(or (:done %) (:cancelled %)) (map #(get-task % (concat tasks milestones)) (:after task)))))
          paths ;; milestone to milestone paths
          (loop [paths (apply concat
                              (for [milestone milestones]
                                (for [task (filter #(= (:id milestone) (:before %)) tasks)]
                                  [task milestone])))]
            (let [{complete-paths true incomplete-paths false}
                  (group-by #(and (> (count %) 1) (is-milestone? (first %))) paths)]
              (if (empty? incomplete-paths)
                complete-paths
                (recur
                 (concat complete-paths
                         (apply concat
                                (for [path incomplete-paths]
                                  (for [t (:after (first path))]
                                    (cons (get-task t (concat tasks milestones)) path)))))))))
          paths (map #(map :id %) paths)
          tasks
          (for [task tasks]
            (let [id (:id task)
                  longest-path (first (sort-by count > (filter #(some #{id} %) paths)))
                  length (- (count longest-path) 2)
                  index (- (count (take-while (partial not= id) longest-path)) 1)
                  begin (- (:time (get-task (first longest-path) milestones)) now)
                  end (- (:time (get-task (last longest-path) milestones)) now)]
              (assoc task
                :begin (+ begin (* (- end begin) (/ index length)))
                :end (+ begin (* (- end begin) (/ (+ index 1) length))))))
          tasks (sort (fn [x y]
                        (if (< (:begin x) (:begin y))
                          true
                          (if (> (:begin x) (:begin y))
                            false
                            (< (:end x) (:end y)))))
                      tasks)
          div-width (.-offsetWidth div-top)
          div-top-height (.-offsetHeight div-top)
          div-mid-height (.-offsetHeight div-mid)
          time-scale 3500000
          box-height 20
          height (* (count tasks) box-height)
          width (/ (- (apply max (map :end tasks)) (apply min (map :begin tasks))) time-scale)
          height (max height div-mid-height)
          width (max width div-width)
          milestones
          (for [milestone milestones]
            (assoc milestone
              :x (+ (/ div-width 2) (/ (- (:time milestone) now) time-scale))))
          tasks
          (let [i (atom -1)]
            (for [task tasks]
              (let [end (:end task)
                    begin (:begin task)]
                (swap! i inc)
                (assoc task
                  :x (+ (/ div-width 2) (/ begin time-scale))
                  :y (* @i box-height)
                  :width (/ (- end begin) time-scale)
                  :height box-height))))
          paper-top (js/Raphael div-top width div-top-height)
          paper-middle (js/Raphael div-mid width height)
          viewbox-x (atom 0)
          viewbox-y (atom 0)]
      (.setViewBox paper-top @viewbox-x 0 width div-top-height)
      (.setViewBox paper-middle @viewbox-x @viewbox-y width height)
      (.attr (.path paper-middle (str "M" (/ div-width 2) "," 0
                                      "L" (/ div-width 2) "," height))
             (clj->js {:stroke-width 2}))
      (let [first-day (doto (js/Date. (apply min (map :time milestones)))
                        (.setHours 0 0 0 0))
            days (js/Math.ceil (/ (- (apply max (map :time milestones)) first-day) 86400000))
            get-week (fn [date]
                       (let [nearest-thursday
                             (doto (js/Date. date)
                               (.setDate (+ (.getDate first-day) (- (mod (- 7 (.getDay first-day)) 7) 3))))
                             begin-of-year (js/Date. (.getFullYear nearest-thursday) 0 1)]
                         (js/Math.ceil (/ (+ (/ (- nearest-thursday begin-of-year) 86400000) 1) 7))))
            inc-day #(doto % (.setHours (+ (.getHours %) 24)))
            day-width (/ 86400000 time-scale)]
        (loop [day first-day
               days-left days]
          (let [week (get-week day)
                day-of-week (.getDay day)]
          (when (> days-left 0)
            (let [day-x (+ (/ div-width 2) (/ (- day now) time-scale))
                  rect (.rect paper-top
                              day-x (- div-top-height 22)
                              day-width 21)]
              (.attr rect (clj->js {:stroke "#555" :stroke-width 0.5
                                    :fill (if (#{6 0} day-of-week)
                                            "url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAQAAAAEBAMAAABb34NNAAAAFVBMVEX6+vr/AADd7h7d7h7d7h7d7h7/AAB/wwCsAAAAEklEQVQImWNIYGBgYGNgSAASAAXiAM1TeI9fAAAAAElFTkSuQmCC)"
                                            "#fff")}))
              (when (not (#{6 0} day-of-week))
                (.attr (.text paper-top (+ day-x 3) (- div-top-height 12)
                              (str week "." day-of-week))
                       (clj->js {:text-anchor "start"
                                 :font-size "9px"
                                 :font-family "Tahoma"}))))
            (recur (inc-day day)
                   (dec days-left))))))
      (let [milestones
            (loop [acc nil
                   milestones milestones]
              (if-let [milestone (first milestones)]
                (recur
                 (cons (assoc milestone
                         :path
                         (let [{x :x} milestone]
                           (.attr
                            (.path paper-middle (str "M" x "," 0
                                                     "L" x "," height))
                            (clj->js {:stroke "#ccc"
                                      :stroke-width 0.5}))
                           (.attr
                            (.path paper-top (str "M" x "," (- div-top-height 25)
                                                  "L" (- x 10) "," (- div-top-height 35)
                                                  "L" (- x 10) "," 1
                                                  "L" (+ x 10) "," 1
                                                  "L" (+ x 10) "," (- div-top-height 35)
                                                  "Z"))
                            (clj->js {:fill (bg-color milestone)
                                      :stroke "#ffffff"}))))
                       acc)
                 (rest milestones))
                acc))
            tasks
            (loop [acc nil
                   tasks tasks]
              (if-let [task (first tasks)]
                (recur
                 (cons (assoc task
                         :rect
                         (let [{x :x y :y w :width h :height} task
                               rect (.attr (.rect paper-middle x y w h)
                                           (clj->js {:fill (bg-color task)
                                                     :stroke "#ffffff"
                                                     :title (:desc task)}))]
                           ;; FIXME make text unselectable!
                           (.attr (.paragraph paper-middle (clj->js {:x (+ x 5)
                                                                     :y (+ y 10)
                                                                     :maxWidth (- w 5 5)
                                                                     :maxHeight h
                                                                     :text (:desc task)}))
                                  (clj->js {:text-anchor "start"
                                            :clip-rect (join " " [x y (- w 5) h])
                                            :font-size "10px"
                                            :font-family "Tahoma"}))
                           (when-let [assignees (not-empty (:assignees task))]
                             (.attr (.text paper-middle (+ x w 5) (+ y 10)
                                           (join ", " (map #(str "@" (name %)) assignees)))
                                    (clj->js {:text-anchor "start"
                                              :font-size "10px"
                                              :font-family "Tahoma"})))
                           rect))
                       acc)
                 (rest tasks))
                acc))]
        (letfn [(select-task [task]
                  (doall (map #(.attr (:rect %) (clj->js {:fill (bg-color %) :stroke "#ffffff"})) tasks))
                  (doall (map #(.attr (:path %) (clj->js {:fill (bg-color %) :stroke "#ffffff"})) milestones))
                  (.attr (:rect task) (clj->js {:fill (bg-color :bright task)
                                                :stroke "#ffffff"}))
                  (let [html (str "<h2>" (:desc task) "</h2>")
                        html (if-let [issues (not-empty (:issues task))]
                               (str html
                                    "More info:<ul>"
                                    (join "\n"
                                          (for [issue issues]
                                            (let [[_ _ _ owner repo _ number] (split issue #"/")]
                                              (str "<li><a class='ghi' href='" issue "' target='_blank' "
                                                   "owner='" owner "' repo='" repo "' number='" number "'>"
                                                   issue
                                                   "</a></li>"))))
                                    "</ul>")
                               html)]
                    (let [div-bottom ($ div-bottom)]
                      (.html div-bottom html)
                      (.each (.find div-bottom "a.ghi")
                             (fn [] (this-as this
                                             (let [this ($ this)
                                                   owner (.attr this "owner")
                                                   repo (.attr this "repo")
                                                   number (.attr this "number")]
                                               (gh/issue owner repo number
                                                         #(do (.text this (str "[" owner "/" repo "] " (:title %)))
                                                              (when-let [assignee (:assignee %)]
                                                                (.after this (str " (" assignee ")")))))))))))
                  (reset! viewbox-x (+ (:x task) (/ (- (:width task) div-width) 2)))
                  (reset! viewbox-y (+ (:y task) (/ (- (:height task) div-mid-height) 2)))
                  (.animateViewBox paper-middle
                                   @viewbox-x
                                   @viewbox-y
                                   width
                                   height
                                   700
                                   "<>")
                  (.animateViewBox paper-top
                                   @viewbox-x
                                   0
                                   width
                                   div-top-height
                                   700
                                   "<>")
                  (doall
                   (map #(if (is-milestone? %)
                           (.attr (:path %) (clj->js {:fill (bg-color :bright %) :stroke "#ffffff"}))
                           (.attr (:rect %) (clj->js {:fill (bg-color :bright %) :stroke "#ffffff"})))
                        (concat (map #(get-task % (concat tasks milestones)) (:after task))
                                (filter #(some #{(:id task)} (:after %)) tasks)
                                (map #(get-task % milestones) (remove nil? [(:before task)]))))))
                (select-milestone [milestone]
                  (doall (map #(.attr (:rect %) (clj->js {:fill (bg-color %) :stroke "#ffffff"})) tasks))
                  (doall (map #(.attr (:path %) (clj->js {:fill (bg-color %) :stroke "#ffffff"})) milestones))
                  (.attr (:path milestone) (clj->js {:fill (bg-color :bright milestone)
                                                     :stroke "#ffffff"}))
                  (set! (.-innerHTML div-bottom) (str "<h2>" (:desc milestone) "</h2>"
                                                      "Due date: " (:time milestone)))
                  (doall
                   (map #(if (is-milestone? %)
                           (.attr (:path %) (clj->js {:fill (bg-color :bright %) :stroke "#ffffff"}))
                           (.attr (:rect %) (clj->js {:fill (bg-color :bright %) :stroke "#ffffff"})))
                        (concat (filter #(or (some #{(:id milestone)} (:after %))
                                             (= (:id milestone) (:before %)))
                                        tasks)))))]
          (doall
           (for [task tasks]
             (set! (.-onclick (.-node (:rect task)))
                   ;; #(select-task task)
                   #(set! (.-hash (.-location js/window)) (str "#" (:id task)))
                   )))
          (doall
           (for [milestone milestones]
             (set! (.-onclick (.-node (:path milestone)))
                   ;; #(select-milestone milestone)
                   #(set! (.-hash (.-location js/window)) (str "#" (:id milestone)))
                   )))
          (letfn [(select-task-or-milestone [id]
                    (when-let [task-or-milestone (get-task id (concat tasks milestones))]
                      (if (is-milestone? task-or-milestone)
                        (select-milestone task-or-milestone)
                        (select-task task-or-milestone))))]
            ;; (if (.hasOwnProperty js/window "onhashchange")
            ;;   (set! (.-onhashchange (.-node (:path milestone))) #(...))
            (let [hash (atom "")]
              (.setInterval js/window
                            #(when-let [[_ new-hash] (re-matches #"#(.+)" (.-hash (.-location js/window)))]
                               (when (not (= new-hash @hash))
                                 (reset! hash new-hash)
                                 (select-task-or-milestone new-hash)))
                            100)))
          (let [mousedown (atom false)
                page-x (atom)
                page-y (atom)]
            (-> ($ div-mid)
                (.mousedown #(do (reset! mousedown true)
                                 (reset! page-x (.-pageX %))
                                 (reset! page-y (.-pageY %))))
                (.mousemove #(when @mousedown
                               (.setViewBox paper-middle
                                            (+ @viewbox-x (- @page-x (.-pageX %)))
                                            (+ @viewbox-y (- @page-y (.-pageY %)))
                                            width
                                            height
                                            false)
                               (.setViewBox paper-top
                                            (+ @viewbox-x (- @page-x (.-pageX %)))
                                            0
                                            width
                                            div-top-height
                                            false)))
                (.mouseup #(do (reset! mousedown false)
                               (reset! viewbox-x (+ @viewbox-x (- @page-x (.-pageX %))))
                               (reset! viewbox-y (+ @viewbox-y (- @page-y (.-pageY %))))))))
          nil)))))
