(ns app.view.graphical
  (:require [app.github :as gh])
  (:use [app.util :only [elems-by-tag elem-by-id remove-elem]]
        [clojure.string :only [join]] ;; why sometimes not working
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

(def bg-todo "#eee")
(def bg-todo-selected "#aaa")
(def bg-done "#dfd")
(def bg-done-selected "#7d0")
(def bg-ready "#ffd")
(def bg-ready-selected "#fd0")

(defn bg-color [task]
  (if (:done task)
    bg-done
    (if (:ready task)
      bg-ready
      bg-todo)))

(defn bg-color-selected [task]
  (if (:done task)
    bg-done-selected
    (if (:ready task)
      bg-ready-selected
      bg-todo-selected)))

;; TODO detect circular deps!
;; TODO detect duplicate ids
;; TODO detect tasks without :after
;; TODO detect tasks and with no :before or next task 
(defn draw [tasks]
  (let [now (.now js/Date)
        div1 (elem-by-id "top")
        div2 (elem-by-id "middle")
        div3 (elem-by-id "bottom")]
    (set! (.-innerHTML div1) "")
    (set! (.-innerHTML div2) "")
    (set! (.-innerHTML div3) "")
  (let [{milestones true tasks false} (group-by is-milestone? tasks)
        milestones
        (for [milestone milestones]
          (assoc milestone
            :done (and (> now (:time milestone))
                       (every? :done (filter #(= (:id milestone) (:before %)) tasks)))))
        tasks
        (for [task tasks]
          (assoc task
            :ready (every? :done (map #(get-task % (concat tasks milestones)) (:after task)))))
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
                start (- (:time (get-task (first longest-path) milestones)) now)
                end (- (:time (get-task (last longest-path) milestones)) now)]
            (assoc task
              :begin (+ start (* (- end start) (/ index length)))
              :end (+ start (* (- end start) (/ (+ index 1) length))))))
        ;; sorting doesn't work?
        tasks (sort (fn [x y]
                      (if (< (:start x) (:start y))
                        true
                        (if (> (:start x) (:start y))
                          false
                          (< (:end x) (:end y)))))
                    tasks)
        div-width (.-offsetWidth div1)
        div1-height (.-offsetHeight div1)
        div2-height (.-offsetHeight div2)
        time-scale 2700000
        box-height 30
        height (* (count tasks) box-height)
        width (/ (- (apply max (map :end tasks)) (apply min (map :begin tasks))) time-scale)
        height (max height div2-height)
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
        paper1 (js/Raphael div1 width div1-height)
        paper2 (js/Raphael div2 width height)]
    (.setViewBox paper1 0 0 width div1-height)
    (.setViewBox paper2 0 0 width height)
    (.attr (.path paper2 (str "M" (/ div-width 2) "," 0
                              "L" (/ div-width 2) "," div2-height))
           (clj->js {:stroke-width 2}))
    (let [milestones
          (loop [acc nil
                 milestones milestones]
            (if-let [milestone (first milestones)]
              (recur
               (cons (assoc milestone
                       :path
                       (let [{x :x} milestone]
                         (.attr
                          (.path paper2 (str "M" x "," 0
                                             "L" x "," div2-height))
                          (clj->js {:stroke "#666"
                                    :stroke-dasharray "-"}))
                         (.attr
                          (.path paper1 (str "M" x "," div1-height
                                             "L" (- x 10) "," (- div1-height 10)
                                             "L" (- x 10) "," 1
                                             "L" (+ x 10) "," 1
                                             "L" (+ x 10) "," (- div1-height 10)
                                             "Z"))
                          (clj->js {:fill (bg-color milestone)}))))
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
                             rect (.attr (.rect paper2 x y w h)
                                         (clj->js {:fill (bg-color task)
                                                   :stroke "#666"
                                                   :title (:desc task)}))]
                         ;; FIXME make text unselectable!
                         (.attr (.paragraph paper2 (clj->js {:x (+ x 5)
                                                             :y (+ y 10)
                                                             :maxWidth (- w 5 5)
                                                             :maxHeight h
                                                             :text (:desc task)}))
                                (clj->js {:text-anchor "start"
                                          :clip-rect (join " " [x y (- w 5) h])
                                          :font-size "10px"}))
                         (when-let [assignees (not-empty (:assignees task))]
                           (.attr (.text paper2 (+ x w 5) (+ y 10)
                                         (join ", " (map #(str "@" (name %)) assignees)))
                                  (clj->js {:text-anchor "start"
                                            :font-size "10px"})))
                         rect))
                     acc)
               (rest tasks))
              acc))]
      (doall
       (for [task tasks]
         (set! (.-onclick (.-node (:rect task)))
               (fn []
                 (doall (map #(.attr (:rect %) (clj->js {:fill (bg-color %)})) tasks))
                 (doall (map #(.attr (:path %) (clj->js {:fill (bg-color %)})) milestones))
                 (.attr (:rect task) (clj->js {:fill (bg-color-selected task)}))
                 (let [html (str "<h2>" (:desc task) "</h2>")
                       html (if-let [issues (not-empty (:issues task))]
                              (str html
                                   "More info:<ul>"
                                   (join "\n"
                                         (for [issue issues]
                                           (str "<li><a href='" issue "' target='_blank'>" issue "</a></li>")))
                                   "</ul>")
                              html)]
                   (set! (.-innerHTML div3) html))
                 (.animateViewBox paper1
                                  (+ (:x task) (/ (- (:width task) div-width) 2))
                                  0
                                  width
                                  div1-height
                                  700
                                  "<>")
                 (.animateViewBox paper2
                                  (+ (:x task) (/ (- (:width task) div-width) 2))
                                  (+ (:y task) (/ (- (:height task) div2-height) 2))
                                  width
                                  height
                                  700
                                  "<>")
                 (doall
                  (map #(if (is-milestone? %)
                          (.attr (:path %) (clj->js {:fill (bg-color-selected %)}))
                          (.attr (:rect %) (clj->js {:fill (bg-color-selected %)})))
                       (concat (map #(get-task % (concat tasks milestones)) (:after task))
                               (filter #(some #{(:id task)} (:after %)) tasks)
                               (map #(get-task % milestones) (remove nil? [(:before task)])))))))))
      (doall
       (for [milestone milestones]
         (set! (.-onclick (.-node (:path milestone)))
               (fn []
                 (doall (map #(.attr (:rect %) (clj->js {:fill (bg-color %)})) tasks))
                 (doall (map #(.attr (:path %) (clj->js {:fill (bg-color %)})) milestones))
                 (.attr (:path milestone) (clj->js {:fill (bg-color-selected milestone)}))
                 (set! (.-innerHTML div3) (str "<h2>" (:desc milestone) "</h2>"
                                               "Due date: " (:time milestone)))
                 (doall
                  (map #(if (is-milestone? %)
                          (.attr (:path %) (clj->js {:fill (bg-color-selected %)}))
                          (.attr (:rect %) (clj->js {:fill (bg-color-selected %)})))
                       (concat (filter #(or (some #{(:id milestone)} (:after %))
                                            (= (:id milestone) (:before %)))
                                       tasks))))))))
      nil))))
