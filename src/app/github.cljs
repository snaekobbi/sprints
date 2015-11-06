(ns app.github
  (:require [jayq.core :as $]))

(defn issue [owner repo number callback]
  ($/ajax (str "https://api.github.com/repos/" owner "/" repo "/issues/" number)
          {:success (fn [data]
                      (let [data (js->clj data)]
                        (callback
                         {:title (get data "title")
                          :assignee (get-in data ["assignee" "login"])})))})
  nil)

(defn re-find-all [re s]
  (let [re (js/RegExp. (.-source re) "g")]
    (loop [acc []]
      (if-let [m (.exec re s)]
        (recur (conj acc (first m)))
        acc))))

(defn extract-links [body]
  (re-find-all #"https://github\.com/[^/]+/[^/]+/issues/[0-9]+" body))

(defn issue-referenced-by [owner repo number callback]
  ($/ajax (str "https://api.github.com/repos/" owner "/" repo "/issues/" number)
          {:success (fn [data]
                      (let [data (js->clj data)
                            links (into #{} (extract-links (get data "body")))]
                        (when (not (empty? links))
                          (callback links))))})
  ($/ajax (str "https://api.github.com/repos/" owner "/" repo "/issues/" number "/comments")
          {:success (fn [data]
                      (let [data (js->clj data)
                            links (into #{} (apply concat (map #(extract-links (get % "body")) data)))]
                        (when (not (empty? links))
                          (callback links))))})
  nil)


;; (issue "snaekobbi" "sprints" "135" #(println %))
;; (issue "snaekobbi" "requirements" "42" #(println %))
;; (issue-referenced-by "snaekobbi" "requirements" "42" #(println %))
