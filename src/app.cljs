(ns app
  (:require [clojure.browser.repl :as repl]
            [app.view.text :as text-view]
            [app.view.graphical :as graphical-view]
            [jayq.core :as $]))

;; - on command line: lein cljsbuild once dev
;; - in emacs: M-x inferior-lisp
;; - or on command line: rlwrap lein trampoline cljsbuild repl-listen
;; - in browser: go to http://localhost:9000/test/index.html
;; - in browser console: app.repl_connect();
;; TODO: only for developing, so put in separate file?
;; TODO: creates directories "out" and "repl" -> move somewhere else?
;; TODO: fix error "GET http://localhost:9000/target/cljsbuild/dev/clojure/string.js: Not found"
;; TODO: how come i have to do "make clean && lein cljsbuild once dev" every time?
;; TODO: why is jquery/jayq causing trouble??
(defn repl-connect []
  (repl/connect "http://localhost:9000/repl"))

(defn read-data [callback]
  ($/ajax "https://raw.githubusercontent.com/snaekobbi/sprints/schedule-data/data.yaml"
          {:dataType "text"
           :success (fn [data]
                      (let [data (.load js/jsyaml data)
                            data (js->clj data)
                            data (map (fn [task] (into {} (map (fn [[k v]] [(keyword k) v]) task))) data)]
                        (callback data)))})
  nil)

(defn text-view []
  (read-data text-view/draw))

(defn graphical-view []
  (read-data graphical-view/draw))
