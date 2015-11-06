(ns app.util)

(defn elems-by-tag [name]
  (.getElementsByTagName js/document name))

(defn elem-by-id [id]
  (.getElementById js/document id))

(defn remove-elem [elem]
  (.removeChild (.-parentNode elem) elem))

(extend-type js/HTMLCollection
  ISeqable
  (-seq [array] (array-seq array 0)))
