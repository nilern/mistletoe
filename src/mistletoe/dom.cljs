(ns mistletoe.dom)

(defn el [tag & args]
  (let [props #js {"nodeName" (if (keyword? tag) (name tag) tag)}]
    (loop [[k v & args* :as args] args]
      (if (and (seq args) (keyword? k))
        (do (if (= k :style)
              (set! (.-style props) (clj->js v))
              (aset props (name k) v))
            (recur args*))
        (do (set! (.-childNodes props) (into-array (flatten args)))
            props)))))

;; TODO: Make this unnecessary:
(defn text-node [text]
  #js {"nodeName"  "#text"
       "nodeValue" text})

