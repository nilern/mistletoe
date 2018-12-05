(ns mistletoe.node
  (:require [goog.object :as obj]
            [clojure.string :as str]
            [mistletoe.component :as component]
            [mistletoe.updates :refer [request-init-property! request-init-style-property!
                                       request-init-event-handler!]]))

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

;;;; ## Initializing Parent Pointers

(defn set-parents! [vdom]
  (if (string? (.-nodeName vdom))
    (let [children (.-childNodes vdom)]
      (when-not (undefined? children)
        (dotimes [i (alength children)]
          (let [child (aget children i)]
            (set! (.-parentNode child) vdom)
            (set-parents! child)))))
    (let [child (.-childNode vdom)]
      (when-not (undefined? child)
        (set! (.-parentNode child) vdom)
        (set-parents! child)))))

;;;; # Hiccup -> VDOM node

(defprotocol VDOMNode
  (get-dom [self])
  (set-parent! [self parent]))

(deftype VDOMText [dom element ^:mutable parent]
  VDOMNode
  (get-dom [_] dom)
  (set-parent! [_ parent*]
    (.appendChild (get-dom parent) dom)
    (set! parent parent*)))

(deftype VDOMElement [dom element ^:mutable parent children]
  VDOMNode
  (get-dom [_] dom)
  (set-parent! [_ parent*]
    (.appendChild (get-dom parent) dom)
    (set! parent parent*)))

(deftype VDOMComponent [component ^:mutable element])

(defprotocol Element
  (->vdom [self deltas]))

(defprotocol VDOMFactory
  (create-vdom [self element deltas]))

(extend-protocol Element
  string
  (->vdom [s _] (->VDOMText (.createTextNode js/document s) s nil))

  PersistentVector
  (->vdom [[tag :as element] deltas] (create-vdom tag element deltas)))

(extend-protocol VDOMFactory
  Keyword
  (create-vdom [self [_ & args :as element] deltas]
    (let [dom (.createElement js/document (name self))
          [attrs children] (if (and (seq args) (map? (first args)))
                             [(first args) (rest args)]
                             [{} args])]
      ;; OPTIMIZE: If dependency can be resolved, use the resolved value immediately:
      (doseq [[k v] (first args)
            :let [kstr (name k)]]
        (cond
          (= k :style) (doseq [[k v] v]
                         (request-init-style-property! v (name k) dom deltas))
          (str/starts-with? kstr "on") (request-init-event-handler! v (subs kstr 2) dom deltas)
          :else (request-init-property! v kstr dom deltas)))

      (let [children (map ->vdom children)
            vdom (->VDOMElement dom element nil (into-array children))]
        (doseq [child children]
          (set-parent! child vdom))

        vdom)))


  function
  (create-vdom [self element deltas]
    (let [component (self element)
          element (component/render component element)
          element-vdom (->vdom element deltas)
          vdom (->VDOMComponent component element-vdom)]
      (set-parent! element-vdom (get-parent self))
      vdom)))

;;;; # Creating New DOM Nodes from VDOM nodes

;; OPTIMIZE: If dependency can be resolved, use the resolved value immediately:
(defn- materialize-element! [vdom parent-dom deltas]
  (let [dom (if (= (.-nodeName vdom) "#text")
              (.createTextNode js/document (.-nodeValue vdom))
              (.createElement js/document (.-nodeName vdom)))]
    (set! (.-dom vdom) dom)
    (when parent-dom
      (.appendChild parent-dom dom))
    (obj/forEach vdom (fn [v k _]
                        (case k
                          ("dom" "parentNode" "childNodes" "nodeName") nil

                          "style" (obj/forEach v (fn [v k _]
                                                   (request-init-style-property! v k vdom deltas)))

                          (if (str/starts-with? k "on")
                            (request-init-event-handler! v (subs k 2) vdom deltas)
                            (request-init-property! v k vdom deltas)))))
    dom))

(defn materialize!
  ([vdom deltas] (materialize! vdom nil deltas))
  ([vdom parent-dom deltas]
   (let [node-name (.-nodeName vdom)]
     (if (string? node-name)
       (let [dom (materialize-element! vdom parent-dom deltas)]
         (let [children (.-childNodes vdom)]
           (when-not (undefined? children)
             (dotimes [i (alength children)]
               (materialize! (aget children i) dom deltas)))))
       (let [component (node-name vdom)
             child (component/render component vdom)]
         (set! (.-component vdom) component)
         (set! (.-childNode vdom) child)
         (materialize! child parent-dom deltas))))))
