(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.events :as ev]))

;;;; Library
;;;; ===============================================================================================

(defn- set-parents! [vdom]
  (let [children (.-childNodes vdom)]
    (when-not (undefined? children)
      (dotimes [i (alength children)]
        (let [child (aget children i)]
          (set! (.-parentNode child) vdom)
          (set-parents! child))))))

(defn- materialize-node! [vdom]
  (let [dom (if (= (.-nodeName vdom) "#text")
              (.createTextNode js/document (.-nodeValue vdom))
              (.createElement js/document (.-nodeName vdom)))]
    (obj/forEach vdom (fn [v k _]
                        (case k
                          ("dom" "parentNode" "childNodes" "nodeName") nil

                          "style" (obj/forEach v (fn [v k _] (aset dom "style" k v)))

                          (if (str/starts-with? k "on")
                            (ev/listen dom (subs k 2) v)
                            (aset dom k v)))))
    (set! (.-dom vdom) dom)
    dom))

(defn materialize! [vdom]
  (let [dom (materialize-node! vdom)]
    (let [children (.-childNodes vdom)]
      (when-not (undefined? children)
        (dotimes [i (alength children)]
          (let [child-dom (materialize! (aget children i))]
            (.appendChild dom child-dom)))))
    dom))

(defprotocol DOMDelta
  (apply-delta! [self]))

(deftype AppendChild [parent child]
  DOMDelta
  (apply-delta! [_] (.appendChild (.-dom parent) (.-dom child))))

(deftype RemoveChild [parent child]
  DOMDelta
  (apply-delta! [_] (.removeChild (.-dom parent) (.-dom child))))

(deftype ReplaceChild [parent new-child old-child]
  DOMDelta
  (apply-delta! [_] (.replaceChild (.-dom parent) (.-dom new-child) (.-dom old-child))))

(deftype SetProperty [node property value]
  DOMDelta
  (apply-delta! [_] (aset (.-dom node) property value)))

(deftype SetCSSProperty [node property value]
  DOMDelta
  (apply-delta! [_] (aset (.-dom node) "style" property value)))

(deftype SetEventListener [node property prev-value value]
  DOMDelta
  (apply-delta! [_]
    (when (and prev-value (not (undefined? prev-value)))
      (ev/unlisten (.-dom node) property prev-value))
    (ev/listen (.-dom node) property value)))

(declare diff-subtrees!)

;; TODO: Components
;; OPTIMIZE: Should we set .dom in `apply-delta!` instead?
(defn- diff! [deltas prev-vdom new-vdom]
  (cond
    (undefined? prev-vdom) (do (materialize! new-vdom)
                               (.push deltas (AppendChild. (.-parentNode prev-vdom) new-vdom)))

    (undefined? new-vdom) (do (set! (.-dom new-vdom) (.-dom prev-vdom))
                              (.push deltas (RemoveChild. (.-parentNode prev-vdom) new-vdom)))

    (not= (.-nodeName prev-vdom) (.-nodeName new-vdom))
    (do (materialize! new-vdom)
        (.push deltas (ReplaceChild. (.-parentNode prev-vdom) new-vdom prev-vdom)))

    :else (do (set! (.-dom new-vdom) (.-dom prev-vdom))
              (diff-subtrees! deltas prev-vdom new-vdom))))

(declare diff-attributes! diff-children!)

(defn- diff-subtrees! [deltas prev-vdom new-vdom]
  (diff-attributes! deltas prev-vdom new-vdom)
  (diff-children! deltas prev-vdom new-vdom))

(defn diff-attributes! [deltas prev-vdom new-vdom]
  (obj/forEach new-vdom (fn [v k _]
                          (case k
                            ("dom" "parentNode" "childNodes" "nodeName") nil

                            "style"
                            (obj/forEach v (fn [v k _]
                                             (when-not (= v (aget (.-style prev-vdom) k))
                                               (.push deltas (SetCSSProperty. prev-vdom k v)))))

                            (when-not (= v (aget prev-vdom k))
                              (if (str/starts-with? k "on")
                                (.push deltas (SetEventListener. prev-vdom (subs k 2)
                                                                 (aget prev-vdom k) v))         
                                (.push deltas (SetProperty. prev-vdom k v))))))))

(defn- diff-children! [deltas prev-vdom new-vdom]
  (when-not (= (.-nodeName prev-vdom) "#text")
    (let [prev-children (.-childNodes prev-vdom)
          new-children (.-childNodes new-vdom)]
      (dotimes [i (max (alength prev-children) (alength new-children))]
        (diff! deltas (aget prev-children i) (aget new-children i))))))

(defn- diff [prev-vdom new-vdom]
  (let [deltas (array)]
    (diff! deltas prev-vdom new-vdom)
    deltas))

(defn- commit-diff! [deltas]
  (dotimes [i (alength deltas)]
    (apply-delta! (aget deltas i))))

;;;; Element Factories
;;;; -----------------------------------------------------------------------------------------------

(defn element-node [tag props children]
  (set! (.-nodeName props) tag)
  (set! (.-childNodes props) children)
  props)

(defn text-node [text]
  #js {"nodeName"  "#text"
       "nodeValue" text})

;;;; Demo App
;;;; ===============================================================================================

(defn counter-view [v]
  (element-node "DIV" #js {"style" #js {"color" "red"}} #js [(text-node (str v))]))

(defn ui [state v click-handler]
  (element-node "DIV" #js {} #js [(counter-view v)
                                  (element-node "INPUT"
                                                #js {"type"    "button"
                                                     "onclick" click-handler
                                                     "value"   "Click me."}
                                                #js [])]))

(defn main []
  (let [state (atom 0)
        on-click (fn [_] (swap! state inc))
        vdom-root (atom (doto (ui state @state on-click)
                          (set-parents!)
                          (materialize!)))]
    (add-watch state nil (fn [_ state _ v]
                           (let [vdom (ui state v on-click)]
                             (set-parents! vdom)
                             (let [deltas (diff @vdom-root vdom)]
                               (println deltas)
                               (commit-diff! deltas)
                               (reset! vdom-root vdom)))))
    (.appendChild (.getElementById js/document "app-root") (.-dom @vdom-root))))

(main)
