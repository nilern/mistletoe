(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.events :as ev]))

;;;; Library
;;;; ===============================================================================================

(defprotocol Render
  (render [self vdom]))

(defn- materialize-element! [vdom parent-dom]
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
    (when parent-dom
      (.appendChild parent-dom dom))
    dom))

(defn materialize! [vdom parent-dom]
  (let [node-name (.-nodeName vdom)]
    (if (string? node-name)
      (let [dom (materialize-element! vdom parent-dom)]
        (let [children (.-childNodes vdom)]
          (when-not (undefined? children)
            (dotimes [i (alength children)]
              (materialize! (aget children i) dom)))))
      (let [component (node-name vdom)
            child (render component vdom)]
        (set! (.-component vdom) component)
        (set! (.-childNode vdom) child)
        (materialize! child parent-dom)))))

(defn- set-parents! [vdom]
  (if (string? (.-nodeName vdom))
    (let [children (.-childNodes vdom)]
      (when-not (undefined? children)
        (dotimes [i (alength children)]
          (let [child (aget children i)]
            (set! (.-parentNode child) vdom)
            (set-parents! child)))))
    (let [child (.-childNode vdom)]
      (set! (.-parentNode child) vdom)
      (set-parents! child))))

(defprotocol DOMDelta
  (apply-delta! [self]))

(deftype AppendChild [parent-dom child]
  DOMDelta
  (apply-delta! [_] (.appendChild parent-dom (.-dom child))))

(deftype RemoveChild [parent-dom child]
  DOMDelta
  (apply-delta! [_] (.removeChild parent-dom (.-dom child))))

(deftype ReplaceChild [parent-dom new-child old-child]
  DOMDelta
  (apply-delta! [_] (.replaceChild parent-dom (.-dom new-child) (.-dom old-child))))

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

(defn- diff! [deltas parent-dom prev-vdom new-vdom]
  (cond
    (undefined? prev-vdom) (do (materialize! new-vdom parent-dom)
                               (.push deltas (AppendChild. parent-dom new-vdom))
                               new-vdom)

    (undefined? new-vdom) (do (set! (.-dom new-vdom) (.-dom prev-vdom))
                              (.push deltas (RemoveChild. parent-dom new-vdom))
                              nil)

    (not= (.-nodeName prev-vdom) (.-nodeName new-vdom))
    (do (materialize! new-vdom parent-dom)
        (.push deltas (ReplaceChild. parent-dom new-vdom prev-vdom))
        new-vdom)

    (string? (.-nodeName prev-vdom)) (do (set! (.-dom new-vdom) (.-dom prev-vdom))
                                         (diff-subtrees! deltas (.-dom prev-vdom) prev-vdom new-vdom)
                                         prev-vdom)

    :else (let [element (.-childNode prev-vdom)
                element* (render (.-component prev-vdom) new-vdom)]
            (set! (.-childNode prev-vdom) element*)
            (diff! deltas parent-dom element element*)
            prev-vdom)))

(declare diff-attributes! diff-children!)

(defn- diff-subtrees! [deltas dom prev-vdom new-vdom]
  (diff-attributes! deltas prev-vdom new-vdom)
  (diff-children! deltas dom prev-vdom new-vdom))

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

(defn- diff-children! [deltas dom prev-vdom new-vdom]
  (if (string? (.-nodeName prev-vdom))
    (when-not (= (.-nodeName prev-vdom) "#text")
      (let [prev-children (.-childNodes prev-vdom)
            new-children (.-childNodes new-vdom)]
        (dotimes [i (max (alength prev-children) (alength new-children))]
          (diff! deltas dom (aget prev-children i) (aget new-children i)))))
    (diff! deltas dom (.-childNode prev-vdom) (.-childNode new-vdom))))

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

(defn counter-view [vdom]
  (reify Render
    (render [_ vdom]
      (element-node "DIV" #js {"style" #js {"color" "red"}}
                          #js [(text-node (str (.-v vdom)))]))))

(defn ui [state v click-handler]
  (element-node "DIV" #js {} #js [(element-node counter-view #js {:v v} #js [])
                                  (element-node "INPUT"
                                                #js {"type"    "button"
                                                     "onclick" click-handler
                                                     "value"   "Click me."}
                                                #js [])]))

(defn main []
  (let [state (atom 0)
        on-click (fn [_] (swap! state inc))
        vdom-root (atom (doto (ui state @state on-click)
                          (materialize! nil)
                          (set-parents!)))
        container (.getElementById js/document "app-root")
        _ (.appendChild container (.-dom @vdom-root))]
    (add-watch state nil (fn [_ state _ v]
                           (let [vdom (ui state v on-click)]
                             (let [deltas (array)
                                   vdom (diff! deltas container @vdom-root vdom)]
                               (set-parents! vdom)
                               (.log js/console deltas)
                               (commit-diff! deltas)
                               (reset! vdom-root vdom)))))))

(main)
