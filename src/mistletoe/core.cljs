(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.dom :as dom]
            [goog.events :as ev]))

;;;; Library
;;;; ===============================================================================================

;;;; Elements
;;;; -----------------------------------------------------------------------------------------------

(declare VDOMNode VDOMComponentNode VDOMTextNode)

(defprotocol Element
  (->VDOM [self parent])
  (update-dom-props! [self prev-props dom]))

(defprotocol Tag
  (instantiate [self element parent]))

(defprotocol Render
  (render [self]))

(defn element-type [element] (aget element 0))

(defn element-props [element] (aget element 1))

(defn public-instance [element] ((element-type element) element))

(extend-protocol Element
  string
  (->VDOM [self parent] (VDOMTextNode. self (dom/createTextNode self) parent))
  (update-dom-props! [self _ dom] (set! (.-nodeValue dom) self))

  array
  (->VDOM [self parent] (instantiate (element-type self) self parent))
  (update-dom-props! [self prev-props dom]
    (obj/forEach (aget self 1)
                 (fn [v k _]
                   (cond
                     (= k "style") (obj/forEach v (fn [v k _] (aset dom "style" k v)))
                     (str/starts-with? k "on") (let [ev-name (subs k 2)]
                                                 (when-let [prevv (aget prev-props k)]
                                                   (ev/unlisten dom ev-name prevv))
                                                 (ev/listen dom ev-name v))
                     :else (aset dom k v))))))

(extend-protocol Tag
  string
  (instantiate [self element parent]
    (let [dom (dom/createElement self)
          _ (update-dom-props! element #js {} dom)
          len (count element)
          inst-children (make-array (- len 2))
          instance (VDOMNode. element dom parent inst-children)]
      (loop [i 2]
        (when (< i len)
          (let [inst-child (->VDOM (aget element i) instance)]
            (aset inst-children (- i 2) inst-child)
            (dom/appendChild dom (.-dom inst-child))
            (recur (inc i)))))
      instance))

  function
  (instantiate [self element parent]
    (let [component-inst (public-instance element)
          child-elem (render component-inst)
          child-inst (->VDOM child-elem parent)]
      (VDOMComponentNode. element (.-dom child-inst) parent child-inst component-inst))))

;;;; Virtual DOM and Reconciliation
;;;; -----------------------------------------------------------------------------------------------

(declare reconcile reconcile-children)

(defprotocol VirtualDOMNode
  (reconcile-subnodes! [self element*]))

(deftype VDOMNode [^:mutable element dom parent ^:mutable children]
  VirtualDOMNode
  (reconcile-subnodes! [self element*]
    (update-dom-props! element* (element-props element) dom)
    (set! element element*)
    (set! children (reconcile-children self element*))))

(deftype VDOMTextNode [^:mutable element dom parent]
  VirtualDOMNode
  (reconcile-subnodes! [self element*]
    (update-dom-props! element* element dom)
    (set! element element*)))

(deftype VDOMComponentNode [^:mutable element ^:mutable dom parent ^:mutable child
                            ^:mutable component-inst]
  VirtualDOMNode
  (reconcile-subnodes! [self element*]
    (let [component-inst* (public-instance element*)
          child* (reconcile parent child (render component-inst*))]
      (set! element element*)
      (set! dom (.-dom child*))
      (set! child child*)
      (set! component-inst component-inst*))))

(defn reconcile [parent instance element]
  (cond
    (not instance) (let [instance* (->VDOM element parent)]
                     (dom/appendChild (.-dom parent) (.-dom instance*))
                     instance*)

    (not element) (do (dom/removeNode (.-dom instance))
                      nil)

    (not= (element-type (.-element instance)) (element-type element))
    (let [instance* (->VDOM element parent)]
      (dom/replaceNode (.-dom instance*) (.-dom instance))
      instance*)

    :else (do (reconcile-subnodes! instance element)
              instance)))

(defn reconcile-children [instance element]
  (let [inst-children (.-children instance)
        len (max (alength inst-children) (- (alength element) 2))
        inst-children* (make-array len)]
    (loop [i 0]
      (when (< i len)
        (some->> (reconcile instance (aget inst-children i) (aget element (+ i 2)))
                 (aset inst-children* i))
        (recur (inc i))))
    inst-children*))

;;;

(defn- set-parents! [vdom]
  (let [children (.-childNodes vdom)]
    (when-not (undefined? children)
      (dotimes [i (alength children)]
        (let [child (aget children i)]
          (set! (.-parent child) vdom)
          (set-parents! child))))))

(defn- materialize-node! [vdom]
  (let [dom (if (= (.-nodeName vdom) "#text")
              (.createTextNode js/document (.-nodeValue vdom))
              (.createElement js/document (.-nodeName vdom)))]
    (obj/forEach vdom (fn [v k _]
                        (case k
                          ("parentNode" "childNodes" "nodeName")
                          (assert false (str "setting " k " manually is forbidden"))

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
          (let [child-dom (materialize-node! (aget children i))]
            (.appendChild dom child-dom)))))))

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
(defn- diff! [deltas prev-vdom new-vdom]
  (cond
    (undefined? prev-vdom) (.push deltas (AppendChild. (.-parentNode prev-vdom) new-vdom))

    (undefined? new-vdom) (.push deltas (RemoveChild. (.-parentNode prev-vdom) new-vdom))

    (not= (.-nodeName prev-vdom) (.-nodeName new-vdom))
    (.push deltas (ReplaceChild. (.-parentNode prev-vdom) new-vdom prev-vdom))

    :else (diff-subtrees! deltas prev-vdom new-vdom)))

(declare diff-attributes! diff-children!)

(defn- diff-subtrees! [deltas prev-vdom new-vdom]
  (diff-attributes! deltas prev-vdom new-vdom)
  (diff-children! deltas prev-vdom new-vdom))

(defn diff-attributes! [deltas prev-vdom new-vdom]
  (obj/forEach new-vdom (fn [v k _]
                          (case k
                            ("parentNode" "childNodes" "nodeName") nil

                            "style" (obj/forEach v (fn [v k _] (.push deltas (SetCSSProperty. prev-vdom k v))))

                            (if (str/starts-with? k "on")
                              (.push deltas (SetEventListener. prev-vdom (subs k 2) (aget prev-vdom k) v))
                              (.push deltas (SetProperty. prev-vdom k v)))))))

(defn- diff-children! [deltas prev-vdom new-vdom]
  (let [prev-children (.-childNodes prev-vdom)
        new-children (.-childNodes new-vdom)
        len (max (alength prev-children) (alength new-children))]
    (loop [i 0]
      (when (< i len)
        (diff! deltas (aget prev-children i) (aget new-children i))
        (recur (inc i))))))

(defn- diff [prev-vdom new-vdom]
  (let [deltas (array)]
    (diff! deltas prev-vdom new-vdom)
    deltas))

(defn- commit-diff! [deltas]
  (loop [i 0]
    (when (< i (alength deltas))
      (apply-delta! (aget deltas i)))))

(defn element-node [tag props children]
  (set! (.-nodeName props) tag)
  (set! (.-childNodes props) children)
  props)

(defn text-node [text] #js {"nodeValue" text})

;;;; Mounting DOM
;;;; -----------------------------------------------------------------------------------------------

(defn renderer [mount-point]
  (let [root-inst (atom nil)]
    (fn [element]
      (swap! root-inst #(reconcile #js {"dom" mount-point} % element)))))

;;;; Demo App
;;;; ===============================================================================================

(defn counter-view [element]
  (reify Render
    (render [_] #js ["div" #js {"style" #js {"color" "red"}} (str (aget element 2))])))

(defn ui [state v]
  #js ["div" #js {}
       #js [counter-view #js {} v]
       #js ["input" #js {"type"    "button"
                         "onclick" (fn [_] (swap! state inc))
                         "value"   "lick!"}]])

(defn render-app! [render! state v]
  (render! (ui state v)))

(defn main []
  (let [state (atom 0)
        render! (renderer (dom/getElement "app-root"))]
    (add-watch state nil (fn [_ state _ v] (render-app! render! state v)))
    (render-app! render! state @state)))

(main)
