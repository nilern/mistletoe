(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.events :as ev]))

;;;; Library
;;;; ===============================================================================================

(defn- set-property! [dom property value] (aset dom property value))

(defn- set-style-property! [dom property value] (aset (.-style dom) property value))

(defn- set-event-handler! [dom property value prev-value]
  (when (and (some? prev-value) (not (undefined? prev-value)))
    (ev/unlisten dom property prev-value))
  (ev/listen dom property value))

(defprotocol Render
  (render [self vdom]))

(defprotocol PropertyValue
  (-deref-prop [self vnode visited])

  (init-property! [self name vnode deltas])
  (init-style-property! [self name vnode deltas])
  (init-event-handler! [self name vnode deltas])

  (schedule-set-property! [self name vnode deltas])
  (schedule-set-style-property! [self name vnode deltas])
  (schedule-set-event-handler! [self name vnode deltas prev]))

(defn deref-prop [v vnode]
  (if-some [v (-deref-prop v vnode #{})]
    v
    (throw (ex-info "unresolvable phloem" {:at v}))))

(defprotocol DOMDelta
  (apply-delta! [self]))

(defprotocol DOMPropDelta
  (dereph-delta! [self]))

(extend-protocol DOMPropDelta
  default
  (dereph-delta! [_] nil))

(deftype AppendChild [parent-dom child]
  DOMDelta
  (apply-delta! [_] (.appendChild parent-dom child)))

(deftype RemoveChild [parent-dom child]
  DOMDelta
  (apply-delta! [_] (.removeChild parent-dom child)))

(deftype ReplaceChild [parent-dom new-child old-child]
  DOMDelta
  (apply-delta! [_] (.replaceChild parent-dom new-child old-child)))

(deftype SetProperty [^:mutable node property ^:mutable value]
  DOMPropDelta
  (dereph-delta! [_]
    (when-not (instance? js/Node node)
      (set! value (deref-prop value node))
      (set! node (.-dom node))))

  DOMDelta
  (apply-delta! [_] (set-property! node property value)))

(deftype SetCSSProperty [^:mutable node property ^:mutable value]
  DOMPropDelta
  (dereph-delta! [_]
    (when-not (instance? js/Node node)
      (set! value (deref-prop value node))
      (set! node (.-dom node))))

  DOMDelta
  (apply-delta! [_] (set-style-property! node property value)))

(deftype SetEventListener [^:mutable node property ^:mutable prev-value ^:mutable value]
  DOMPropDelta
  (dereph-delta! [_]
    (when-not (instance? js/Node node)
      (set! prev-value (deref-prop prev-value nil))
      (set! value (deref-prop value node))
      (set! node (.-dom node))))

  DOMDelta
  (apply-delta! [_] (set-event-handler! node property value prev-value)))

(extend-protocol PropertyValue
  default
  (-deref-prop [self _ _] self)

  (init-property! [self name vnode _] (set-property! (.-dom vnode) name self))
  (init-style-property! [self name vnode _] (set-style-property! (.-dom vnode) name self))
  (init-event-handler! [self name vnode _] (ev/listen (.-dom vnode) name self))

  (schedule-set-property! [self name vnode deltas]
    (.push deltas (SetProperty. (.-dom vnode) name self)))
  (schedule-set-style-property! [self name vnode deltas]
    (.push deltas (SetCSSProperty. (.-dom vnode) name self)))
  (schedule-set-event-handler! [self name vnode deltas prev]
    (.push deltas (SetEventListener. (.-dom vnode) name prev self))))

(deftype SelfPhloem [^:mutable cached f]
  PropertyValue
  (-deref-prop [self vnode visited]
    (if (contains? visited self)
      (throw (ex-info "phloem cycle detected" {:at self}))
      (if (some? cached)
        cached
        (let [v (-deref-prop (f vnode) vnode (conj visited self))]
          (when (some? v)
            (set! cached v))
          v))))

  (init-property! [self name vnode deltas] (.push deltas (SetProperty. vnode name self)))
  (init-style-property! [self name vnode deltas] (.push deltas (SetCSSProperty. vnode name self)))
  (init-event-handler! [self name vnode deltas]
    (.push deltas (SetEventListener. vnode name nil self)))

  (schedule-set-property! [self name vnode deltas] (.push deltas (SetProperty. vnode name self)))
  (schedule-set-style-property! [self name vnode deltas]
    (.push deltas (SetCSSProperty. vnode name self)))
  (schedule-set-event-handler! [self name vnode deltas prev]
    (.push deltas (SetEventListener. vnode name prev self))))

(defn self-phloem [f] (SelfPhloem. nil f))

;; OPTIMIZE: If phloem can be resolved, use the resolved value immediately:
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
                                                   (init-style-property! v k vdom deltas)))

                          (if (str/starts-with? k "on")
                            (init-event-handler! v (subs k 2) vdom deltas)
                            (init-property! v k vdom deltas)))))
    dom))

(defn materialize! [vdom parent-dom deltas]
  (let [node-name (.-nodeName vdom)]
    (if (string? node-name)
      (let [dom (materialize-element! vdom parent-dom deltas)]
        (let [children (.-childNodes vdom)]
          (when-not (undefined? children)
            (dotimes [i (alength children)]
              (materialize! (aget children i) dom deltas)))))
      (let [component (node-name vdom)
            child (render component vdom)]
        (set! (.-component vdom) component)
        (set! (.-childNode vdom) child)
        (materialize! child parent-dom deltas)))))

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

(declare diff-subtrees!)

;; OPTIMIZE: If phloem can be resolved, use the resolved value immediately:
(defn diff! [deltas parent-dom prev-vdom new-vdom]
  (cond
    (undefined? prev-vdom) (do (materialize! new-vdom parent-dom deltas)
                               (.push deltas (AppendChild. parent-dom (.-dom new-vdom)))
                               new-vdom)

    (undefined? new-vdom) (do (.push deltas (RemoveChild. parent-dom (.-dom prev-vdom)))
                              nil)

    (not= (.-nodeName prev-vdom) (.-nodeName new-vdom))
    (do (materialize! new-vdom parent-dom deltas)
        (.push deltas (ReplaceChild. parent-dom (.-dom new-vdom) (.-dom prev-vdom)))
        new-vdom)

    (string? (.-nodeName prev-vdom)) (do (set! (.-dom new-vdom) (.-dom prev-vdom))
                                         (diff-subtrees! deltas (.-dom prev-vdom) prev-vdom
                                                         new-vdom)
                                         new-vdom)

    :else (let [element (.-childNode prev-vdom)
                element* (render (.-component prev-vdom) new-vdom)]
            (set! (.-childNode prev-vdom) element*)
            (set-parents! prev-vdom)
            (diff! deltas parent-dom element element*)
            prev-vdom)))

(declare diff-attributes! diff-children!)

(defn- diff-subtrees! [deltas dom prev-vdom new-vdom]
  (diff-attributes! deltas prev-vdom new-vdom)
  (diff-children! deltas dom prev-vdom new-vdom))

;; OPTIMIZE: If phloem can be resolved, use the resolved value immediately:
(defn diff-attributes! [deltas prev-vdom new-vdom]
  (obj/forEach new-vdom (fn [v k _]
                          (case k
                            ("dom" "parentNode" "childNodes" "nodeName") nil

                            "style"
                            (obj/forEach v (fn [v k _]
                                             (schedule-set-style-property! v k new-vdom deltas)))

                            (if (str/starts-with? k "on")
                              (schedule-set-event-handler! v (subs k 2) new-vdom deltas
                                                           (aget prev-vdom k))
                              (schedule-set-property! v k new-vdom deltas))))))

(defn- diff-children! [deltas dom prev-vdom new-vdom]
  (if (string? (.-nodeName prev-vdom))
    (when-not (= (.-nodeName prev-vdom) "#text")
      (let [prev-children (.-childNodes prev-vdom)
            new-children (.-childNodes new-vdom)]
        (dotimes [i (max (alength prev-children) (alength new-children))]
          (diff! deltas dom (aget prev-children i) (aget new-children i)))))
    (diff! deltas dom (.-childNode prev-vdom) (.-childNode new-vdom))))

(defn dereph-diff! [deltas]
  (dotimes [i (alength deltas)]
    (dereph-delta! (aget deltas i))))

(defn commit-diff! [deltas]
  (dotimes [i (alength deltas)]
    (apply-delta! (aget deltas i))))

;;;; Element Factories
;;;; -----------------------------------------------------------------------------------------------

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

(defn text-node [text]
  #js {"nodeName"  "#text"
       "nodeValue" text})

;;;; Demo App
;;;; ===============================================================================================

(defn ui [state {:keys [todos]}]
  (el :div
      (el :ul (for [[i todo] todos]
                (el :li (text-node todo)
                    (el :input :type "button"
                        :style {:margin-left "8px"}
                        :value "x"
                        :onclick (fn [_] (swap! state update :todos dissoc i))))))
      (el :form (el :input :type "text"
                    :id "new-todo-text")
          (el :input :type "button"
              :style {:margin-left "8px"}
              :value (self-phloem #(str "+-" (.-type %1)))
              :onclick (fn [_]
                         (let [todo-text (.. js/document (getElementById "new-todo-text") -value)]
                           (swap! state (fn [{:keys [counter] :as v}]
                                          (-> v
                                              (update :counter inc)
                                              (update :todos assoc counter todo-text))))))))))

(defn main []
  (let [state (atom {:counter 0, :todos {}})
        vdom-root (let [deltas (array)
                        vdom (ui state @state)]
                    (materialize! vdom nil deltas)
                    (set-parents! vdom)
                    (doto deltas
                      (dereph-diff!)
                      (commit-diff!))
                    (atom vdom))
        container (.getElementById js/document "app-root")
        _ (.appendChild container (.-dom @vdom-root))]
    (add-watch state nil (fn [_ state _ v]
                           (let [vdom (ui state v)]
                             (set-parents! vdom)
                             (let [deltas (array)
                                   vdom (diff! deltas container @vdom-root vdom)]
                               (doto deltas
                                 (dereph-diff!)
                                 (commit-diff!))
                               (reset! vdom-root vdom)))))))
