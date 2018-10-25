(ns mistletoe.diff
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.events :as ev]))

;;; OPTIMIZE: Use iterator and requestIdleCallback to enable render scheduling.

(defn- set-property! [dom property value] (aset dom property value))

(defn- set-style-property! [dom property value]
  (aset (.-style dom) property (if (number? value) (str value "px") value)))

(defn- set-event-handler! [dom property value prev-value]
  (when (and (some? prev-value) (not (undefined? prev-value)))
    (ev/unlisten dom property prev-value))
  (ev/listen dom property value))

;;;;

(defprotocol Render
  (render [self vdom]))

;;;;

(defprotocol DerefProperty
  (-deref-prop [self vnode visited]))

(extend-protocol DerefProperty
  default
  (-deref-prop [self _ _] self))

(defn deref-prop [v vnode]
  (if-some [v (-deref-prop v vnode (js/Set.))]
    v
    (throw (ex-info "unresolvable VDOM dependency" {:at v}))))

;;;;

(defprotocol DOMDelta
  (apply-delta! [self]))

(defprotocol DOMPropDelta
  (deref-delta! [self]))

(extend-protocol DOMPropDelta
  default
  (deref-delta! [_] nil))

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
  (deref-delta! [_]
    (when-not (instance? js/Node node)
      (set! value (deref-prop value node))
      (set! node (.-dom node))))

  DOMDelta
  (apply-delta! [_] (set-property! node property value)))

(deftype SetCSSProperty [^:mutable node property ^:mutable value]
  DOMPropDelta
  (deref-delta! [_]
    (when-not (instance? js/Node node)
      (set! value (deref-prop value node))
      (set! node (.-dom node))))

  DOMDelta
  (apply-delta! [_] (set-style-property! node property value)))

(deftype SetEventListener [^:mutable node property ^:mutable prev-value ^:mutable value]
  DOMPropDelta
  (deref-delta! [_]
    (when-not (instance? js/Node node)
      (set! prev-value (deref-prop prev-value nil))
      (set! value (deref-prop value node))
      (set! node (.-dom node))))

  DOMDelta
  (apply-delta! [_] (set-event-handler! node property value prev-value)))

;;;;

(defprotocol PropertyValue
  (request-init-property! [self name vnode deltas])
  (request-init-style-property! [self name vnode deltas])
  (request-init-event-handler! [self name vnode deltas])

  (schedule-set-property! [self name vnode deltas])
  (schedule-set-style-property! [self name vnode deltas])
  (schedule-set-event-handler! [self name vnode deltas prev]))

(extend-protocol PropertyValue
  default
  (request-init-property! [self name vnode _] (set-property! (.-dom vnode) name self))
  (request-init-style-property! [self name vnode _] (set-style-property! (.-dom vnode) name self))
  (request-init-event-handler! [self name vnode _] (set-event-handler! (.-dom vnode) name self nil))

  (schedule-set-property! [self name vnode deltas]
    (.push deltas (SetProperty. (.-dom vnode) name self)))
  (schedule-set-style-property! [self name vnode deltas]
    (.push deltas (SetCSSProperty. (.-dom vnode) name self)))
  (schedule-set-event-handler! [self name vnode deltas prev]
    (.push deltas (SetEventListener. (.-dom vnode) name prev self))))

;;;;

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
             child (render component vdom)]
         (set! (.-component vdom) component)
         (set! (.-childNode vdom) child)
         (materialize! child parent-dom deltas))))))

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

;;;;

(declare diff-subtrees!)

;; OPTIMIZE: If dependency can be resolved, use the resolved value immediately:
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

;; OPTIMIZE: If dependency can be resolved, use the resolved value immediately:
(defn- diff-attributes! [deltas prev-vdom new-vdom]
  (obj/forEach new-vdom (fn [v k _]
                          (case k
                            ("dom" "parentNode" "childNodes" "nodeName") nil

                            "style"
                            (obj/forEach v (fn [v k _]
                                             (when (not= v (aget (.-style prev-vdom) k))
                                               (schedule-set-style-property! v k new-vdom deltas))))

                            (when (not= v (aget prev-vdom k))
                              (if (str/starts-with? k "on")
                                (schedule-set-event-handler! v (subs k 2) new-vdom deltas
                                                             (aget prev-vdom k))
                                (schedule-set-property! v k new-vdom deltas)))))))

;; OPTIMIZE: Use keys to make reordering children efficient:
(defn- diff-children! [deltas dom prev-vdom new-vdom]
  (if (string? (.-nodeName prev-vdom))
    (when-not (= (.-nodeName prev-vdom) "#text")
      (let [prev-children (.-childNodes prev-vdom)
            new-children (.-childNodes new-vdom)]
        (dotimes [i (max (alength prev-children) (alength new-children))]
          (diff! deltas dom (aget prev-children i) (aget new-children i)))))
    (diff! deltas dom (.-childNode prev-vdom) (.-childNode new-vdom))))

;;;;

(defn apply-diff! [deltas]
  (dotimes [i (alength deltas)]
    (deref-delta! (aget deltas i)))

  (dotimes [i (alength deltas)]
    (apply-delta! (aget deltas i))))
