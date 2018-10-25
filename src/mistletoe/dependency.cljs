(ns mistletoe.dependency
  (:refer-clojure :exclude [resolve])
  (:require [mistletoe.updates :refer [PropertyValue request-init-property!
                                       request-init-style-property! request-init-event-handler!
                                       schedule-set-property! schedule-set-style-property!
                                       schedule-set-event-handler!
                                       SetProperty SetCSSProperty SetEventListener]]))

;;;; # Dependency Resolution

(defprotocol Dependency
  (-resolve [self vnode visited]))

(extend-protocol Dependency
  default
  (-resolve [self _ _] self))

(defn resolve [v vnode]
  (if-some [v (-resolve v vnode (js/Set.))]
    v
    (throw (ex-info "unresolvable VDOM dependency" {:at v}))))

;;;; # Dependencies on Other VDOM Nodes

(deftype VDOMDependency [^:mutable cached deref-prop]
  Dependency
  (-resolve [self vnode visited]
    (if (.has visited self)
      (throw (ex-info "VDOM dependency cycle detected" {:at self}))
      (if (some? cached)
        cached
        (do (.add visited self)
            (when-some [v (deref-prop self vnode visited)]
              (set! cached v)
              v)))))


  PropertyValue
  (request-init-property! [self name vnode deltas] (.push deltas (SetProperty. vnode name self)))
  (request-init-style-property! [self name vnode deltas] (.push deltas (SetCSSProperty. vnode name self)))
  (request-init-event-handler! [self name vnode deltas]
    (.push deltas (SetEventListener. vnode name nil self)))

  (schedule-set-property! [self name vnode deltas] (.push deltas (SetProperty. vnode name self)))
  (schedule-set-style-property! [self name vnode deltas]
    (.push deltas (SetCSSProperty. vnode name self)))
  (schedule-set-event-handler! [self name vnode deltas prev]
    (.push deltas (SetEventListener. vnode name prev self))))

(defn $ [query]
  (VDOMDependency. nil (fn [_ vnode _]
                         (case query
                           :parent (let [res (.-parentNode vnode)]
                                     (when (and (some? res) (not (undefined? res)))
                                       res))
                           (throw (ex-info "invalid query" {:query query}))))))

(defn map-dep [f phloem]
  (VDOMDependency. nil (fn [_ vnode visited]
                         (some-> (-resolve phloem vnode visited) f))))
