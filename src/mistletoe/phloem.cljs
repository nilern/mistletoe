(ns mistletoe.phloem
  (:require [mistletoe.diff :refer [PropertyValue DerefProperty -deref-prop
                                    SetProperty SetCSSProperty SetEventListener]]))

(deftype Phloem [^:mutable cached deref-prop]
  DerefProperty
  (-deref-prop [self vnode visited]
    (if (.has visited self)
      (throw (ex-info "phloem cycle detected" {:at self}))
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
  (Phloem. nil (fn [_ vnode _]
                 (case query
                   :parent (let [res (.-parentNode vnode)]
                             (when (and (some? res) (not (undefined? res)))
                               res))
                   (throw (ex-info "invalid query" {:query query}))))))

(defn phmap [f phloem]
  (Phloem. nil (fn [_ vnode visited]
                 (some-> (-deref-prop phloem vnode visited) f))))
