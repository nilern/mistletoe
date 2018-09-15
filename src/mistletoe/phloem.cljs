(ns mistletoe.phloem
  (:require [mistletoe.diff :refer [PropertyValue DerefProperty -deref-prop
                                    SetProperty SetCSSProperty SetEventListener]]))

(deftype SelfPhloem [^:mutable cached f]
  DerefProperty
  (-deref-prop [self vnode visited]
    (if (contains? visited self)
      (throw (ex-info "phloem cycle detected" {:at self}))
      (if (some? cached)
        cached
        (let [v (-deref-prop (f vnode) vnode (conj visited self))]
          (when (some? v)
            (set! cached v))
          v))))


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

(defn self-phloem [f] (SelfPhloem. nil f))
