(ns mistletoe.updates
  (:refer-clojure :exclude [resolve])
  (:require [mistletoe.dependency :refer [resolve]]
            [mistletoe.dom :as dom]))

;;;; # Updates

(defprotocol DOMUpdate
  (apply-delta! [self]))

(defprotocol DOMPropertyUpdate
  (deref-delta! [self]))

(extend-protocol DOMPropertyUpdate
  default
  (deref-delta! [_] nil))

(deftype AppendChild [parent-dom child]
  DOMUpdate
  (apply-delta! [_] (.appendChild parent-dom child)))

(deftype RemoveChild [parent-dom child]
  DOMUpdate
  (apply-delta! [_] (.removeChild parent-dom child)))

(deftype ReplaceChild [parent-dom new-child old-child]
  DOMUpdate
  (apply-delta! [_] (.replaceChild parent-dom new-child old-child)))

(deftype SetProperty [^:mutable node property ^:mutable value]
  DOMPropertyUpdate
  (deref-delta! [_]
    (when-not (instance? js/Node node)
      (set! value (resolve value node))
      (set! node (.-dom node))))

  DOMUpdate
  (apply-delta! [_] (dom/set-property! node property value)))

(deftype SetCSSProperty [^:mutable node property ^:mutable value]
  DOMPropertyUpdate
  (deref-delta! [_]
    (when-not (instance? js/Node node)
      (set! value (resolve value node))
      (set! node (.-dom node))))

  DOMUpdate
  (apply-delta! [_] (dom/set-style-property! node property value)))

(deftype SetEventListener [^:mutable node property ^:mutable prev-value ^:mutable value]
  DOMPropertyUpdate
  (deref-delta! [_]
    (when-not (instance? js/Node node)
      (set! prev-value (resolve prev-value nil))
      (set! value (resolve value node))
      (set! node (.-dom node))))

  DOMUpdate
  (apply-delta! [_] (dom/set-event-handler! node property value prev-value)))

;;;; # Scheduling Property Updates

(defprotocol PropertyValue
  (request-init-property! [self name vnode deltas])
  (request-init-style-property! [self name vnode deltas])
  (request-init-event-handler! [self name vnode deltas])

  (schedule-set-property! [self name vnode deltas])
  (schedule-set-style-property! [self name vnode deltas])
  (schedule-set-event-handler! [self name vnode deltas prev]))

(extend-protocol PropertyValue
  default
  (request-init-property! [self name vnode _] (dom/set-property! (.-dom vnode) name self))
  (request-init-style-property! [self name vnode _] (dom/set-style-property! (.-dom vnode) name self))
  (request-init-event-handler! [self name vnode _] (dom/set-event-handler! (.-dom vnode) name self nil))

  (schedule-set-property! [self name vnode deltas]
    (.push deltas (SetProperty. (.-dom vnode) name self)))
  (schedule-set-style-property! [self name vnode deltas]
    (.push deltas (SetCSSProperty. (.-dom vnode) name self)))
  (schedule-set-event-handler! [self name vnode deltas prev]
    (.push deltas (SetEventListener. (.-dom vnode) name prev self))))
