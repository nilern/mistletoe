(ns mistletoe.dom
  (:require [goog.events :as ev]))

(defn- set-property! [dom property value] (aset dom property value))

(defn- set-style-property! [dom property value]
  (aset (.-style dom) property (if (number? value) (str value "px") value)))

(defn- set-event-handler! [dom property value prev-value]
  (when (and (some? prev-value) (not (undefined? prev-value)))
    (ev/unlisten dom property prev-value))
  (ev/listen dom property value))
