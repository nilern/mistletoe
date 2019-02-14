(ns mistletoe.core
  (:require [mistletoe.dom :refer [el]]))

(defn- ui-root []
  (el :div
      (el :h1 "Hello Trees!")

      (el :ul
          :style {:list-style-type "lower-greek"}
          (for [name ["Pine" "Birch" "Spruce"]]
            (el :li :onclick (fn [_] (.log js/console name))
                name)))))

(defn main []
  (.. js/document
      (getElementById "app-root")
      (appendChild (ui-root))))
