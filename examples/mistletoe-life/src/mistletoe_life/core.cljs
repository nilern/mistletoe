(ns mistletoe-life.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.dom :refer [el]]))

(def ^:private live true)
(def ^:private dead false)

(defn- empty-row [width]
  (mapv (constantly dead) (range width)))

(defn- empty-grid [width height]
  (mapv (fn [_] (empty-row width)) (range height)))

(def ^:private state (sgn/source (empty-grid 100 100)))

(defn- row-view [i row]
  (el :tr
      (for [[j cell] (map-indexed (fn [j _] [j (smap #(get % j) row)]) @row)]
        (el :td
            :style {:width "10px"
                    :height "10px"
                    :background-color (smap #(if % "black" "none") cell)}
            :onclick (fn [_] (swap! state :update-in [i j] not))))))

(defn- ui-main [state]
  (el :table
      (for [[i row] (map-indexed (fn [i _] [i (smap #(get % i) state)]) @state)]
        (row-view i row))))

(defn main []
  (.. js/document
      (getElementById "app-root")
      (appendChild (ui-main state))))
