(ns mistletoe.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.signal.util :refer [map-index-cached]]
            [mistletoe.dom :refer [el append-child!]]))

(def state (sgn/source ["Pine" "Birch" "Spruce"]))

(defn- species-view [i name]
  (el :li
      (el :span name)

      (el :input :type "text" :value name
          :style {:background-color (smap #(if (empty? %) "red" "transparent") name)}
          :onchange (fn [ev] (swap! state assoc i (.. ev -target -value))))

      (el :input :type "button" :value "Delete"
          :onclick (fn [_]
                     (swap! state (fn [species]
                                    (vec (lazy-cat (take i species)
                                                   (drop (inc i) species)))))))))

(defn- ui-root []
  (el :div
      (el :h1 "Hello Trees!")

      (smap (fn [trees]
              (let [n (count trees)]
                (if (> n 0)
                  (el :p (str (count trees) " trees"))
                  (el :h2 "Deforestation strikes again! :'("))))
            state)

      (el :ul :style {:list-style-type "lower-greek"}
          ;; TODO: Make doing this easier:
          (->> state
               (smap (partial map-indexed (fn [i _] [i (smap #(nth % i) state)])))
               (smap (map-index-cached (fn [[i name]] (species-view i name))))))

      (el :form
          :onsubmit (fn [ev]
                      (.preventDefault ev)
                      (swap! state conj (.get (js/FormData. (.-target ev)) "species")))

          (el :input :type "text" :name "species")
          (el :input :type "submit" :value "Add tree"))))

(defn main []
  (let [parent (.getElementById js/document "app-root")]
    (append-child! parent (ui-root))))

