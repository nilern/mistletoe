(ns mistletoe.core
  (:require [mistletoe.node :as node :refer [el text-node]]
            [mistletoe.vdom-deps :refer [$ map-dep]]
            [mistletoe.diff :refer [diff! apply-diff!]]
            [goog.events :as ev]
            [clojure.string :as str]))

(defn process-initial-vdom! [vdom]
  (let [diff (array)]
    (node/materialize! vdom diff)
    (node/set-parents! vdom)
    (apply-diff! diff)
    vdom))

(defn process-vdom-changes! [dom-container vdom vdom*]
  (node/set-parents! vdom*)
  (let [diff (array)]
    (diff! diff dom-container vdom vdom*)
    (.log js/console diff)
    (apply-diff! diff))
  vdom*)

;;;; Demo App

(defn ui [state {:keys [todos window]}]
  (el :div
      :style {:position "absolute"
              :left     0
              :top      0
              :width    (:width window)
              :height   (:height window)}
      (el :div
          :style {:position "absolute"
                  :left     (map-dep #(-> % .-style .-width (- 500) (/ 2)) ($ :parent))
                  :top      50
                  :width    500}
          (el :ul (for [[i todo] todos]
                    (el :li
                        (interleave (map text-node (str/split-lines todo))
                                    (repeatedly #(el :br))) ; FIXME: Make nodes immutable so we can just `interpose`.
                        (el :input :type "button"
                            :style {:margin-left "8px"}
                            :value "x"
                            :onclick (fn [_] (swap! state update :todos dissoc i))))))
          (el :form (el :textarea :id "new-todo-text")
              (el :input :type "button"
                  :style {:margin-left "8px"}
                  :value "+"
                  :onclick (fn [_]
                             (let [todo-text (.. js/document (getElementById "new-todo-text") -value)]
                               (swap! state (fn [{:keys [counter] :as v}]
                                              (-> v
                                                  (update :counter inc)
                                                  (update :todos assoc counter todo-text)))))))))))

(defn main []
  (let [container (.getElementById js/document "app-root")
        state (atom {:counter 0, :todos {}, :window {:width  (.-innerWidth js/window)
                                                     :height (.-innerHeight js/window)}})

        vdom-root (atom (process-initial-vdom! (ui state @state)))]
    (.appendChild container (.-dom @vdom-root))

    (add-watch state nil (fn [_ state _ v]
                           (let [vdom (ui state v)]
                             (reset! vdom-root
                                     (process-vdom-changes! container @vdom-root vdom)))))

    (ev/listen js/window "resize" (fn [e]
                                    (swap! state assoc :window
                                           {:width  (.. e -target -innerWidth)
                                            :height (.. e -target -innerHeight)})))))
