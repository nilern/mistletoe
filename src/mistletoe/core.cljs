(ns mistletoe.core
  (:require [mistletoe.diff :refer [materialize! set-parents! diff! apply-diff!]]
            [mistletoe.dom :refer [el text-node]]
            [mistletoe.phloem :refer [self-phloem]]))

(defn process-initial-vdom! [vdom]
  (let [diff (array)]
    (materialize! vdom diff)
    (set-parents! vdom)
    (apply-diff! diff)
    vdom))

(defn process-vdom-changes! [dom-container vdom vdom*]
  (set-parents! vdom*)
  (let [diff (array)]
    (diff! diff dom-container vdom vdom*)
    (apply-diff! diff))
  vdom*)

;;;; Demo App

(defn ui [state {:keys [todos]}]
  (el :div
      (el :ul (for [[i todo] todos]
                (el :li (text-node todo)
                    (el :input :type "button"
                        :style {:margin-left "8px"}
                        :value "x"
                        :onclick (fn [_] (swap! state update :todos dissoc i))))))
      (el :form (el :input :type "text"
                    :id "new-todo-text")
          (el :input :type "button"
              :style {:margin-left "8px"}
              :value (self-phloem #(str "+-" (.-type %1)))
              :onclick (fn [_]
                         (let [todo-text (.. js/document (getElementById "new-todo-text") -value)]
                           (swap! state (fn [{:keys [counter] :as v}]
                                          (-> v
                                              (update :counter inc)
                                              (update :todos assoc counter todo-text))))))))))

(defn main []
  (let [container (.getElementById js/document "app-root")
        state (atom {:counter 0, :todos {}})

        vdom-root (atom (process-initial-vdom! (ui state @state)))]
    (.appendChild container (.-dom @vdom-root))

    (add-watch state nil (fn [_ state _ v]
                           (let [vdom (ui state v)]
                             (reset! vdom-root
                                     (process-vdom-changes! container @vdom-root vdom)))))))
