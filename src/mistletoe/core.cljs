(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.dom :as dom]
            [goog.events :as ev]))

;;;; # Library

;;;; ## Building DOM

(declare render)

(defprotocol AsDOMNode
  (->DOM [self]))

(extend-protocol AsDOMNode
  string
  (->DOM [self] (dom/createTextNode self))

  array
  (->DOM [[tag props & children]]
    (let [dom (dom/createElement tag)]
      (doseq [child children]
        (render child dom))

      (obj/forEach props 
                   (fn [v k _]
                     (cond
                       (str/starts-with? k "on") (ev/listen dom (subs k 2) v)
                       :else (aset dom k v))))
      
      dom)))

(defn render [element parent-dom]
  (dom/appendChild parent-dom (->DOM element)))

;;;; ## Mounting DOM

(defn render! [element parent-dom]
  (if-let [old-element (.-lastChild parent-dom)]
    (dom/replaceNode (->DOM element) old-element)
    (render element parent-dom)))

;;;; # Demo App

(defn ui [state v]
  #js ["div" #js {}
       #js ["div" #js {} (str v)]
       #js ["input" #js {"type" "button"
                         "onclick" (fn [_] (swap! state inc))
                         "value" "lick!"}]])

(defn render-app! [state v]
  (render! (ui state v) (dom/getElement "app-root")))

(defn main []
  (let [state (atom 0)]
    (add-watch state nil (fn [_ state _ v] (render-app! state v)))
    (render-app! state @state)))

(main)
