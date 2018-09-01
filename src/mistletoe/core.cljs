(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.dom :as dom]
            [goog.events :as ev]))

;;;; # Library

;;;; ## Building VDOM

(deftype VDOMNode [element dom children])

(deftype VDOMTextNode [element dom])

(defprotocol Element
  (->VDOM [self])
  (update-dom-props! [self prev-props dom]))

(declare update-dom-props)

(extend-protocol Element
  string
  (->VDOM [self] (VDOMTextNode. self (dom/createTextNode self)))
  (update-dom-props! [self _ dom] (set! (.-nodeValue dom) self))

  array
  (->VDOM [self]
    (let [dom (dom/createElement (aget self 0))]
      (update-dom-props! self #js {} dom)

      (let [child-count (count self)
            inst-children (make-array (- child-count 2))]
        (loop [i 2]
          (when (< i child-count)
            (let [inst-child (->VDOM (aget self i) )]
              (aset inst-children (- i 2) inst-child)
              (dom/appendChild dom (.-dom inst-child))
              (recur (inc i)))))
        (VDOMNode. self dom inst-children))))
        
  (update-dom-props! [self prev-props dom]
    (obj/forEach (aget self 1)
                 (fn [v k _]
                   (cond
                     (str/starts-with? k "on") (let [ev-name (subs k 2)]
                                                 (when-let [prevv (aget prev-props k)]
                                                   (ev/unlisten dom ev-name prevv))
                                                 (ev/listen dom ev-name v))
                     :else (aset dom k v))))))

;;;; ## Reconciliation

(declare reconcile)

(defn reconcile-children [instance element]
  (let [inst-children (.-children instance)
        len (max (alength inst-children) (- (alength element) 2))
        inst-children* (make-array len)]
    (loop [i 0]
      (when (< i len)
        (some->> (reconcile (.-dom instance) (aget inst-children i) (aget element (+ i 2)))
                 (aset inst-children* i))
        (recur (inc i))))
    inst-children*))

(defn reconcile [parent-dom instance element]
  (cond
    (not instance) (let [instance* (->VDOM element)]
                     (dom/appendChild parent-dom (.-dom instance*))
                     instance*)

    (not element) (do (dom/removeNode (.-dom instance))
                      nil)

    (= (aget (.-element instance) 0) (aget element 0))
    (do (update-dom-props! element (aget (.-element instance) 1) (.-dom instance))
        (set! (.-children instance) (reconcile-children instance element))
        (set! (.-element instance) element)
        instance)

    :else (let [instance* (->VDOM element)]
            (dom/replaceNode (.-dom instance*) (.-dom instance))
            instance*)))

;;;; ## Mounting DOM

(defn renderer [mount-point]
  (let [root-inst (atom nil)]
    (fn [element]
      (swap! root-inst #(reconcile mount-point % element)))))

;;;; # Demo App

(defn ui [state v]
  #js ["div" #js {}
       #js ["div" #js {} (str v)]
       #js ["input" #js {"type" "button"
                         "onclick" (fn [_] (swap! state inc))
                         "value" "lick!"}]])

(defn render-app! [render! state v]
  (render! (ui state v)))

(defn main []
  (let [render! (renderer (dom/getElement "app-root"))
        state (atom 0)]
    (add-watch state nil (fn [_ state _ v] (render-app! render! state v)))
    (render-app! render! state @state)))

(main)
