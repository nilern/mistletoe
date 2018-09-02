(ns mistletoe.core
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [goog.dom :as dom]
            [goog.events :as ev]))

;;;; # Library

;;;; ## Elements

(declare VDOMNode VDOMComponentNode VDOMTextNode)
  
(defprotocol Element
  (->VDOM [self])
  (update-dom-props! [self prev-props dom]))

(defprotocol Tag
  (instantiate [self element]))
    
(defprotocol Render
  (render [self]))

(defn element-type [element] (aget element 0))

(defn element-props [element] (aget element 1))

(defn public-instance [element] ((element-type element) element))

(extend-protocol Element
  string
  (->VDOM [self] (VDOMTextNode. self (dom/createTextNode self)))
  (update-dom-props! [self _ dom] (set! (.-nodeValue dom) self))
  
  array
  (->VDOM [self] (instantiate (element-type self) self))
  (update-dom-props! [self prev-props dom]
    (obj/forEach (aget self 1)
                 (fn [v k _]
                   (cond
                     (str/starts-with? k "on") (let [ev-name (subs k 2)]
                                                 (when-let [prevv (aget prev-props k)]
                                                   (ev/unlisten dom ev-name prevv))
                                                 (ev/listen dom ev-name v))
                     :else (aset dom k v))))))

(extend-protocol Tag
  string
  (instantiate [self element]
    (let [dom (dom/createElement self)
          _ (update-dom-props! element #js {} dom)
          len (count element)
          inst-children (make-array (- len 2))]
      (loop [i 2]
        (when (< i len)
          (let [inst-child (->VDOM (aget element i))]
            (aset inst-children (- i 2) inst-child)
            (dom/appendChild dom (.-dom inst-child))
            (recur (inc i)))))
      (VDOMNode. element dom inst-children)))
  
  function
  (instantiate [self element]
    (let [component-inst (public-instance element)
          child-elem (render component-inst)
          child-inst (->VDOM child-elem)]
      (VDOMComponentNode. element (.-dom child-inst) child-inst component-inst))))

;;;; ## Virtual DOM and Reconciliation

(declare reconcile reconcile-children)

(defprotocol VirtualDOMNode
  (reconcile-subnodes! [self element* parent-dom]))

(deftype VDOMNode [^:mutable element dom ^:mutable children]
  VirtualDOMNode
  (reconcile-subnodes! [self element* _]
    (update-dom-props! element* (element-props element) dom)
    (set! element element*)
    (set! children (reconcile-children self element*))))

(deftype VDOMTextNode [^:mutable element dom]
  VirtualDOMNode
  (reconcile-subnodes! [self element* _]
    (update-dom-props! element* element dom)
    (set! element element*)))

(deftype VDOMComponentNode [^:mutable element ^:mutable dom ^:mutable child
                            ^:mutable component-inst]
  VirtualDOMNode
  (reconcile-subnodes! [self element* parent-dom]
    (let [component-inst* (public-instance element*)
          child* (reconcile parent-dom child (render component-inst*))]
      (set! element element*)
      (set! dom (.-dom child*))
      (set! child child*)
      (set! component-inst component-inst*))))

(defn reconcile [parent-dom instance element]
  (cond
    (not instance) (let [instance* (->VDOM element)]
                     (dom/appendChild parent-dom (.-dom instance*))
                     instance*)

    (not element) (do (dom/removeNode (.-dom instance))
                      nil)

    (not= (element-type (.-element instance)) (element-type element))
    (let [instance* (->VDOM element)]
      (dom/replaceNode (.-dom instance*) (.-dom instance))
      instance*)

    :else (do (reconcile-subnodes! instance element parent-dom)
              instance)))
              
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

;;;; ## Mounting DOM

(defn renderer [mount-point]
  (let [root-inst (atom nil)]
    (fn [element]
      (swap! root-inst #(reconcile mount-point % element)))))

;;;; # Demo App

(defn counter-view [element]
  (reify Render
    (render [_] #js ["div" #js {} (str (aget element 2))])))

(defn ui [state v]
  #js ["div" #js {}
       #js [counter-view #js {} v]
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
