(ns mistletoe.dom
  (:require [clojure.string :as s]
            [goog.object :as obj]
            [mistletoe.signal :as sgn]
            [mistletoe.signal.util :refer [alloc-watch-key]]))

;;;;

(defprotocol DOMMount
  (mount! [node])
  (unmount! [node]))

(defprotocol Child
  (-init-child! [self parent]))

(defn init-child! [element child]
  (-init-child! child element))

(defprotocol AttributeValue
  (-init-attr! [self name element])
  (-init-style-attr! [self name element]))

(defmulti init-attr! (fn [_element k _v]
                       (if (s/starts-with? k "on")
                         :event
                         k)))

;;;;

(defn- proper-node? [dom] (<= (.-nodeType dom) (.-TEXT_NODE js/Node)))

(defn- element? [dom] (= (.-nodeType dom) (.-ELEMENT_NODE js/Node)))

(defn- add-watchee! [dom watchee k f]
  (let [watchees (.-__mistletoeWatchees dom)]
    (set! (.-__mistletoeWatchees dom) (update watchees watchee assoc k f))))

(defn- detached? [dom] (.-__mistletoeDetached dom))

(defn- mounted? [dom] (not (detached? dom)))

(extend-protocol DOMMount
  default
  (mount! [_])
  (unmount! [_])


  js/Element
  (mount! [element]
    (set! (.-__mistletoeDetached element) false)
    (doseq [[watchee kfs] (.-__mistletoeWatchees element)
            [k f] kfs]
      (add-watch watchee k f))

    (doseq [child (prim-seq (.-children element))
            :when (proper-node? child)]
      (mount! child)))

  (unmount! [element]
    (set! (.-__mistletoeDetached element) true)
    (doseq [[watchee kfs] (.-__mistletoeWatchees element)
            [k _] kfs]
      (remove-watch watchee k))

    (doseq [child (prim-seq (.-children element))
            :when (proper-node? child)]
      (unmount! child)))


  js/Text
  (mount! [text]
    (set! (.-__mistletoeDetached text) false)
    (doseq [[watchee kfs] (.-__mistletoeWatchees text)
            [k f] kfs]
      (add-watch watchee k f)))

  (unmount! [text]
    (set! (.-__mistletoeDetached text) true)
    (doseq [[watchee kfs] (.-__mistletoeWatchees text)
            [k _] kfs]
      (remove-watch watchee k))))

;;;;

(defn append-child! [parent child]
  (when (mounted? parent)
    (mount! child))
  (.appendChild parent child))

(defn- -init-signal-child! [sgn parent]
  (let [v @sgn]
    (if (instance? js/Element v)
      (throw (js/Error. "unimplemented"))
      (let [child (.createTextNode js/document (str v))]
        (add-watchee! child sgn (alloc-watch-key)
                      (fn [_ _ _ v] (set! (.-nodeValue child) (str v))))
        (append-child! parent child)))))

(extend-protocol Child
  js/Element
  (-init-child! [child parent] (append-child! parent child))

  sgn/SourceSignal
  (-init-child! [child parent] (-init-signal-child! child parent))

  sgn/ConstantSignal
  (-init-child! [child parent] (-init-signal-child! child parent))

  sgn/DerivedSignal
  (-init-child! [child parent] (-init-signal-child! child parent))

  default
  (-init-child! [child parent]
    (.appendChild parent (.createTextNode js/document (str child)))))

(defn- -init-signal-attr! [sgn k element]
  (.setAttribute element k @sgn)
  (add-watchee! element sgn (alloc-watch-key)
                (fn [_ _ _ v] (.setAttribute element k v))))

(defn- -init-signal-style-attr! [sgn k element]
  (obj/set (.-style element) k @sgn)
  (add-watchee! element sgn (alloc-watch-key)
                (fn [_ _ _ v] (obj/set (.-style element) k v))))

(extend-protocol AttributeValue
  default
  (-init-attr! [v k element] (.setAttribute element k v))
  (-init-style-attr! [v k element] (obj/set (.-style element) k v))

  sgn/SourceSignal
  (-init-attr! [v k element] (-init-signal-attr! v k element))
  (-init-style-attr! [v k element] (-init-signal-style-attr! v k element))

  sgn/ConstantSignal
  (-init-attr! [v k element] (-init-signal-attr! v k element))
  (-init-style-attr! [v k element] (-init-signal-style-attr! v k element))

  sgn/DerivedSignal
  (-init-attr! [v k element] (-init-signal-attr! v k element))
  (-init-style-attr! [v k element] (-init-signal-style-attr! v k element)))

(defmethod init-attr! :default [element k v]
  (-init-attr! v k element))

(defmethod init-attr! :event [element k f]
  (.addEventListener element (subs k 2) f))

(defmethod init-attr! "style" [element _ style-attrs]
  (doseq [[k v] style-attrs]
    (-init-style-attr! v (name k) element)))

;;;;

(defn el [tag & args]
  (let [el (.createElement js/document (name tag))]
    (loop [args args]
      (when-not (empty? args)
        (let [[arg & args] args]
          (if (keyword? arg)
            (if (empty? args)
              (throw (js/Error. (str "No value for attribute " arg)))
              (let [[arg* & args] args]
                (init-attr! el (name arg) arg*)
                (recur args)))
            (do (if (or (string? arg) (not (seqable? arg)))
                  (init-child! el arg)
                  (doseq [arg arg]
                    (init-child! el arg)))
                (recur args))))))
    el))
