(ns mistletoe.dom
  (:require [clojure.string :as s]
            [goog.object :as obj]
            [mistletoe.signal :as sgn]
            [mistletoe.signal.util :refer [alloc-watch-key]]))

;;;;

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

(defn- proper-node? [dom]
  (<= (.-nodeType dom) (.-TEXT_NODE js/Node)))

(defn- add-watchee! [dom watchee k f]
  (let [watchees (.-__mistletoeWatchees dom)]
    (set! (.-__mistletoeWatchees dom) (update watchees watchee assoc k f))))

(defn- mount! [dom]
  (set! (.-__mistletoeDetached dom) false)
  (doseq [[watchee kfs] (.-__mistletoeWatchees dom)
          [k f] kfs]
    (add-watch watchee k f))

  (doseq [child (prim-seq (.-children dom))
          :when (proper-node? child)]
    (mount! child)))

(defn- unmount! [dom]
  (set! (.-__mistletoeDetached dom) true)
  (doseq [[watchee kfs] (.-__mistletoeWatchees dom)
          [k _] kfs]
    (remove-watch watchee k))

  (doseq [child (prim-seq (.-children dom))
          :when (proper-node? child)]
    (unmount! child)))

;;;;

(defn- -init-signal-child! [sgn parent]
  (let [v @sgn]
    (if (instance? js/Element v)
      (throw (js/Error. "unimplemented"))
      (let [child (.createTextNode js/document (str v))]
        (.appendChild parent child)
        (add-watch sgn (alloc-watch-key)
                   (fn [_ _ _ v] (set! (.-nodeValue child) (str v))))))))

(extend-protocol Child
  js/Element
  (-init-child! [child parent] (.appendChild parent child))

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
  (add-watch sgn (alloc-watch-key) (fn [_ _ _ v] (.setAttribute element k v))))

(defn- -init-signal-style-attr! [sgn k element]
  (obj/set (.-style element) k @sgn)
  (add-watch sgn (alloc-watch-key) (fn [_ _ _ v] (obj/set (.-style element) k v))))

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
