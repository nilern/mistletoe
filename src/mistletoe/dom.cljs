(ns mistletoe.dom
  (:require [clojure.string :as s]
            [goog.object :as obj]))

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

(extend-protocol Child
  js/Element
  (-init-child! [child parent] (.appendChild parent child))

  default
  (-init-child! [child parent]
    (.appendChild parent (.createTextNode js/document (str child)))))

(extend-protocol AttributeValue
  default
  (-init-attr! [v k element] (.setAttribute element k v))
  (-init-style-attr! [v k element] (obj/set (.-style element) k v)))

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
