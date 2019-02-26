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

(defn- add-watchee! [dom watchee k f]
  (let [watchees (.-__mistletoeWatchees dom)]
    (set! (.-__mistletoeWatchees dom) (update watchees watchee assoc k f))))

(defn- detached? [dom] (.-__mistletoeDetached dom))

(defn- mounted? [dom] (not (detached? dom)))

(defn- activate-watches! [dom]
  (doseq [[watchee kfs] (.-__mistletoeWatchees dom)
          [k f] kfs]
    (add-watch watchee k f)))

(defn- deactivate-watches! [dom]
  (doseq [[watchee kfs] (.-__mistletoeWatchees dom)
          [k _] kfs]
    (remove-watch watchee k)))

(defn- run-children! [f element]
  (run! f (prim-seq (.-childNodes element))))

(extend-protocol DOMMount
  default
  (mount! [_])
  (unmount! [_])


  js/Element
  (mount! [element]
    (run-children! mount! element)
    (activate-watches! element)
    (set! (.-__mistletoeDetached element) false))

  (unmount! [element]
    (run-children! unmount! element)
    (deactivate-watches! element)
    (set! (.-__mistletoeDetached element) true))


  js/Text
  (mount! [text]
    (activate-watches! text)
    (set! (.-__mistletoeDetached text) false))

  (unmount! [text]
    (deactivate-watches! text)
    (set! (.-__mistletoeDetached text) true)))

;;;;

(defn insert-before! [parent child next-child]
  (.insertBefore parent child next-child)
  (when (mounted? parent)
    ;; OPTIMIZE: Also mounts siblings unnecessarily:
    (mount! parent)))                                       ; Also remount parent to activate child signal watches

(defn append-child! [parent child]
  (.appendChild parent child)
  (when (mounted? parent)
    ;; OPTIMIZE: Also mounts siblings unnecessarily:
    (mount! parent)))                                       ; Also remount parent to activate child signal watches.

(defn remove-child! [parent child]
  (.removeChild parent child)
  (when (mounted? parent)
    (unmount! child)))

(defn replace-child! [parent new-child old-child]
  (.replaceChild parent new-child old-child)
  (when (mounted? parent)
    (unmount! old-child)
    (mount! new-child)))

(defprotocol FlattenChild
  (flatten-child [child]))

(extend-protocol FlattenChild
  sgn/SourceSignal
  (flatten-child [child] (flatten-child @child))

  sgn/ConstantSignal
  (flatten-child [child] (flatten-child @child))

  sgn/DerivedSignal
  (flatten-child [child] (flatten-child @child))

  default
  (flatten-child [child]
    (if (or (string? child) (not (seqable? child)))
      [child]
      (mapcat flatten-child child))))

;; OPTIMIZE: Probably allocates and thunkifies unnecessarily:
(defn- flat-children [parent]
  (flatten-child (.-__mistletoeChildArgs parent)))

(defn- rearrange-children! [parent]
  (loop [new-children (flat-children parent)
         old-child (aget (.-childNodes parent) 0)]
    (if (seq new-children)
      (let [[new-child & new-children] new-children]
        (if old-child
          (do (when-not (identical? new-child old-child)
                (insert-before! parent new-child old-child))
              (recur new-children (.-nextSibling old-child)))
          ;; Append new children where no one has gone before:
          (do (append-child! parent new-child)
              (recur new-children old-child))))
      (when old-child
        ;; Remove old children that are not needed any more.
        (remove-child! parent old-child)
        (recur new-children (.-nextSibling old-child))))))

(defn- -init-signal-child! [sgn parent]
  (let [v @sgn]
    (if (instance? js/Element v)
      (let [child v]
        (add-watchee! parent sgn (alloc-watch-key)
                      (fn [_ _ old-child new-child]
                        (when (not= new-child old-child)
                          (replace-child! parent new-child old-child))))
        (append-child! parent child))
      (if (or (string? v) (not (seqable? v)))
        (let [child (.createTextNode js/document (str v))]
          (set! (.-__mistletoeDetached child) true)
          (add-watchee! child sgn (alloc-watch-key)
                        (fn [_ _ oldv newv]
                          (when (not= newv oldv)
                            (set! (.-nodeValue child) (str newv)))))
          (append-child! parent child))
        (do (add-watchee! parent sgn (alloc-watch-key)
                          ;; OPTIMIZE: Rearranges all children every time:
                          (fn [_ _ old-children new-children]
                            (when (not= new-children old-children)
                              (rearrange-children! parent))))
            (doseq [child v]
              (append-child! parent child)))))))

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
                (fn [_ _ oldv newv]
                  (when (not= newv oldv)
                    (.setAttribute element k newv)))))

(defn- -init-signal-style-attr! [sgn k element]
  (obj/set (.-style element) k @sgn)
  (add-watchee! element sgn (alloc-watch-key)
                (fn [_ _ oldv newv]
                  (when (not= newv oldv)
                    (obj/set (.-style element) k newv)))))

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
    (set! (.-__mistletoeDetached el) true)
    (loop [args args, child-args []]
      (if-not (empty? args)
        (let [[arg & args] args]
          (if (keyword? arg)
            (if (empty? args)
              (throw (js/Error. (str "No value for attribute " arg)))
              (let [[arg* & args] args]
                (init-attr! el (name arg) arg*)
                (recur args child-args)))
            (do (if (or (string? arg) (not (seqable? arg)))
                  (init-child! el arg)
                  (doseq [arg arg]
                    (init-child! el arg)))
                (recur args (conj child-args arg)))))
        (set! (.-__mistletoeChildArgs el) child-args)))
    el))
