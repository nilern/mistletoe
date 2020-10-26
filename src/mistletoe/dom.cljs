(ns mistletoe.dom
  (:require [clojure.string :as s]
            [goog.object :as obj]
            [mistletoe.signal :as sgn]
            [mistletoe.vecnal :as vcn]
            [mistletoe.signal.util :refer [alloc-watch-key]]))

;;;; # DOM Node Watch Lifecycle

(defprotocol Watcher
  "An object that watches [[IWatchable]]:s only when it is active (e.g. mounted to the DOM)."
  (add-watchee [self watchee k f] "Add a `watchee` with key `k` and callback `f` to `self`,
                                  but don't [[add-watch]] to `watchee` yet.")
  (remove-watchee [self watchee k] "Remove the `watchee` with key `k` from `self`.")
  (add-multi-watchee [self watchee k w])
  (remove-multi-watchee [self watchee k])
  (activate-watches [self] "Activate the watches in `self` (i.e. call [[add-watch]] for each of them).")
  (deactivate-watches [self] "Deactivate the watches in `self` (i.e. call [[remove-watch]] for each of them)."))

(extend-protocol Watcher
  js/Node
  (add-watchee [self watchee k f]
    (let [watchees (.-__mistletoeWatchees self)]
      (set! (.-__mistletoeWatchees self) (update watchees watchee assoc k f))))

  (remove-watchee [self watchee k]
    (let [watchees (.-__mistletoeWatchees self)]
      (set! (.-__mistletoeWatchees self) (update watchees watchee dissoc k))))

  (add-multi-watchee [self watchee k f]
    (let [watchees (.-__mistletoeMultiWatchees self)]
      (set! (.-__mistletoeMultiWatchees self) (update watchees watchee assoc k f))))

  (remove-multi-watchee [self watchee k]
    (let [watchees (.-__mistletoeMultiWatchees self)]
      (set! (.-__mistletoeMultiWatchees self) (update watchees watchee dissoc k))))

  (activate-watches [self]
    (reduce-kv (fn [_ watchee kfs]
                 (reduce-kv (fn [watchee k f] (add-watch watchee k f) watchee)
                            watchee kfs))
               nil (.-__mistletoeWatchees self))
    (reduce-kv (fn [_ watchee kfs]
                 (reduce-kv (fn [watchee k f] (vcn/add-multi-watch watchee k f) watchee)
                            watchee kfs))
               nil (.-__mistletoeMultiWatchees self)))

  (deactivate-watches [self]
    (reduce-kv (fn [_ watchee kfs]
                 (reduce-kv (fn [watchee k _] (remove-watch watchee k) watchee)
                            watchee kfs))
               nil (.-__mistletoeWatchees self))
    (reduce-kv (fn [_ watchee kfs]
                 (reduce-kv (fn [watchee k _] (vcn/remove-multi-watch watchee k) watchee)
                            watchee kfs))
               nil (.-__mistletoeMultiWatchees self))))

(defn- detached?
  "Is `dom` not mounted to the unmanaged DOM?"
  [dom]
  (.-__mistletoeDetached dom))

(def ^:private mounted?
  "Is the node mounted to the unmanaged DOM?"
  (complement detached?))

(defn- run-children!
  "Call the side-effecting function `f` on each child of `element`."
  [f element]
  (loop [child (aget (.-childNodes element) 0)]
    (when child
      (f child)
      (recur (.-nextSibling child)))))

(defprotocol DOMMount
  "Lifecycle protocol (for i.e. for activating and deactivating signal subscriptions)."
  (mount! [node] "Called when mounting to DOM.")
  (unmount! [node] "Called when unmounting from DOM."))

(extend-protocol DOMMount
  js/Node
  (mount! [element]
    (run-children! mount! element)
    (activate-watches element)
    (set! (.-__mistletoeDetached element) false))

  (unmount! [element]
    (run-children! unmount! element)
    (deactivate-watches element)
    (set! (.-__mistletoeDetached element) true))


  default
  (mount! [_])
  (unmount! [_]))

;; TODO: Use this:
(defprotocol Drop
  "Manual destructor / resource disposal."
  (drop! [self] "Destructor; we are done with this object, release any (non-memory) resources immediately."))

;;;; # Children

(defn insert-before!
  "A version of `Element/insertBefore` that also uses [[DOMMount]] appropriately."
  [parent child next-child]
  (.insertBefore parent child next-child)
  (when (mounted? parent)
    (mount! child)
    (activate-watches parent)))

(defn append-child!
  "A version of `Element/appendChild` that also uses [[DOMMount]] appropriately."
  [parent child]
  (.appendChild parent child)
  (when (mounted? parent)
    (mount! child)
    (activate-watches parent)))

(defn remove-child!
  "A version of `Element/removeChild` that also uses [[DOMMount]] appropriately."
  [parent child]
  (.removeChild parent child)
  (when (mounted? parent)
    (unmount! child)))

(defn replace-child!
  "A version of `Element/replaceChild` that also uses [[DOMMount]] appropriately."
  [parent new-child old-child]
  (.replaceChild parent new-child old-child)
  (when (mounted? parent)
    (unmount! old-child)
    (mount! new-child)))

(defn child->node [child]
  (cond
    (instance? js/Node child) child
    (string? child) (.createTextNode js/document child)
    :else (throw (ex-info "invalid mistletoe.dom child" child))))

(defn child->vecnal [child]
  (cond
    (sgn/signal? child) (vcn/imux (sgn/smap #(list (child->node %)) child))
    (vcn/vecnal? child) (vcn/smap-map child->node child)
    (seqable? child) (vcn/pure (mapv child->node child))
    :else (vcn/pure (vector (child->node child)))))

(defn init-children! [parent children]
  (let [children (apply vcn/smap-concat (map child->vecnal children))]
    (add-multi-watchee parent children (alloc-watch-key)
                       (reify vcn/VecnalWatcher
                         (on-insert [_ i child] (insert-before! parent child (aget (.-childNodes parent) i)))
                         (on-assoc [_ i child] (replace-child! parent child (aget (.-childNodes parent) i)))
                         (on-dissoc [_ i] (remove-child! parent (aget (.-childNodes parent) i)))))
    (doseq [child @children]
      (cond
        (sgn/signal? child) (append-child! parent (child->node @child))
        (vcn/vecnal? child) (run! #(append-child! parent (child->node %)) @child)
        (seqable? child) (run! #(append-child! parent (child->node %)) child)
        :else (append-child! parent (child->node child))))))

;;;; # Attributes

(defn set-attribute!
  "Set attribute `k` of `element` to `v`. If `v` is nil uses Element.removeAttribute() instead."
  [element k v]
  (if (some? v)
    (.setAttribute element k v)
    (.removeAttribute element k)))

(defprotocol AttributeValue
  (-init-attr! [self name element] "Set initial attribute value and eny other setup (i.e. set up watches),
                                   dispatching on the initial value.")
  (-init-style-attr! [self name element] "Set initial style value and eny other setup (i.e. set up watches),
                                         dispatching on the initial value."))

(defn- -init-signal-attr!
  "Implementation of [[-init-attr!]] for signal attribute values."
  [sgn k element]
  (set-attribute! element k @sgn)
  (add-watchee element sgn (alloc-watch-key) (fn [_ _ _ v] (set-attribute! element k v))))

(defn- -init-signal-style-attr!
  "Implementation of [[-init-style-attr!]] for signal attribute values."
  [sgn k element]
  (obj/set (.-style element) k @sgn)
  (add-watchee element sgn (alloc-watch-key) (fn [_ _ _ v] (obj/set (.-style element) k v))))

(extend-protocol AttributeValue
  default
  (-init-attr! [v k element] (set-attribute! element k v))
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

(defmulti init-attr!
          "Set initial attribute value and eny other setup (i.e. set up watches)."
          (fn [_element k _v]
            (if (s/starts-with? k "on")
              :event
              k)))

(defmethod init-attr! :default [element k v]
  (-init-attr! v k element))

(defmethod init-attr! :event [element k f]
  (.addEventListener element (subs k 2) f))

(defmethod init-attr! "style" [element _ style-attrs]
  (reduce-kv (fn [element k v] (-init-style-attr! v (name k) element) element)
             element style-attrs))

;;;; # Creating Reactive Elements

(defn el
  "Instantiate the [[INamed]] `tag` (e.g. `:div`) to a [[js/Element]]."
  [tag & args]
  (let [el (.createElement js/document (name tag))]
    (set! (.-__mistletoeDetached el) true)
    (loop [args args, child-args []]
      (when-not (empty? args)
        (let [[arg & args*] args]
          (if (keyword? arg)
            (if (empty? args*)
              (throw (js/Error. (str "No value for attribute " arg)))
              (let [[arg* & args*] args*]
                (init-attr! el (name arg) arg*)
                (recur args* child-args)))
            (init-children! el args)))))
    el))
