(ns mistletoe.dom
  (:require [clojure.string :as s]
            [goog.object :as obj]
            [mistletoe.signal :as sgn]
            [mistletoe.signal.util :refer [alloc-watch-key]]))

;;;; # Protocols and Multimethods to Implement

;; TODO: Use this:
(defprotocol Drop
  "Manual destructor / resource disposal."
  (drop! [self] "Destructor; we are done with this object, release any (non-memory) resources immediately."))

(defprotocol DOMMount
  "Lifecycle protocol (for i.e. for activating and deactivating signal subscriptions)."
  (mount! [node] "Called when mounting to DOM.")
  (unmount! [node] "Called when unmounting from DOM."))

(defprotocol Child
  "Things that can be parented by DOM nodes."
  (-init-child! [self parent] "Append `self` under `parent` and do any other required initialization
                              (i.e. set up watches)."))

(defn init-child!
  "Append `child` under `parent` and do any other required initialization (i.e. set up watches)."
  [element child]
  (-init-child! child element))

(defprotocol AttributeValue
  (-init-attr! [self name element] "Set initial attribute value and eny other setup (i.e. set up watches),
                                   dispatching on the initial value.")
  (-init-style-attr! [self name element] "Set initial style value and eny other setup (i.e. set up watches),
                                         dispatching on the initial value."))

(defmulti init-attr!
          "Set initial attribute value and eny other setup (i.e. set up watches)."
          (fn [_element k _v]
            (if (s/starts-with? k "on")
              :event
              k)))

;;;; # DOM Node Watch Lifecycle

;; TODO: Use this:
(defprotocol Watcher
  "An object that watches [[IWatchable]]:s only when it is active (e.g. mounted to the DOM)."
  (add-watchee [self watchee k f])
  (remove-watchee [self watchee k])
  (activate-watches [self])
  (deactivate-watches [self]))

(defn add-watchee!
  "Add a `watchee` with key `k` and callback `f` to `dom`, but don't [[add-watch]] to `watchee` yet."
  [dom watchee k f]
  (let [watchees (.-__mistletoeWatchees dom)]
    (set! (.-__mistletoeWatchees dom) (update watchees watchee assoc k f))))

(defn- detached?
  "Is `dom` not mounted to the unmanaged DOM."
  [dom]
  (.-__mistletoeDetached dom))

(def ^:private mounted?
  "Is the node mounted to the unmamaged DOM."
  (complement detached?))

(defn- activate-watches!
  "Activate the watches in `dom` (i.e. call [[add-watch]] for each of them)."
  [dom]
  (doseq [[watchee kfs] (.-__mistletoeWatchees dom)
          [k f] kfs]
    (add-watch watchee k f)))

(defn- deactivate-watches!
  "Activate the watches in `dom` (i.e. call [[remove-watch]] for each of them)."
  [dom]
  (doseq [[watchee kfs] (.-__mistletoeWatchees dom)
          [k _] kfs]
    (remove-watch watchee k)))

(defn- run-children!
  "Call the side-effecting function `f` on each child of `element`."
  [f element]
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

;;;; # Children

(defn insert-before!
  "A version of `Element/insertBefore` that also uses [[DOMMount]] appropriately."
  [parent child next-child]
  (.insertBefore parent child next-child)
  (when (mounted? parent)
    ;; OPTIMIZE: Also mounts siblings unnecessarily:
    (mount! parent)))                                       ; Also remount parent to activate child signal watches

(defn append-child!
  "A version of `Element/appendChild` that also uses [[DOMMount]] appropriately."
  [parent child]
  (.appendChild parent child)
  (when (mounted? parent)
    ;; OPTIMIZE: Also mounts siblings unnecessarily:
    (mount! parent)))                                       ; Also remount parent to activate child signal watches.

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

(defprotocol FlattenChild
  "Producing a flat sequence of child nodes."
  (flatten-child [child] "Flatten any sequences and deref any [[IDeref]]:s in `child`."))

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
(defn- flat-children
  "Get the children of `parent` as a flat, signal-free sequence."
  [parent]
  (flatten-child (.-__mistletoeChildArgs parent)))

(defn- rearrange-children!
  "Rearrange the children of `parent` to match the current state of its watchees."
  [parent]
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

(defn- -init-signal-child!
  "Implementation of [[-init-child!]] for a signal child."
  [sgn parent]
  (let [v @sgn]
    (if (instance? js/Element v)
      (let [child v]
        (add-watchee! parent sgn (alloc-watch-key)
                      (fn [_ _ old-child new-child]
                        (replace-child! parent new-child old-child)))
        (append-child! parent child))
      (if (or (string? v) (not (seqable? v)))
        (let [child (.createTextNode js/document (str v))]
          (set! (.-__mistletoeDetached child) true)
          (add-watchee! child sgn (alloc-watch-key)
                        (fn [_ _ _ v] (set! (.-nodeValue child) (str v))))
          (append-child! parent child))
        (do (add-watchee! parent sgn (alloc-watch-key)
                          ;; OPTIMIZE: Rearranges all children every time:
                          (fn [_ _ _ _] (rearrange-children! parent)))
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
  (-init-child! [child parent] (.appendChild parent (.createTextNode js/document (str child)))))

;;;; # Attributes

(defn- -init-signal-attr!
  "Implementation of [[-init-attr!]] for signal attribute values."
  [sgn k element]
  (.setAttribute element k @sgn)
  (add-watchee! element sgn (alloc-watch-key)
                (fn [_ _ _ v] (.setAttribute element k v))))

(defn- -init-signal-style-attr!
  "Implementation of [[-init-style-attr!]] for signal attribute values."
  [sgn k element]
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

;;;; # Creating Reactive Elements

(defn el
  "Instantiate the [[INamed]] `tag` (e.g. `:div`) to a [[js/Element]]."
  [tag & args]
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
