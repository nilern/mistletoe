(ns mistletoe.signal
  "The signal reference types.")

;;;; # Source

(deftype SourceSignal [^:mutable value, equals?, ^:mutable watches]
  IDeref
  (-deref [_] value)


  IReset
  (-reset! [self value*]
    (let [old-value value]
      (set! value value*)
      (-notify-watches self old-value value*)
      value*))


  ISwap
  (-swap! [self f] (-reset! self (f value)))
  (-swap! [self f a] (-reset! self (f value a)))
  (-swap! [self f a b] (-reset! self (f value a b)))
  (-swap! [self f a b xs] (-reset! self (f value a b xs)))


  IWatchable
  (-notify-watches [self old-val new-val]
    (when-not (equals? old-val new-val)                     ; This condition is why we don't just use cljs.core/Atom.
      (doseq [[k f] watches]
        (f k self old-val new-val))))

  (-add-watch [_ k f] (set! watches (assoc watches k f)))

  (-remove-watch [_ k] (set! watches (dissoc watches k))))

(defn source*
  "A source signal with initial `value`; can be updated (with `reset!` or `swap!`).
  Unlike normal atom, will not notify watchers if value is updated but does not actually change
  (according to the equality function `equals?`)."
  [equals? value]
  (SourceSignal. value equals? {}))

(defn source
  "Like [[source*]], but the equality function is always [[=]]."
  [value]
  (source* = value))

;;;; # Constant

(deftype ConstantSignal [value]
  IDeref
  (-deref [_] value)


  IWatchable
  (-notify-watches [_ _ _] nil)
  (-add-watch [_ _ _] nil)
  (-remove-watch [_ _] nil))

(defn pure
  "A constant signal; `deref` always returns `value` and watch operations are no-ops."
  [value]
  (ConstantSignal. value))

;;;; # Derived

(defn- propagator [f dependencies]
  (fn [self dependency _old-val new-val]
    (let [old-val (.-value self)
          ;; OPTIMIZE:
          new-val (apply f (map (fn [dep]
                                  (if (identical? dep dependency)
                                    new-val
                                    @dependency))
                                dependencies))]
      (set! (.-value self) new-val)
      (-notify-watches self old-val new-val))))

(deftype DerivedSignal [f, dependencies, ^:mutable value, equals?, ^:mutable watches, propagate]
  IDeref
  (-deref [_] value)


  IWatchable
  (-notify-watches [self old-val new-val]
    ;; TODO: DRY:
    (when-not (equals? old-val new-val)
      (doseq [[k g] watches]
        (g k self old-val new-val))))

  (-add-watch [self k g]
    (when (empty? watches)
      ;; To avoid space leaks and 'unused' updates to `self` only start watching `dependencies`
      ;; when `self` gets its first watcher:
      (doseq [dependency dependencies]
        (add-watch dependency self propagate)))
    (set! watches (assoc watches k g)))

  (-remove-watch [self k]
    (set! watches (dissoc watches k))
    (when (empty? watches)
      ;; Watcher count just became zero, but watchees still have pointers to `self`.
      ;; Remove those to avoid space leaks and 'unused' updates to `self`:
      (doseq [dependency dependencies]
        (-remove-watch dependency self)))))

(defn smap*
  "A derived signal; its value is always equal to `(apply f (map deref signals))`.
  Will only notify watchers if the output value actually changed (according to the equality function `equals?`).
  Will only keep watches in `signals` if has watches itself so an impure `f` is not recommended."
  ([equals? f] (let [dependencies []]
                 (DerivedSignal. f dependencies (f) equals? {} (propagator f dependencies))))
  ([equals? f a] (let [dependencies [a]]
                   (DerivedSignal. f dependencies (f @a) equals? {} (propagator f dependencies))))
  ([equals? f a b] (let [dependencies [a b]]
                     (DerivedSignal. f dependencies (f @a @b) equals? {} (propagator f dependencies))))
  ([equals? f a b c] (let [dependencies [a b c]]
                       (DerivedSignal. f dependencies (f @a @b @c) equals? {} (propagator f dependencies))))
  ([equals? f a b c d] (let [dependencies [a b c d]]
                         (DerivedSignal. f dependencies (f @a @b @c @d) equals? {} (propagator f dependencies))))
  ([equals? f a b c d & more]
   (let [dependencies (into [a b c d] more)]
     (DerivedSignal. f dependencies (apply f @a @b @c @d (map deref more)) equals? {} (propagator f dependencies)))))

(defn smap
  "Like [[smap*]] but the equality function is always [[=]]."
  ([f] (smap* = f))
  ([f a] (smap* = f a))
  ([f a b] (smap* = f a b))
  ([f a b c] (smap* = f a b c))
  ([f a b c d] (smap* = f a b c d))
  ([f a b c d & more] (apply smap* = f a b c d more)))

