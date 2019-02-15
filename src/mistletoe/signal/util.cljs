(ns mistletoe.signal.util
  (:require [mistletoe.signal :refer [smap]]))

(defprotocol KeyDepot
  (-alloc-key [self])
  (-return-key [self k]))

(deftype PerpetualKeyDepot [^:mutable used, ^:mutable counter]
  KeyDepot
  (-alloc-key [_]
    (if (seq used)
      (let [k (peek used)]
        (set! used (pop used))
        k)
      (let [k counter]
        (set! counter (inc k))
        k)))

  (-return-key [_ k] (set! used (conj used k))))

(def ^:private key-depot (->PerpetualKeyDepot #queue [] 0))

(defn alloc-watch-key [] (-alloc-key key-depot))

(defn free-watch-key [k] (-return-key key-depot k))

(defn seqsig->sigseq
  "Make a sequence of signals for the elements of the
  signal of sequnces `seqsig`."
  [seqsig]
  (map-indexed (fn [i _] (smap #(nth % i) seqsig)) @seqsig))
