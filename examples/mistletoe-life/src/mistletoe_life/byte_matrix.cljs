(ns mistletoe-life.byte-matrix)

(defn- in-bounds? [rows cols i j]
  (and (< -1 i rows) (< -1 j cols)))

(defn- internal-index [cols i j]
  (+ (* cols i) j))

(defprotocol MatrixGet
  (mget [m i j]))

(defprotocol MatrixRow
  (mrow [m i]))

(defprotocol MatrixSet
  (mset! [m i j v]))

(deftype RowSeq [cells start end]
  ICloneable
  (-clone [_] (RowSeq. cells start end))

  ISeqable
  (-seq [self] (when (< start end) self))

  ASeq
  ISeq
  (-first [_] (aget cells start))
  (-rest [_] (if (< (inc start) end)
               (RowSeq. cells (inc start) end)
               (list)))

  INext
  (-next [_] (when (< (inc start) end)
               (RowSeq. cells (inc start) end)))

  ICounted
  (-count [_] (- end start))

  IIndexed
  (-nth [_ n]
    (let [i (+ start n)]
      (if (< (dec start) i end)
        (aget cells i)
        (throw (js/Error. "Index out of bounds")))))
  (-nth [_ n not-found]
    (let [i (+ start n)]
      (if (< (dec start) i end)
        (aget cells i)
        not-found)))

  ISequential
  IEquiv
  (-equiv [self other] (equiv-sequential self other))

  IReduce
  (-reduce [_ f]
    (loop [i (inc start), acc (aget cells start)]
      (if (< i end)
        (recur (inc i) (f acc (aget cells i)))
        acc)))
  (-reduce [_ f acc]
    (loop [i start, acc acc]
      (if (< i end)
        (recur (inc i) (f acc (aget cells i)))
        acc))))

(deftype ByteMatrix [cells rows cols]
  MatrixGet
  (mget [_ i j]
    (when (in-bounds? rows cols i j)
      (aget cells (internal-index cols i j))))

  MatrixRow
  (mrow [_ i] (let [start (* cols i)
                    end (+ start cols)]
                (RowSeq. cells start end))))

(defn byte-matrix [rows cols]
  (ByteMatrix. (js/Uint8Array. (* rows cols)) rows cols))

(deftype TransientByteMatrix [cells rows cols editable?]
  MatrixSet
  (mset! [self i j v]
    {:pre [editable?]}
    (when (in-bounds? rows cols i j)
      (aset cells (internal-index cols i j) v))
    self))

(defn transient-byte-matrix [rows cols]
  (TransientByteMatrix. (js/Uint8Array. (* rows cols)) rows cols true))

(defn persistent-byte-matrix! [m]
  (set! (.-editable? m) false)
  (ByteMatrix. (.-cells m) (.-rows m) (.-cols m)))

