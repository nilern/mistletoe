(ns mistletoe-life.byte-matrix)

(defn- in-bounds? [rows cols i j]
  (and (< -1 i rows) (< -1 j cols)))

(defn- internal-index [cols i j]
  (+ (* cols i) j))

(defprotocol MatrixGet
  (mget [m i j]))

(defprotocol RowMatrix
  (mrow [m i])
  (rows [m]))

(defprotocol MatrixUpdate
  (mupdate [m i j f]))

(defprotocol MatrixSet
  (mset! [m i j v]))

(deftype RowSeq [cells start end]
  Object
  (toString [self] (pr-str* self))
  (equiv [self other] (-equiv self other))
  (indexOf [self x] (-indexOf self x 0))
  (indexOf [self x start] (-indexOf self x start))
  (lastIndexOf [self x] (-lastIndexOf self x (count self)))
  (lastIndexOf [self x start] (-lastIndexOf self x start))

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

  ICollection
  (-conj [self v] (cons v self))

  IEmptyableCollection
  (-empty [_] (.-EMPTY List))

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

  RowMatrix
  (mrow [_ i] (let [start (* cols i)
                    end (+ start cols)]
                (RowSeq. cells start end)))
  (rows [self] (map (partial mrow self) (range rows)))

  MatrixUpdate
  (mupdate [_ i j f]
    (let [cells* (js/Uint8Array. cells)
          i (internal-index cols i j)]
      (aset cells* i (f (aget cells* i)))
      (ByteMatrix. cells* rows cols))))

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

