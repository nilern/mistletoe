(ns mistletoe-life.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.signal.util :refer [seqsig->sigseq]]
            [mistletoe.dom :refer [el]]))

;;;;

(def ^:private width 80)
(def ^:private height 80)
(def ^:private framerate 2)

;;;;

(def ^:private live true)
(def ^:private dead false)

(def ^:private live? identity)

(defn- empty-row [width]
  (mapv (constantly dead) (range width)))

(defn- empty-grid [width height]
  (mapv (fn [_] (empty-row width)) (range height)))

(defn- next-cells [cells]
  (let [get-cell (fn [i j] (some-> cells (get i) (get j)))
        get-cell-int (fn [i j] (if (get-cell i j) 1 0))
        live-neighbours (fn [i j]
                          (+ (get-cell-int (dec i) (dec j))
                             (get-cell-int (dec i) j)
                             (get-cell-int (dec i) (inc j))
                             (get-cell-int i (dec j))
                             (get-cell-int i (inc j))
                             (get-cell-int (inc i) (dec j))
                             (get-cell-int (inc i) j)
                             (get-cell-int (inc i) (inc j))))
        next-cell (fn [cell i j]
                    (if (live? cell)
                      (case (live-neighbours i j)
                        (0 1) dead ; underpopulation
                        (2 3) live
                        dead) ; overpopulation
                      (if (= (live-neighbours i j) 3)
                        live                                ; reproduction
                        dead)))
        next-row (fn [row i]
                   (into [] (map-indexed (fn [j cell] (next-cell cell i j))) row))]
    (into [] (map-indexed #(next-row %2 %1)) cells)))

;;;;

(defn- next-phase [current-phase]
  (case current-phase
    :edit :run
    :run :edit))

(def ^:private state
  (sgn/source {:phase :edit
               :cells (empty-grid width height)}))

(def ^:private editable? (smap #(= (:phase %) :edit) state))

;;;;

(defn- row-view [i row]
  (el :tr
      (for [[j cell] (map-indexed vector (seqsig->sigseq row))]
        (el :td
            :style {:width            "15px", :height "15px"
                    :border           "1px solid black"
                    :background-color (smap #(if % "black" "transparent") cell)}
            :onclick (fn [_]
                       (when @editable?
                         (swap! state update-in [:cells i j] not)))))))

(defn- ui-main [state]
  (el :div
      (el :input :type "button" :value (smap (comp name next-phase :phase) state)
          :onclick (fn [_]
                     (case (:phase (swap! state update :phase next-phase))
                       :run (swap! state assoc :interval-id
                                   (.setInterval js/window
                                                 (fn [] (swap! state update :cells next-cells))
                                                 (/ 1000 framerate)))
                       :edit (.clearInterval js/window (:interval-id @state)))))

      (el :input :type "button" :value "clear"
          :onclick (fn [_]
                     (when @editable?
                       (swap! state assoc :cells (empty-grid width height)))))

      (el :table
          :style {:border-collapse "collapse"}
          (for [[i row] (map-indexed vector (seqsig->sigseq (smap :cells state)))]
            (row-view i row)))))

;;;;

(defn main []
  (.. js/document
      (getElementById "app-root")
      (appendChild (ui-main state))))
