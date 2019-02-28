(ns mistletoe-deck.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.signal.util :refer [alloc-watch-key map-key-cached]]
            [mistletoe.dom :refer [el append-child! add-watchee!]]))

(defn- rand-in-range [min max]
  (->> (.random js/Math)
       (* (- max min))                                      ; zoom
       (.floor js/Math)                                     ; clip
       (+ min)))                                            ; translate

(def ^:private framerate 2)

;;;;

(defn- rank-symbol [rank]
  (case rank
    1 "A"
    11 "J"
    12 "Q"
    13 "K"
    (str rank)))

(def ^:private suits [:clubs :diamonds :hearts :spades])

(def ^:private suit-symbol
  {:clubs    "\u2663"
   :diamonds "\u2666"
   :hearts   "\u2665"
   :spades   "\u2660"})

(def ^:private suit-color
  {:clubs    "black"
   :diamonds "crimson"
   :hearts   "crimson"
   :spades   "black"})

(def ^:private nsuits (count suits))
(def ^:private nranks 13)
(def ^:private deck-len (* nsuits nranks))

(defrecord Card [suit rank]
  Object
  (toString [_] (str (rank-symbol rank) (suit-symbol suit))))

(defn- ic-by-initial-card [initial-card deck]
  (some (fn [[_i card :as ic]] (and (= card initial-card) ic))
        (map-indexed vector deck)))

;;;;

(def ^:private state
  (sgn/source {:phase       :select
               :phase-state nil
               :deck        (into [] (for [suit suits
                                           rank (range 1 (inc nranks))]
                                       (->Card suit rank)))}))

(def ^:private editable? (smap #(= (:phase %) :select) state))

;;;;

(defn- card-view [ic]
  (let [i (smap first ic)
        card (smap second ic)
        view (el :div :class "card" :id (str @card)
                 :style {:color      (smap (comp suit-color :suit) card)
                         :transform  (str "translate(" (* 35 @i) "px)")
                         :transition (str "transform " (/ 1000 framerate 2) "ms ease-in-out")}
                 (smap str card))]
    ;; HACK: Force CSS transition to trigger by letting the element have the old transform when it is re-inserted
    ;;       to a different location in the DOM:
    (let [wk (alloc-watch-key)
          wf (fn [_ _ _ i]
               (.setTimeout js/window
                            (fn [] (set! (.. view -style -transform) (str "translate(" (* 35 i) "px)")))
                            (/ 1000 framerate 4)))]
      (add-watchee! view i wk wf)
      (add-watch i wk wf))
    view))

(defn- ui-main [state]
  (let [deck (smap :deck state)]
    (el :div
        (el :div
            (el :input :type "button" :value "Fisher-Yates shuffle"
                :onclick (fn [_]
                           (let [step (fn [] (swap! state
                                                    (fn [state]
                                                      (let [i (-> state :phase-state :index)]
                                                        (if (< i (dec deck-len))
                                                          (let [j (rand-in-range i deck-len)]
                                                            (-> state
                                                                (assoc-in [:deck i] (get-in state [:deck j]))
                                                                (assoc-in [:deck j] (get-in state [:deck i]))
                                                                (update-in [:phase-state :index] inc)))
                                                          (do (.clearInterval js/window (:interval-id state))
                                                              (assoc state :phase :select, :phase-state nil)))))))]
                             (when @editable?
                               (swap! state assoc :phase :fisher-yates)
                               (swap! state assoc :phase-state {:index 0})
                               (swap! state assoc :interval-id (.setInterval js/window step (/ 1000 framerate))))))))

        (el :div
            (let [ics (smap (fn [initial-deck]
                              (for [initial-card initial-deck]
                                (smap (partial ic-by-initial-card initial-card) deck)))
                            deck)]
              (smap (map-key-cached (fn [_ ic] (second @ic))
                                    (fn [ic] (card-view ic)))
                    ics))))))

;;;;

(defn- main []
  (let [parent (.getElementById js/document "app-root")]
    (append-child! parent (ui-main state))))
