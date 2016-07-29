(ns t3tr0s-slides.slide02
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))


(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")

(def piece-keys
  [:I :T :O :J :L :S :Z])

(def positions
  {:I [4 1]
   :T [4 4]
   :O [4 7]
   :J [4 10]
   :L [4 13]
   :S [4 16]
   :Z [4 19]})

(def pieces
  {:I [[-1  0] [ 0  0] [ 1  0] [ 2  0]]
   :L [[ 1 -1] [-1  0] [ 0  0] [ 1  0]]
   :J [[-1 -1] [-1  0] [ 0  0] [ 1  0]]
   :S [[ 0 -1] [ 1 -1] [-1  0] [ 0  0]]
   :Z [[-1 -1] [ 0 -1] [ 0  0] [ 1  0]]
   :O [[ 0 -1] [ 1 -1] [ 0  0] [ 1  0]]
   :T [[ 0 -1] [-1  0] [ 0  0] [ 1  0]]})

(defn piece-abs-coords
  [piece]
  (let [[cx cy] (positions piece)]
    (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) (pieces piece))))

(def app (atom {:piece nil :index nil}))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn data-row
  [piece]
  [:span
   {:key (str "piece" piece)
    :class (if (= piece (:piece @app)) "active-row-534ed" "")
    :onMouseEnter #(swap! app assoc :piece piece)}

   "["
    (for [[index [x y]] (map-indexed vector (pieces piece))]
      [:span
       {:key (str "piece" piece "index" index)
        :class (if (and (= piece (:piece @app))
                        (= index (:index @app))) "active-col-d9099")
        :onMouseEnter #(swap! app assoc :index index)}

       (let [pad #(if (neg? %) % (str " " %))
             fmt #(sx/lit (pad %))]
         (list " [" (fmt x) " " (fmt y) "]"))])
    " ]"])

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
     (sx/cmt "; TRY IT: mouse over the pieces.") "\n"
     "\n"
     "(" (sx/core "def") " pieces\n"
     (let [ps piece-keys
           first-p (first ps)
           last-p (last ps)]
       (for [p ps]
         (condp = p
           first-p    (list "  {" (sx/kw (str p)) " " (data-row p) "\n")
           last-p     (list "   " (sx/kw (str p)) " " (data-row p) "})\n")
                      (list "   " (sx/kw (str p)) " " (data-row p) "\n"))))
     "\n\n"
     (when-let [p (:piece @app)]
       (list (sx/cmt "; piece = " (str p)) "\n"
        (when-let [i (:index @app)]
          (list (sx/cmt "; coord = " (str (nth (pieces p) i))) "\n"))))]]])

(def cell-size (quot 600 rows))

(defn piece-index [x y]
  (some identity
        (map #(first (keep-indexed
                       (fn [i [px py]]
                         (when (and (= px x) (= py y))
                           [% i]))
                       (piece-abs-coords %)))
             (keys pieces))))

(def global-canvas)

(defn canvas-mouse [e]
  (let [canvas global-canvas
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left rect))
        y (- (.-clientY e) (.-top rect))
        col (quot x cell-size)
        row (quot y cell-size)
        [piece index] (piece-index col row)]
    (when (and piece index)
      (swap! app assoc :piece piece)
      (swap! app assoc :index index))))

(defn draw-cell!
  [ctx [x y] is-piece is-index is-center]
  (set! (.. ctx -fillStyle)
        (cond is-index dark-purple
              is-piece dark-green
              :else "transparent"))
  (set! (.. ctx -strokeStyle)
        (cond is-index light-purple
              is-piece light-green
              :else "#888"))
  (set! (.. ctx -lineWidth) 2)
  (let [rx (* cell-size x)
        ry (* cell-size y)
        rs cell-size
        cx (* cell-size (+ x 0.5))
        cy (* cell-size (+ y 0.5))
        cr (/ cell-size 4)]

    (.. ctx (fillRect rx ry rs rs))
    (.. ctx (strokeRect rx ry rs rs))
    (when is-center
      (.. ctx beginPath)
      (.. ctx (arc cx cy cr 0 (* 2 (.-PI js/Math))))
      (.. ctx fill)
      (.. ctx stroke))))

(defn draw-piece! [ctx piece]
  (let [is-piece (= piece (:piece @app))
        index (and is-piece (:index @app))
        center (positions piece)]
    (doseq [[i c] (map-indexed vector (piece-abs-coords piece))]
      (when-not (= i index)
        (draw-cell! ctx c is-piece (= i index) (= c center))))
    (doseq [[i c] (map-indexed vector (piece-abs-coords piece))]
      (when (= i index)
        (draw-cell! ctx c is-piece (= i index) (= c center))))))

(defn draw-canvas! [canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (doseq [p piece-keys]
      (draw-piece! ctx p))))

(def canvas-mixin
  {:did-mount
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (set! global-canvas canvas)
      (set! (.. canvas -width) (* cols cell-size))
      (set! (.. canvas -height) (* rows cell-size))
      (draw-canvas! canvas)
      state))
   :did-update
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (draw-canvas! canvas)
      state))})

(rum/defc canvas < canvas-mixin []
  [:.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}
     :onMouseMove canvas-mouse}]])

(rum/defc slide []
  [:div
   [:h1 "2. Create the pieces."]
   (code)
   (canvas)])

(def slide-elm)
(defn render []
  (rum/mount (slide) slide-elm))

(defn init [id]
  (set! slide-elm (js/document.getElementById id))
  (render)
  (add-watch app :render render))

(defn resume [])
(defn stop [])
