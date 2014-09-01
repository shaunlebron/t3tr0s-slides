(ns t3tr0s-slides.slide2
  (:require
    [om.core :as om :include-macros true]
    [om-tools.core :refer-macros [defcomponent]]
    [sablono.core :refer-macros [html]]
    ))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")

(def piece-keys
  [:I :T :O :J :L :S :Z])

(def positions
  {:I [4 1 ]
   :T [4 4 ]
   :O [4 7 ]
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

(def app-state (atom {:piece nil :index nil}))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn data-row
  [piece app]
  [:span
   {:key (str "piece" piece)
    :class (if (= piece (:piece app)) "active-row-534ed" "")
    :onMouseEnter #(om/update! app :piece piece)
    :onMouseLeave #(om/update! app :piece nil)
    }
    "["
    (for [[index [x y]] (map-indexed vector (pieces piece))]
      [:span 
       {:key (str "piece" piece "index" index)
        :class (if (and (= piece (:piece app))
                        (= index (:index app))) "active-col-d9099")
        :onMouseEnter #(om/update! app :index index)
        :onMouseLeave #(om/update! app :index nil)
        }
       (let [pad #(if (neg? %) % (str " " %))]
         (str " [" (pad x) " " (pad y) "]"))])
    " ]"])

(defcomponent code
  [app owner]
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code
         "(def pieces\n"
         (let [ps piece-keys
               first-p (first ps)
               last-p (last ps)]
           (for [p ps]
             (condp = p
               first-p    (list "  {" (str p " ") (data-row p app) "\n")
               last-p     (list "   " (str p " ") (data-row p app) "})\n")
                          (list "   " (str p " ") (data-row p app) "\n"))))
         "\n\n"
         (when-let [p (:piece app)]
           (list "; piece = " (str p) "\n"
           (when-let [i (:index app)]
             (list "; coord = " (str (nth (pieces p) i)) "\n"))))
         ]]])))

(def cell-size (quot 600 rows))

(defn piece-index
  [x y]
  (some identity
        (map #(first (keep-indexed
                       (fn [i [px py]]
                         (when (and (= px x) (= py y))
                           [% i]))
                       (piece-abs-coords %)))
             (keys pieces))))

(defn canvas-mouse
  [app owner e]
  (let [canvas (om/get-node owner)
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left rect) 20)
        y (- (.-clientY e) (.-top rect) 20)
        col (quot x cell-size)
        row (quot y cell-size)
        [piece index] (piece-index col row)]
    (om/update! app :piece piece)
    (om/update! app :index index)))

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
        cr (/ cell-size 4)
        ]
    (.. ctx (fillRect rx ry rs rs))
    (.. ctx (strokeRect rx ry rs rs))
    (when is-center
      (.. ctx beginPath)
      (.. ctx (arc cx cy cr 0 (* 2 (.-PI js/Math))))
      (.. ctx fill)
      (.. ctx stroke)
      )
    ))

(defn draw-piece!
  [app ctx piece]
  (let [is-piece (= piece (:piece app))
        index (and is-piece (:index app))
        center (positions piece)]
    (doseq [[i c] (map-indexed vector (piece-abs-coords piece))]
      (when-not (= i index)
        (draw-cell! ctx c is-piece (= i index) (= c center))))
    (doseq [[i c] (map-indexed vector (piece-abs-coords piece))]
      (when (= i index)
        (draw-cell! ctx c is-piece (= i index) (= c center))))
    ))

(defn draw-canvas!
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#111")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (doseq [p piece-keys]
      (draw-piece! app ctx p))
    ))

(defcomponent canvas
  [app owner]
  (did-mount [_]
    (let [canvas (om/get-node owner "canvas")]
      (set! (.. canvas -width) (* cols cell-size))
      (set! (.. canvas -height) (* rows cell-size))
      (draw-canvas! app (om/get-node owner "canvas"))
      ))
  (did-update [_ _ _]
    (draw-canvas! app (om/get-node owner "canvas"))
    )
  (render [_]
    (html
      [:div.canvas-2a4d7
       [:canvas
        {:ref "canvas"
         :style {:position "relative"}
         :onMouseMove #(canvas-mouse app owner %)
         :onMouseLeave #(do (om/update! app :row nil)
                            (om/update! app :col nil))
         }
        ]])))

(defcomponent slide
  [app owner]
  (render
    [_]
    (html
      [:div
       (om/build code app)
       (om/build canvas app)])))

(defn init
  []
  (om/root
    slide
    app-state
    {:target (. js/document (getElementById "app"))}))
