(ns t3tr0s-slides.slide5
  (:require
    [om.core :as om :include-macros true]
    [om-tools.core :refer-macros [defcomponent]]
    [sablono.core :refer-macros [html]]
    ))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")

(def pieces
  {:I [[-1  0] [ 0  0] [ 1  0] [ 2  0]]
   :L [[ 1 -1] [-1  0] [ 0  0] [ 1  0]]
   :J [[-1 -1] [-1  0] [ 0  0] [ 1  0]]
   :S [[ 0 -1] [ 1 -1] [-1  0] [ 0  0]]
   :Z [[-1 -1] [ 0 -1] [ 0  0] [ 1  0]]
   :O [[ 0 -1] [ 1 -1] [ 0  0] [ 1  0]]
   :T [[ 0 -1] [-1  0] [ 0  0] [ 1  0]]})

(defn rotate-coord [[x y]] [(- y) x])
(defn rotate-piece [piece] (mapv rotate-coord piece))
(defn move-left [[x y]] [(dec x) y])
(defn move-right [[x y]] [(inc x) y])
(defn move-down [[x y]] [x (inc y)])

(def g0 {:position [4 6] :piece (:J pieces)})
(def g1 (update-in g0 [:position] move-left))
(def g2 (update-in g1 [:position] move-left))
(def g3 (update-in g2 [:piece] rotate-piece))
(def g4 (update-in g3 [:position] move-down))
(def g5 (update-in g4 [:position] move-down))
(def g6 (update-in g5 [:piece] rotate-piece))
(def g7 (update-in g6 [:position] move-right))
(def g8 (update-in g7 [:position] move-right))
(def g9 (update-in g8 [:piece] rotate-piece))
(def g10 (update-in g9 [:piece] rotate-piece))
(def g11 (update-in g10 [:piece] rotate-piece))

(defn rotate-coord [[x y]] [(- y) x])
(defn rotate-piece [piece] (mapv rotate-coord piece))

(defn piece-abs-coords
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) piece))

(def app-state (atom {:piece nil
                      :position nil}))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn state-code
  [app state code-str]
  [:span
   {:key (str "state" code-str)
    :class (if (= state app) "active-col-d9099" "")
    :onMouseEnter #(om/update! app state)}
   code-str])

(defcomponent code
  [app owner]
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code
         "(defn move-left  [pos] [ (dec x)      y  ])\n"
         "(defn move-right [pos] [ (inc x)      y  ])\n"
         "(defn move-down  [pos] [      x  (inc y) ])\n"
         "\n\n"
         (state-code app g0  "(def g0  {:position [4 6] :piece (:J pieces)})\n")
         "\n"
         (state-code app g1  "(def g1  (update-in g0  [:position] move-left))\n")
         "\n"
         (state-code app g2  "(def g2  (update-in g1  [:position] move-left))\n")
         "\n"
         (state-code app g3  "(def g3  (update-in g2  [:piece]    rotate-piece))\n")
         "\n"
         (state-code app g4  "(def g4  (update-in g3  [:position] move-down))\n")
         "\n"
         (state-code app g5  "(def g5  (update-in g4  [:position] move-down))\n")
         "\n"
         (state-code app g6  "(def g6  (update-in g5  [:piece]    rotate-piece))\n")
         "\n"
         (state-code app g7  "(def g7  (update-in g6  [:position] move-right))\n")
         "\n"
         (state-code app g8  "(def g8  (update-in g7  [:position] move-right))\n")
         "\n"
         (state-code app g9  "(def g9  (update-in g8  [:piece]    rotate-piece))\n")
         "\n"
         (state-code app g10 "(def g10 (update-in g9  [:piece]    rotate-piece))\n")
         "\n"
         (state-code app g11 "(def g11 (update-in g10 [:piece]    rotate-piece))\n")
         ]]])))

(def cell-size (quot 600 rows))

(defn draw-cell!
  [ctx [x y] is-center]
  (set! (.. ctx -fillStyle) dark-purple)
  (set! (.. ctx -strokeStyle) light-purple)
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
  [app ctx piece pos]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos))))

(defn draw-canvas!
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (let [piece (:piece app)
          pos (:position app)]
      (when (and piece pos)
        (draw-piece! app ctx piece pos)))
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
