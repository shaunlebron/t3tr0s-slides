(ns t3tr0s-slides.slide03
  (:require
    [om.core :as om :include-macros true]
    [om-tools.core :refer-macros [defcomponent]]
    [sablono.core :refer-macros [html]]
    [t3tr0s-slides.syntax-highlight :as sx]
    ))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")

(def next-piece
  {:I :T
   :T :O
   :O :J
   :J :L
   :L :S
   :S :Z
   :Z :I})

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
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) (pieces piece)))

(def initial-pos [4 6])
(def app-state (atom {:piece-name :J
                      :position initial-pos
                      :highlight nil
                      }))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn piece-code
  [app]
  [:span
   {:class (if (= (:highlight app) :piece) "clickable-ae1bb" "")
    :onMouseEnter #(om/update! app :highlight :piece)
    :onMouseLeave #(om/update! app :highlight nil)
    :onClick #(om/transact! app :piece-name next-piece)
    }
   (list "(" (sx/kw (str (:piece-name app))) " pieces)")]
  )

(defn full-piece
  [piece]
  (let [pad #(if (neg? %) % (str " " %))
        fmt #(sx/lit (pad %))
        fmt-coord (fn [[x y]]
                    (list "[" (fmt x) " " (fmt y) "]"))]
    (interpose " " (map fmt-coord piece))))
  

(defn position-code
  [app]
  [:span
   {:class (if (= (:highlight app) :position) "active-col-d9099" "")}
   (let [[x y] (:position app)]
     (list "[" (sx/lit x) " " (sx/lit y) "]"))
   ])

(defcomponent code
  [app owner]
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code
         "(" (sx/core "def") " initial-state {" (sx/kw ":board") " empty-board\n"
         "                    " (sx/kw ":piece") " " (sx/lit "nil") "\n"
         "                    " (sx/kw ":position") " " (sx/lit "nil") "})\n"
         "\n"
         "> (" (sx/core "assoc") " initial-state " (sx/kw ":piece") " " (piece-code app) "\n"
         "                       " (sx/kw ":position") " " (position-code app) ")\n"
         "\n"
         "  {" (sx/kw ":piece") " " [:span {:class (if (= (:highlight app) :piece) "active-col-d9099" "")}
                       (full-piece (pieces (:piece-name app)))] "\n"
         "   " (sx/kw ":position") " " (position-code app) "\n"
         "   " (sx/kw ":board") " [[ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]\n"
         "           [ 0 0 0 0 0 0 0 0 0 0 ]]}\n"
         ]]])))

(def cell-size (quot 600 rows))

(defn canvas-mouse
  [app owner e]
  (let [canvas (om/get-node owner)
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left rect) 20)
        y (- (.-clientY e) (.-top rect) 20)
        col (quot x cell-size)
        row (quot y cell-size)]
    (om/update! app :position [col row])))

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
  [ctx piece pos]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos)))
  )

(defn draw-canvas!
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (draw-piece! ctx (:piece-name app) (:position app))
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
         :onMouseMove #(do (canvas-mouse app owner %)
                           (om/update! app :highlight :position))
         :onMouseLeave #(do (om/update! app :position initial-pos)
                            (om/update! app :highlight nil))
         }
        ]])))

(defcomponent slide
  [app owner]
  (render
    [_]
    (html
      [:div
       [:h1 "3. Create initial state."]
       (om/build code app)
       (om/build canvas app)])))

(defn init
  [id]
  (om/root
    slide
    app-state
    {:target (. js/document (getElementById id))}))

(defn resume
  []
  )

(defn stop
  []
  )
