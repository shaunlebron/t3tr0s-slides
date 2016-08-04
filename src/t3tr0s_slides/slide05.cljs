(ns t3tr0s-slides.slide05
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))

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
(def g10 (update-in g9 [:position] move-down))
(def g11 (update-in g10 [:position] move-down))
(def g12 (update-in g11 [:position] move-down))
(def g13 (update-in g12 [:position] move-down))
(def g14 (update-in g13 [:position] move-down))
(def g15 (update-in g14 [:position] move-down))

(def states [g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14 g15])

(def state-places
  {g0  0
   g1  1
   g2  2
   g3  3
   g4  4
   g5  5
   g6  6
   g7  7
   g8  8
   g9  9
   g10 10
   g11 11
   g12 12
   g13 13
   g14 14
   g15 15})

(defn piece-abs-coords
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) piece))

(def app (atom {:piece nil
                :position nil}))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn state-code
  [state code-str]
  [:span
   {:key (str "state" code-str)
    :class (if (= state @app) "active-col-d9099" "")
    :onMouseEnter #(reset! app state)}
   code-str])

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
     "(" (sx/core "defn") " move-left  [[x y]] [ (" (sx/core "dec") " x)      y  ])\n"
     "(" (sx/core "defn") " move-right [[x y]] [ (" (sx/core "inc") " x)      y  ])\n"
     "(" (sx/core "defn") " move-down  [[x y]] [      x  (" (sx/core "inc") " y) ])\n"
     "\n\n"
     (sx/cmt "; TRY IT: mouse over the states.") "\n\n"
     (state-code g0  (list "(" (sx/core "def") " g0  {" (sx/kw ":position") " [" (sx/lit "4") " " (sx/lit "6") "] :piece (" (sx/kw ":J") " pieces)})\n"))
     (state-code g1  (list "(" (sx/core "def") " g1  (" (sx/core "update-in") " g0  [" (sx/kw ":position") "] move-left))\n"))
     (state-code g2  (list "(" (sx/core "def") " g2  (" (sx/core "update-in") " g1  [" (sx/kw ":position") "] move-left))\n"))
     (state-code g3  (list "(" (sx/core "def") " g3  (" (sx/core "update-in") " g2  [" (sx/kw ":piece") "]    rotate-piece))\n"))
     (state-code g4  (list "(" (sx/core "def") " g4  (" (sx/core "update-in") " g3  [" (sx/kw ":position") "] move-down))\n"))
     (state-code g5  (list "(" (sx/core "def") " g5  (" (sx/core "update-in") " g4  [" (sx/kw ":position") "] move-down))\n"))
     (state-code g6  (list "(" (sx/core "def") " g6  (" (sx/core "update-in") " g5  [" (sx/kw ":piece") "]    rotate-piece))\n"))
     (state-code g7  (list "(" (sx/core "def") " g7  (" (sx/core "update-in") " g6  [" (sx/kw ":position") "] move-right))\n"))
     (state-code g8  (list "(" (sx/core "def") " g8  (" (sx/core "update-in") " g7  [" (sx/kw ":position") "] move-right))\n"))
     (state-code g9  (list "(" (sx/core "def") " g9  (" (sx/core "update-in") " g8  [" (sx/kw ":piece") "]    rotate-piece))\n"))
     (state-code g10 (list "(" (sx/core "def") " g10 (" (sx/core "update-in") " g9  [" (sx/kw ":position") "] move-down))\n"))
     (state-code g11 (list "(" (sx/core "def") " g11 (" (sx/core "update-in") " g10 [" (sx/kw ":position") "] move-down))\n"))
     (state-code g12 (list "(" (sx/core "def") " g12 (" (sx/core "update-in") " g11 [" (sx/kw ":position") "] move-down))\n"))
     (state-code g13 (list "(" (sx/core "def") " g13 (" (sx/core "update-in") " g12 [" (sx/kw ":position") "] move-down))\n"))
     (state-code g14 (list "(" (sx/core "def") " g14 (" (sx/core "update-in") " g13 [" (sx/kw ":position") "] move-down))\n"))
     (state-code g15 (list "(" (sx/core "def") " g15 (" (sx/core "update-in") " g14 [" (sx/kw ":position") "] move-down))\n"))]]])


(def cell-size (quot 600 rows))

(defn draw-cell!
  [ctx [x y] is-center]
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



(defn draw-piece!
  [ctx piece pos]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos))))

(defn draw-canvas!
  [canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (let [piece (:piece @app)
          pos (:position @app)]
      (when (and piece pos)
        (let [places (state-places @app)]
          (set! (.. ctx -fillStyle) "#555")
          (set! (.. ctx -strokeStyle) "#AAA")
          (doseq [[i {:keys [piece position]}] (map-indexed vector (take places states))]
            (let [x 7]
              (set! (.. ctx -globalAlpha) (/ (max 0 (min x (- i (- places x)))) x 5)))
            (draw-piece! ctx piece position)))

        (set! (.. ctx -globalAlpha) 1)
        (set! (.. ctx -fillStyle) dark-purple)
        (set! (.. ctx -strokeStyle) light-purple)
        (draw-piece! ctx piece pos)))))

(def canvas-mixin
  {:did-mount
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
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
  [:div.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}}]])

(rum/defc slide []
  [:div
   [:h1 "5. Test state progression."]
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
