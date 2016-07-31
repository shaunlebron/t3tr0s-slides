(ns t3tr0s-slides.slide03
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))

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
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) (pieces piece)))

(def initial-pos [4 6])
(def app (atom {:piece-name :J
                :position initial-pos
                :highlight nil}))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn piece-code []
  [:span
   {:class (if (= (:highlight @app) :piece) "clickable-ae1bb" "")
    :onMouseEnter #(swap! app assoc :highlight :piece)
    :onMouseLeave #(swap! app assoc :highlight nil)
    :onClick #(swap! app update :piece-name next-piece)}
   (list "(" (sx/kw (str (:piece-name @app))) " pieces)")])

(defn full-piece [piece]
  (let [pad #(if (neg? %) % (str " " %))
        fmt #(sx/lit (pad %))
        fmt-coord (fn [[x y]]
                    (list "[" (fmt x) " " (fmt y) "]"))]
    (interpose " " (map fmt-coord piece))))


(defn position-code []
  [:span
   {:class (if (= (:highlight @app) :position) "active-col-d9099" "")}
   (let [[x y] (:position @app)]
     (list "[" (sx/lit x) " " (sx/lit y) "]"))])

(rum/defc code []
  [:div.code-cb62a
   [:pre
    [:code
     "(" (sx/core "def") " initial-state {" (sx/kw ":board") " empty-board\n"
     "                    " (sx/kw ":piece") " " (sx/lit "nil") "\n"
     "                    " (sx/kw ":position") " " (sx/lit "nil") "})\n"
     "\n"
     (sx/cmt "; TRY IT: click piece below to change,") "\n"
     (sx/cmt "; and mouse over the canvas to move.") "\n"
     "\n"
     "> (" (sx/core "assoc") " initial-state " (sx/kw ":piece") " " (piece-code) "\n"
     "                       " (sx/kw ":position") " " (position-code) ")\n"
     "\n"
     "  {" (sx/kw ":piece") " " [:span {:class (if (= (:highlight @app) :piece) "active-col-d9099" "")}
                                 (full-piece (pieces (:piece-name @app)))] "\n"
     "   " (sx/kw ":position") " " (position-code) "\n"
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
     "           [ 0 0 0 0 0 0 0 0 0 0 ]]}\n"]]])

(def cell-size (quot 600 rows))

(def global-canvas)
(defn canvas-mouse [e]
  (let [canvas global-canvas
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left rect))
        y (- (.-clientY e) (.-top rect))
        col (quot x cell-size)
        row (quot y cell-size)]
    (swap! app assoc :position [col row])))

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
    (draw-piece! ctx (:piece-name @app) (:position @app))))

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
  [:div.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}
     :onMouseMove #(do (canvas-mouse %)
                       (swap! app assoc :highlight :position))
     :onMouseLeave #(do (swap! app assoc :position initial-pos)
                        (swap! app assoc :highlight nil))}]])

(rum/defc slide []
  [:div
   [:h1 "3. Create initial state."]
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
