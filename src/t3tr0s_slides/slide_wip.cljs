(ns t3tr0s-slides.slide-wip
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))

(def app (atom {:board
                [[ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 1 0 0 1 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 1 1 1 1 0 0 0]
                 [ 0 0 1 0 0 0 0 1 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 0 0 0]
                 [ 0 0 0 0 0 0 0 1 0 0]
                 [ 0 0 0 1 0 0 1 1 0 0]
                 [ 1 0 1 1 1 0 1 1 0 0]
                 [ 1 0 1 1 1 0 1 1 0 0]
                 [ 1 1 1 1 1 1 1 1 1 0]
                 [ 1 1 1 1 1 1 1 1 1 0]
                 [ 1 1 1 1 1 1 1 1 1 0]
                 [ 1 1 1 1 1 1 1 1 1 0]]}))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")
(def dark-red "#944")
(def light-red "#f8c")

(def rows 20)
(def cols 10)

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
     (sx/cmt "; Sorry, I'm in the process of fixing these slides.") "\n"
     (sx/cmt "; Not really sure what happened, but the rest of") "\n"
     (sx/cmt "; the slides will be back up soon.  Thanks!") "\n"]]])

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

(defn piece-abs-coords
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) piece))

(defn draw-piece!
  [ctx piece pos]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos))))

(defn draw-board!
  [ctx board]
  (doseq [y (range rows)
          x (range cols)]
    (let [v (get-in board [y x])]
      (when-not (zero? v)
        (draw-cell! ctx [x y] false)))))

(defn draw-canvas!
  [canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (set! (.. ctx -fillStyle) dark-red)
    (set! (.. ctx -strokeStyle) light-red)
    (draw-board! ctx (:board @app))))

(def global-canvas)
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
     :style {:position "relative"}}]])

(rum/defc slide []
  [:div
   [:h1 "Trying to fix the rest!"]
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
