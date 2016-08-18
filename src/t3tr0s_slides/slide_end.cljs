(ns t3tr0s-slides.slide-end
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")
(def dark-red "#944")
(def light-red "#f8c")

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

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))
(def filled-board
  [[ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 1 0 0 1 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 1 0 0 0 0 1 0 0]
   [ 0 0 0 1 1 1 1 0 0 0]
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
   [ 1 1 1 1 1 1 1 1 1 0]])

(def initial-pos [4 6])

(def app (atom {:board filled-board
                :piece (last (take 4 (iterate rotate-piece (:L pieces))))
                :position initial-pos}))

(defn write-piece
  [board coords [cx cy]]
  (if-let [[x y] (first coords)]
    (recur (try (assoc-in board [(+ y cy) (+ x cx)] 1)
                (catch js/Error _ board))
           (rest coords)
           [cx cy])
    board))

(defn lock-piece! []
  (let [{:keys [piece position]} @app]
    (swap! app
      update-in [:board]
        write-piece piece position)))

(defn piece-fits?
  [board piece [cx cy]]
  (every? (fn [[x y]]
            (zero? (get-in board [(+ y cy) (+ x cx)])))
          piece))

(defn app-piece-fits?
  []
  (boolean (piece-fits? (:board @app) (:piece @app) (:position @app))))

(defn data-row
  [row]
  [:span
    "["
    (for [col (range cols)]
      (str " " (get-in @app [:board row col])))
    " ]"])

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
     (sx/cmt ";; Well, that covers the main steps we followed") "\n"
     (sx/cmt ";; when creating our full Tetris game.") "\n"
     "\n"
     (sx/cmt ";; Check out " (sx/kw [:a {:href "https://github.com/imalooney/t3tr0s"} "T3TR0S"]) " to learn even more!") "\n"
     "\n"
     (sx/cmt ";; Thanks to these cool people:") "\n"
     "\n"
     (sx/cmt ";;  - " (sx/lit "Elaine Looney")) "\n"
     (sx/cmt ";;  - " (sx/lit "Luis Gutierrez")) "\n"
     (sx/cmt ";;  - " (sx/lit "Chris Oakman")) "\n"
     (sx/cmt ";;  - " (sx/lit "Brett Darnell")) "\n"
     (sx/cmt ";;  - " (sx/lit "Phil Gambling")) "\n"
     "\n\n"
     (sx/cmt ";; Oh, and if you really want to look at the") "\n"
     (sx/cmt ";; code for this presentation...") "\n"
     (sx/cmt ";; ") (sx/out [:a {:href "https://github.com/shaunlebron/t3tr0s-slides"} "here it is."]) "\n"
     "\n\n"
     (sx/cmt ";; Thanks for reading!\n")
     (sx/cmt ";; ") (sx/core [:a {:href "http://twitter.com/shaunlebron"} "@shaunlebron"]) "\n"]]])

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

    (set! (.. ctx -fillStyle) dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board @app))))

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
  [:.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}}]])

(rum/defc slide []
  [:div
   [:h1 "The end."]
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
