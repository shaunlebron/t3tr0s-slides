(ns t3tr0s-slides.slide10
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! take! close! <! >! alts! chan timeout]]
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
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 1 0 0 0 0 0 0 0]
   [ 0 0 1 0 0 0 0 0 0 0]
   [ 0 1 1 1 0 0 0 0 1 0]
   [ 1 1 1 1 0 0 0 0 1 0]
   [ 1 1 1 1 1 0 0 0 1 0]
   [ 1 1 1 1 1 1 1 1 1 0]])

(def initial-pos [4 2])

(def app (atom {:board filled-board
                :piece (:T pieces)
                :position initial-pos
                :active-block nil
                :active-id {"spawn" 0
                            "soft" 0}}))

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

(defn try-shift! [dx]
  (let [{:keys [piece board position]} @app
        [x y] position
        new-pos [(+ x dx) y]]
    (when (piece-fits? board piece new-pos)
      (swap! app assoc :position new-pos))))

(defn try-rotate! []
  (let [{:keys [piece board position]} @app
        new-piece (rotate-piece piece)]
    (when (piece-fits? board new-piece position)
      (swap! app assoc :piece new-piece))))

(declare flash-active-block!)

(defn spawn-piece! []
  (flash-active-block! "spawn")
  (swap! app assoc :position initial-pos
                   :piece (rand-nth (vals pieces))))

(defn piece-done! []
  (lock-piece!)
  (spawn-piece!))

(defn soft-drop! []
  (let [{:keys [piece board position]} @app
        [x y] position
        new-pos [x (inc y)]]
    (if (piece-fits? board piece new-pos)
      (swap! app assoc :position new-pos)
      (piece-done!))))

(rum/defc code []
  (let [spawn-class (if (= (:active-block @app) "spawn") "active-row-534ed" "")
        soft-bump-class (if (= (:active-block @app) "spawn") "bump-row" "")
        soft-class (if (= (:active-block @app) "soft") "active-row-534ed" "")]
    [:.code-cb62a
     [:pre
      [:code
       (sx/cmt "; TRY IT: press down to drop.") "\n"
       "\n"
       [:div {:class spawn-class}
         "(" (sx/core "defn") " spawn-piece! []\n"
         "  (" (sx/core "swap!") " game-state " (sx/core "assoc") "\n"
         "    " (sx/kw ":position") " initial-pos\n"
         "    " (sx/kw ":piece") " (" (sx/core "rand-nth") " (" (sx/core "vals") " pieces))))\n"]
       "\n"
       [:div {:class spawn-class}
         "(" (sx/core "defn") " piece-done! []\n"
         "  (lock-piece!)\n"
         "  (spawn-piece!))\n"]
       "\n"
       [:div {:class soft-class}
         "(" (sx/core "defn") " soft-drop! []\n"
         "  (" (sx/core "let") " [{" (sx/kw ":keys") " [piece board position]} @game-state\n"
         "        [x y] position\n"
         "        new-pos [x (" (sx/core "inc") " y)]\n"
         "    (" (sx/core "if") [:span {:class soft-bump-class} " (piece-fits? board piece new-pos)\n"]
         "      (" (sx/core "swap!") " game-state " (sx/core "assoc") " " (sx/kw ":position") " new-pos)\n"
         "      (piece-done!))))\n"]]]]))

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
    (draw-board! ctx (:board @app))

    (let [piece (:piece @app)
          pos (:position @app)
          fits (app-piece-fits?)]

      (when (and piece pos)
        (set! (.. ctx -fillStyle)   (if fits dark-purple dark-red))
        (set! (.. ctx -strokeStyle) (if fits light-purple light-red))
        (draw-piece! ctx piece pos)))))


(def key-names
  {37 :left
   38 :up
   39 :right
   40 :down
   32 :space})

(def key-name #(-> % .-keyCode key-names))

(defn flash-active-block! [block]
  (let [active (:active-block @app)]
    (when-not (and (= block "soft") (= active "spawn"))
      (go
        (swap! app assoc :active-block block)
        (swap! app update-in [:active-id block] inc)
        (let [current-block #(get-in @app [:active-id block])
              i (current-block)]
          (<! (timeout (if (= block "spawn") 750 250)))
          (when (and (= i (current-block))
                     (= (:active-block @app) block))
            (swap! app assoc :active-block nil)))))))

(defn key-down [e]
  (let [kname (key-name e)]
   (case kname
     :left  (try-shift! -1)
     :right (try-shift! 1)
     :up    (try-rotate!)
     :down  (do (flash-active-block! "soft") (soft-drop!))
     nil)
   (when (#{:down :left :right :space :up} kname)
     (.preventDefault e))))

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
   [:h1 "10. Add soft drop and spawn."]
   (code)
   (canvas)])

(def slide-elm)
(defn render []
  (rum/mount (slide) slide-elm))

(defn init [id]
  (set! slide-elm (js/document.getElementById id))
  (render)
  (add-watch app :render render))

(defn resume []
  (.addEventListener js/window "keydown" key-down))

(defn stop []
  (.removeEventListener js/window "keydown" key-down))
