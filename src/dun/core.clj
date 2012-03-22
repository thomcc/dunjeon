(ns dun.core
  (:use [dun level utils])
  (:import [java.awt Color Graphics Dimension Font]
           [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent KeyAdapter]))

(def panel-width 600)
(def panel-height 600)

(def char-rep {:floor ".", nil "#", :wall "#", :stairs ">", :gold "$", :booze "q",
               :sword "(", :armor "[", :shield "+", :spawner "!"})

(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})

(defn draw-level [{width :width, height :height lvl :points}]
  (doseq [y (range height), x (range width)]
    (when (= 0 x) (.println System/out))
    (.print System/out (char-rep (lvl [x y])))))

(defn initialize-gamestate [level]
  {:level level, :player {:pos (rand-elt (:points level)), :health 50}})

(defn move [pos dir] (map + pos (dir directions)))

(defn tick-player [{{:keys [pos health :as player]} :player, level :level :as game-state} input]
  (let [newpos (move pos input)]
    (cond ((:points level) newpos) (assoc-in game-state [:player :pos] newpos)
          :else game-state)))

(defn tick [input game-state]
  (-> game-state tick-player))

(defn comprehend [input]
  ({KeyEvent/VK_UP :north, KeyEvent/VK_DOWN :south
    KeyEvent/VK_RIGHT :east, KeyEvent/VK_LEFT :west}
   (.getKeyCode input)))

(defn draw [^Graphics2D g
            {{:keys [[x y] health] :as player} :player,
             {:keys [points width height] :as level} :level
             :as game-state}]
  (let [frc (.getFontRenderContext g)]
    (doto g
      (.setColor Color/black)
      (.fillRect 0 0 panel-width panel-height)
      (.setFont (Font. "Monospaced" Font/PLAIN 14)))
    (doseq [xx (range width), yy (range height)]
      (when-let []))))

(defn -main [& args]
  (let [game-state (atom (initialize-gamestate (gen-level 50 50 (random 4 5))))
        dim (Dimension. panel-width panel-height)
        panel (doto (proxy [JPanel] [] (paint [g] (draw g @game-state)))
                (.setMinimumSize dim)
                (.setPreferredSize dim)
                (.setMaximumSize dim))
        ka (proxy [KeyAdapter] []
             (keyPressed [e]
               (when-let [input (comprehend e)]
                 (swap! game-state tick input)
                 (.repaint panel))))]
    (doto (JFrame. "(the dunjeon)")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add panel) .pack
      (.setResizable false)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.addKeyListener ka))))


