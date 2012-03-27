(ns dunjeon.frame
  (:use [dunjeon.core :only [create-ui]])
  (:import [java.awt BorderLayout]
           [javax.swing JFrame JPanel SwingUtilities]
           [java.awt.event KeyListener])
  (:gen-class))

(defn init-frame []
  (let [{:keys [^JPanel panel, ^KeyListener ka]} (create-ui)]
    (doto (JFrame. "(dunjeon)")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add panel) .pack
      (.setResizable false)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.addKeyListener ka))))

(defn -main [] (SwingUtilities/invokeLater init-frame))

