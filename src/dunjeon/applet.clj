(ns dunjeon.applet
  (:use [dunjeon.core :only [create-ui]])
  (:import [javax.swing JApplet SwingUtilities JPanel]
           [java.awt BorderLayout Container]
           [java.awt.event KeyListener])
  (:gen-class :extends javax.swing.JApplet
              :name dunjeon.applet
              :prefix "-applet-"))

(defn -applet-init [^JApplet this]
  (SwingUtilities/invokeLater
   #(let [{:keys [^JPanel panel, ^KeyListener ka]} (create-ui), ^Container pane (.getContentPane this)]
      (doto pane (.setLayout (BorderLayout.)) (.add panel BorderLayout/CENTER))
      (doto this (.addKeyListener ka)))))



