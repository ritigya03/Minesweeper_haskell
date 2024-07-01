import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (Color)

main :: IO ()
main = do
  initGUI
  window <- windowNew

  
  set window [windowTitle := "Hello, It's Ritigya!", containerBorderWidth := 50, windowDefaultWidth := 400, windowDefaultHeight := 400]

  
  let bgColor = Color 304320 35 65535  

  
  widgetModifyBg window StateNormal bgColor

  widgetShowAll window
  on window objectDestroy mainQuit
  mainGUI

