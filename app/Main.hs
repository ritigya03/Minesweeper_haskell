import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (Color)

main :: IO ()
main = do
  initGUI
  window <- windowNew

  -- Set the window properties
  set window [windowTitle := "Hello, It's Kriti!", containerBorderWidth := 50, windowDefaultWidth := 400, windowDefaultHeight := 400]

  -- Create a GdkColor for the desired background color
  let bgColor = Color 65535 0 65535  -- RGB values (0-65535 scale), here it's a light blue color

  -- Modify the background color of the window
  widgetModifyBg window StateNormal bgColor

  widgetShowAll window
  on window objectDestroy mainQuit
  mainGUI
