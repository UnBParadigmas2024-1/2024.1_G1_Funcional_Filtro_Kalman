module PlotChart (renderGraf, saveGraf) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- Função para criar o gráfico
renderGraf :: [Double] -> [Double] -> Renderable ()
renderGraf z1 z2 = toRenderable layout
  where
    val = plot_lines_style . line_color .~ opaque blue
          $ plot_lines_values .~ [ zip z1 z2 ]
          $ plot_lines_title .~ "valores"
          $ def

    layout = layout_title .~ "Resultados"
      $ layout_grid_last .~ False
      $ layout_plots .~ [toPlot val]
      $ def

saveGraf :: Renderable a -> Maybe String -> Maybe String -> IO () -- recebe o grafico renderizado e o nome do arquivo + formato
saveGraf graf nm fmt = do
  namedGraf (maybe "grafico" id nm) (maybe "svg" id fmt) graf
  where
    namedGraf :: String -> String -> Renderable a -> IO ()
    namedGraf sNm sFmt gr = do
      _ <- renderableToFile def ("img/" ++ sNm ++ "." ++ sFmt) gr
      return ()
