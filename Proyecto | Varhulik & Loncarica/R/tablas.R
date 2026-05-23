library(dplyr)
library(gt)

tabla_descriptiva <- function(datos, variable, subtitulo) {
  
  datos %>%
    summarise(
      Minimo = min({{variable}}, na.rm = TRUE),
      Media = mean({{variable}}, na.rm = TRUE),
      Mediana = median({{variable}}, na.rm = TRUE),
      Maximo = max({{variable}}, na.rm = TRUE),
      Desvio_std = sd({{variable}}, na.rm = TRUE)
    ) %>%
    gt() %>%
    tab_header(
      title = "Estadísticas descriptivas",
      subtitle = subtitulo
    )
  
}