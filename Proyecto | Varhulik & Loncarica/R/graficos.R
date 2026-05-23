plot_exam_score <- function(datos) {
  datos %>% 
    ggplot(aes(exam_score)) +
    geom_histogram(fill = "white", color = "steelblue", bins = 30) +
    labs(
      title = "Distribución del puntaje del examen",
      x = "Exam score",
      y = "Frecuencia"
    ) +
    theme_minimal()
}

plot_estudio_score <- function(datos) {
  datos %>%
    ggplot(aes(x = study_hours_per_day, y = exam_score)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = "Horas de Estudio vs Puntaje del Examen",
      x = "Horas de estudio por día",
      y = "Puntaje del examen"
    ) +
    theme_minimal()
}