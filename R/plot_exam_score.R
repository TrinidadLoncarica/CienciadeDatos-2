plot_exam_score <- function(StudentsHabits) {
  StudentsHabits %>% 
    ggplot(aes(exam_score)) +
    geom_histogram(fill = "white", color = "steelblue", bins = 30) +
    labs(
      title = "Distribución del puntaje del examen",
      x = "Exam score",
      y = "Frecuencia"
    ) +
    theme_minimal()
}