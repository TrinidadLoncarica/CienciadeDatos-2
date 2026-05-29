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

plot_densidad_exam <- function(datos) {
  datos %>%
    ggplot(aes(exam_score)) +
    geom_density(fill = "white", color = "steelblue") +
    labs(
      title = "Densidad del puntaje del examen",
      x = "Exam score",
      y = "Densidad"
    ) +
    theme_minimal()
}

plot_densidad_study_hours <- function(datos) {
  datos %>%
    ggplot(aes(study_hours_per_day)) +
    geom_density(fill = "white", color = "steelblue") +
    labs(
      title = "Distribución de las horas de estudio por día",
      x = "Horas de estudio",
      y = "Densidad"
    ) +
    theme_minimal()
}

plot_diet_quality <- function(datos) {
  datos %>%
    count(diet_quality) %>%
    ggplot(aes(x = diet_quality, y = n, fill = diet_quality)) +
    geom_col(alpha = 0.85, width = 0.65) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    ) +
    labs(
      title = "Distribución de la calidad de dieta",
      x = "Calidad de dieta",
      y = "Frecuencia"
    ) +
    theme_minimal()
}

plot_internet_quality <- function(datos) {
  datos %>%
    count(internet_quality) %>%
    ggplot(aes(x = internet_quality, y = n, fill = internet_quality)) +
    geom_col() +
    labs(
      title = "Distribución de la calidad de internet",
      x = "Calidad de internet",
      y = "Frecuencia"
    ) +
    theme_minimal()
}

plot_gender <- function(datos) {
  datos %>%
    count(gender) %>%
    ggplot(aes(x = gender, y = n, fill = gender)) +
    geom_col(alpha = 0.85, width = 0.65) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    ) +
    labs(
      title = "Distribución por género",
      x = "Género",
      y = "Frecuencia"
    ) +
    theme_minimal()
}

plot_parental_education <- function(datos) {
  datos %>%
    count(parental_education_level) %>%
    ggplot(aes(x = parental_education_level, y = n, fill = parental_education_level)) +
    geom_col(alpha = 0.85, width = 0.65) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    ) +
    labs(
      title = "Distribución del nivel educativo parental",
      x = "Nivel educativo parental",
      y = "Frecuencia"
    ) +
    theme_minimal()
}

plot_densidad_social_media <- function(datos) {
  datos %>%
    ggplot(aes(social_media_hours)) +
    geom_density(fill = "white", color = "steelblue") +
    labs(
      title = "Distribución de horas en redes sociales",
      x = "Horas en redes sociales",
      y = "Densidad"
    ) +
    theme_minimal()
}

plot_social_media_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = social_media_hours, y = exam_score)) +
    geom_point(alpha = 0.45, size = 2, color = "steelblue") +
    geom_smooth(method = "lm", color = "firebrick", se = TRUE, linewidth = 1) +
    labs(
      title = "Uso de Redes Sociales vs Puntaje del Examen",
      x = "Horas en redes sociales por día",
      y = "Puntaje del examen"
    ) +
    theme_minimal()
}
plot_densidad_sleep_hours <- function(datos) {
  datos %>%
    ggplot(aes(sleep_hours)) +
    geom_density(fill = "white", color = "steelblue") +
    labs(
      title = "Distribución de horas de sueño",
      x = "Horas de sueño",
      y = "Densidad"
    ) +
    theme_minimal()
}

plot_sleep_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = sleep_hours, y = exam_score)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = "Horas de Sueño vs Puntaje del Examen",
      x = "Horas de sueño por día",
      y = "Puntaje del examen"
    ) +
    theme_minimal()
}

plot_mental_health_rating <- function(datos) {
  datos %>%
    ggplot(aes(mental_health_rating)) +
    geom_histogram(fill = "white", color = "steelblue", bins = 10) +
    labs(
      title = "Distribución del rating de salud mental",
      x = "Salud mental",
      y = "Frecuencia"
    ) +
    theme_minimal()
}

plot_mental_health_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = factor(mental_health_rating), y = exam_score, fill = factor(mental_health_rating))) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_brewer(palette = "RdYlGn") +
    labs(
      title = "Rating de Salud Mental vs Puntaje del Examen",
      x = "Rating de salud mental (1 = malo, 10 = excelente)",
      y = "Puntaje del examen",
      fill = "Rating"
    ) +
    theme_minimal()
}

plot_diet_quality_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = diet_quality, y = exam_score, fill = diet_quality)) +
    geom_boxplot() +
    labs(
      title = "Puntaje según calidad de dieta",
      x = "Calidad de dieta",
      y = "Puntaje del examen"
    ) +
    theme_minimal()
}
plot_ejercicio_dieta_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = exercise_frequency, y = exam_score, color = diet_quality)) +
    geom_jitter(alpha = 0.5, width = 0.2) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Frecuencia de Ejercicio y Calidad de Dieta vs Puntaje",
      x = "Días de ejercicio por semana",
      y = "Puntaje del examen",
      color = "Calidad de dieta"
    ) +
    theme_minimal()
}
plot_trabajo_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = part_time_job, y = exam_score, fill = part_time_job)) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 23,
                 size = 4, fill = "white") +
    labs(
      title = "¿Trabajar afecta el rendimiento académico?",
      x = "Trabajo a tiempo parcial",
      y = "Puntaje del examen"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}
plot_internet_quality_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = internet_quality, y = exam_score, fill = internet_quality)) +
    geom_boxplot(alpha = 0.75, width = 0.6) +
    labs(
      title = "Puntaje según calidad de internet",
      x = "Calidad de internet",
      y = "Puntaje del examen"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
}
plot_parental_education_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = parental_education_level, y = exam_score, fill = parental_education_level)) +
    geom_boxplot() +
    theme_minimal()
}
plot_parental_education_vs_exam <- function(datos) {
  datos %>%
    ggplot(aes(x = parental_education_level, y = exam_score, fill = parental_education_level)) +
    geom_boxplot() +
    theme_minimal()
}

tabla_ranking_correlaciones <- function(datos) {
  datos %>%
    select(
      study_hours_per_day,
      social_media_hours,
      sleep_hours,
      attendance_percentage,
      mental_health_rating,
      exercise_frequency,
      netflix_hours,
      exam_score
    ) %>%
    cor(use = "complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    select(variable, correlacion_con_exam_score = exam_score) %>%
    filter(variable != "exam_score") %>%
    mutate(correlacion_abs = abs(correlacion_con_exam_score)) %>%
    arrange(desc(correlacion_abs)) %>%
    select(variable, correlacion_con_exam_score) %>%
    gt() %>%
    tab_header(
      title    = "Ranking de Correlaciones con Rendimiento Académico",
      subtitle = "Variable dependiente: Exam Score"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      variable                   = "Variable",
      correlacion_con_exam_score = "Correlación con Exam Score"
    ) %>%
    tab_style(
      style     = cell_fill(color = "#d4edff"),
      locations = cells_body(rows = 1)
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
plot_correlaciones_exam <- function(datos) {
  datos %>%
    select(
      study_hours_per_day,
      social_media_hours,
      sleep_hours,
      attendance_percentage,
      mental_health_rating,
      exercise_frequency,
      netflix_hours,
      exam_score
    ) %>%
    cor(use = "complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    select(variable, correlacion_con_exam_score = exam_score) %>%
    filter(variable != "exam_score") %>%
    mutate(
      correlacion_abs = abs(correlacion_con_exam_score),
      variable = reorder(variable, correlacion_abs)
    ) %>%
    ggplot(aes(x = variable, y = correlacion_con_exam_score, fill = correlacion_con_exam_score)) +
    geom_col(width = 0.7) +
    coord_flip() +
    labs(
      title    = "Relación de cada hábito con el puntaje del examen",
      subtitle = "Correlación entre variables cuantitativas y exam_score",
      x        = "Variable",
      y        = "Correlación con exam_score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title      = element_text(face = "bold")
    )
}

plot_perfil_rendimiento <- function(df) {
  
  tema_interno <- theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "#ffffff", color = NA),
      panel.background  = element_rect(fill = "#ffffff", color = NA),
      panel.grid.major  = element_line(color = "#eeeeee", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(color = "#111111", size = 11),
      axis.title        = element_text(color = "#111111", size = 12, face = "bold"),
      plot.title        = element_text(color = "#111111", size = 15, face = "bold"),
      plot.subtitle     = element_text(color = "#888888", size = 11),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.text       = element_text(color = "#111111", size = 10),
      legend.title      = element_text(color = "#111111", size = 11, face = "bold")
    )
  
  df |>
    mutate(grupo = ifelse(df$exam_score >= quantile(df$exam_score, 0.75),
                          "Alto rendimiento", "Resto")) |>
    group_by(grupo) |>
    summarise(
      `Horas de estudio`   = mean(study_hours_per_day, na.rm = TRUE),
      `Horas de sueño`     = mean(sleep_hours, na.rm = TRUE),
      `Redes sociales (h)` = mean(social_media_hours, na.rm = TRUE),
      `Salud mental`       = mean(mental_health_rating, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(-grupo, names_to = "Variable", values_to = "Valor") |>
    ggplot(aes(x = Variable, y = Valor, fill = grupo)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
    scale_fill_manual(values = c("#111111", "#cccccc")) +
    coord_flip() +
    labs(
      title    = "Comparación: alto rendimiento vs. resto",
      subtitle = "Promedios por grupo",
      x        = NULL,
      y        = "Valor promedio",
      fill     = NULL
    ) +
    tema_interno +
    theme(legend.position = "bottom")
}