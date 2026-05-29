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

tabla_resumen_exam <- function(datos) {
  datos %>%
    summarise(
      Minimo     = min(exam_score, na.rm = TRUE),
      Q1         = quantile(exam_score, 0.25, na.rm = TRUE),
      Mediana    = median(exam_score, na.rm = TRUE),
      Media      = mean(exam_score, na.rm = TRUE),
      Q3         = quantile(exam_score, 0.75, na.rm = TRUE),
      Maximo     = max(exam_score, na.rm = TRUE),
      Desvio_std = sd(exam_score, na.rm = TRUE),
      Rango      = max(exam_score) - min(exam_score)
    ) %>%
    gt() %>%
    tab_header(
      title    = "Análisis exploratorio de los datos",
      subtitle = "Variable: Exam Score"
    ) %>%
    fmt_number(decimals = 2) %>%
    cols_label(
      Minimo     = "Mínimo",
      Q1         = "Q1 (25%)",
      Mediana    = "Mediana",
      Media      = "Media",
      Q3         = "Q3 (75%)",
      Maximo     = "Máximo",
      Desvio_std = "Desvío Estándar",
      Rango      = "Rango"
    ) %>%
    tab_options(
      table.font.size          = 14,
      heading.background.color = "steelblue",
      heading.title.font.size  = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_diet_quality <- function(datos) {
  datos %>%
    count(diet_quality) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
    gt() %>%
    tab_header(
      title    = "Distribución de Calidad de Dieta",
      subtitle = "Variable: Diet Quality"
    ) %>%
    cols_label(
      diet_quality = "Calidad de Dieta",
      n            = "Frecuencia",
      porcentaje   = "Porcentaje (%)"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_internet_quality <- function(datos) {
  datos %>%
    count(internet_quality) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
    gt() %>%
    tab_header(
      title    = "Distribución de Calidad de Internet",
      subtitle = "Variable: Internet Quality"
    ) %>%
    cols_label(
      internet_quality = "Calidad de Internet",
      n                = "Frecuencia",
      porcentaje       = "Porcentaje (%)"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_gender <- function(datos) {
  datos %>%
    count(gender) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
    gt() %>%
    tab_header(
      title    = "Distribución por Género",
      subtitle = "Variable: Gender"
    ) %>%
    cols_label(
      gender     = "Género",
      n          = "Frecuencia",
      porcentaje = "Porcentaje (%)"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_parental_education <- function(datos) {
  datos %>%
    count(parental_education_level) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
    gt() %>%
    tab_header(
      title    = "Distribución por Nivel Educativo de los Padres",
      subtitle = "Variable: Parental Education Level"
    ) %>%
    cols_label(
      parental_education_level = "Nivel Educativo",
      n                        = "Frecuencia",
      porcentaje               = "Porcentaje (%)"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_cor_study_exam <- function(datos) {
  cor.test(datos$study_hours_per_day, datos$exam_score) %>%
    tidy() %>%
    gt() %>%
    tab_header(
      title    = "Test de Correlación",
      subtitle = "Study Hours Per Day vs Exam Score"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      estimate    = "Correlación (r)",
      statistic   = "Estadístico (t)",
      p.value     = "P-valor",
      parameter   = "Grados de libertad",
      conf.low    = "IC inferior (95%)",
      conf.high   = "IC superior (95%)",
      method      = "Método",
      alternative = "Hipótesis"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_resumen_social_media <- function(datos) {
  datos %>%
    summarise(
      Minimo     = min(social_media_hours, na.rm = TRUE),
      Q1         = quantile(social_media_hours, 0.25, na.rm = TRUE),
      Mediana    = median(social_media_hours, na.rm = TRUE),
      Media      = mean(social_media_hours, na.rm = TRUE),
      Q3         = quantile(social_media_hours, 0.75, na.rm = TRUE),
      Maximo     = max(social_media_hours, na.rm = TRUE),
      Desvio_std = sd(social_media_hours, na.rm = TRUE),
      Rango      = max(social_media_hours) - min(social_media_hours)
    ) %>%
    gt() %>%
    tab_header(
      title    = "Estadísticas Descriptivas",
      subtitle = "Variable: Social Media Hours"
    ) %>%
    fmt_number(decimals = 2) %>%
    cols_label(
      Minimo     = "Mínimo",
      Q1         = "Q1 (25%)",
      Mediana    = "Mediana",
      Media      = "Media",
      Q3         = "Q3 (75%)",
      Maximo     = "Máximo",
      Desvio_std = "Desvío Estándar",
      Rango      = "Rango"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_coef_modelo_redes <- function(datos) {
  modelo <- lm(exam_score ~ social_media_hours, data = datos)
  tidy(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Coeficientes del Modelo de Regresión Simple",
      subtitle = "exam_score ~ social_media_hours"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      term      = "Variable",
      estimate  = "Estimado",
      std.error = "Error Estándar",
      statistic = "Estadístico (t)",
      p.value   = "P-valor"
    ) %>%
    tab_style(
      style     = cell_fill(color = "#d4f4dd"),
      locations = cells_body(rows = p.value < 0.05)
    ) %>%
    tab_style(
      style     = cell_fill(color = "#ffd4d4"),
      locations = cells_body(rows = p.value >= 0.05)
    ) %>%
    tab_footnote(
      footnote = "Verde: p < 0.05 (significativo) | Rojo: p ≥ 0.05 (no significativo)"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_metricas_modelo_redes <- function(datos) {
  modelo <- lm(exam_score ~ social_media_hours, data = datos)
  glance(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Métricas Globales del Modelo",
      subtitle = "exam_score ~ social_media_hours"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      r.squared     = "R²",
      adj.r.squared = "R² Ajustado",
      sigma         = "Sigma",
      statistic     = "F-statistic",
      p.value       = "P-valor",
      df            = "GL",
      logLik        = "Log-Likelihood",
      AIC           = "AIC",
      BIC           = "BIC",
      deviance      = "Devianza",
      df.residual   = "GL Residuos",
      nobs          = "N observaciones"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_residuos_modelo_redes <- function(datos) {
  modelo <- lm(exam_score ~ social_media_hours, data = datos)
  augment(modelo) %>%
    head(10) %>%
    gt() %>%
    tab_header(
      title    = "Valores Ajustados y Residuos",
      subtitle = "Primeras 10 observaciones"
    ) %>%
    fmt_number(decimals = 4) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_resumen_sleep_hours <- function(datos) {
  datos %>%
    summarise(
      Minimo     = min(sleep_hours, na.rm = TRUE),
      Q1         = quantile(sleep_hours, 0.25, na.rm = TRUE),
      Mediana    = median(sleep_hours, na.rm = TRUE),
      Media      = mean(sleep_hours, na.rm = TRUE),
      Q3         = quantile(sleep_hours, 0.75, na.rm = TRUE),
      Maximo     = max(sleep_hours, na.rm = TRUE),
      Desvio_std = sd(sleep_hours, na.rm = TRUE),
      Rango      = max(sleep_hours) - min(sleep_hours)
    ) %>%
    gt() %>%
    tab_header(
      title    = "Estadísticas Descriptivas",
      subtitle = "Variable: Sleep Hours"
    ) %>%
    fmt_number(decimals = 2) %>%
    cols_label(
      Minimo     = "Mínimo",
      Q1         = "Q1 (25%)",
      Mediana    = "Mediana",
      Media      = "Media",
      Q3         = "Q3 (75%)",
      Maximo     = "Máximo",
      Desvio_std = "Desvío Estándar",
      Rango      = "Rango"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_coef_modelo_sueno <- function(datos) {
  modelo <- lm(exam_score ~ sleep_hours, data = datos)
  tidy(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Coeficientes del Modelo",
      subtitle = "exam_score ~ sleep_hours"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      term      = "Término",
      estimate  = "Estimado",
      std.error = "Error Estándar",
      statistic = "Estadístico (t)",
      p.value   = "P-valor"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_metricas_modelo_sueno <- function(datos) {
  modelo <- lm(exam_score ~ sleep_hours, data = datos)
  glance(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Métricas Globales del Modelo",
      subtitle = "exam_score ~ sleep_hours"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      r.squared     = "R²",
      adj.r.squared = "R² Ajustado",
      sigma         = "Sigma",
      statistic     = "F-statistic",
      p.value       = "P-valor",
      df            = "GL",
      logLik        = "Log-Likelihood",
      AIC           = "AIC",
      BIC           = "BIC",
      deviance      = "Devianza",
      df.residual   = "GL Residuos",
      nobs          = "N observaciones"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_residuos_modelo_sueno <- function(datos) {
  modelo <- lm(exam_score ~ sleep_hours, data = datos)
  augment(modelo) %>%
    head(10) %>%
    gt() %>%
    tab_header(
      title    = "Valores Ajustados y Residuos",
      subtitle = "Primeras 10 observaciones"
    ) %>%
    fmt_number(decimals = 4) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_resumen_mental_health <- function(datos) {
  datos %>%
    summarise(
      Minimo     = min(mental_health_rating, na.rm = TRUE),
      Q1         = quantile(mental_health_rating, 0.25, na.rm = TRUE),
      Mediana    = median(mental_health_rating, na.rm = TRUE),
      Media      = mean(mental_health_rating, na.rm = TRUE),
      Q3         = quantile(mental_health_rating, 0.75, na.rm = TRUE),
      Maximo     = max(mental_health_rating, na.rm = TRUE),
      Desvio_std = sd(mental_health_rating, na.rm = TRUE),
      Rango      = max(mental_health_rating) - min(mental_health_rating)
    ) %>%
    gt() %>%
    tab_header(
      title    = "Estadísticas Descriptivas",
      subtitle = "Variable: Mental Health Rating"
    ) %>%
    fmt_number(decimals = 2) %>%
    cols_label(
      Minimo     = "Mínimo",
      Q1         = "Q1 (25%)",
      Mediana    = "Mediana",
      Media      = "Media",
      Q3         = "Q3 (75%)",
      Maximo     = "Máximo",
      Desvio_std = "Desvío Estándar",
      Rango      = "Rango"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_coef_modelo_mental <- function(datos) {
  modelo <- lm(exam_score ~ mental_health_rating, data = datos)
  tidy(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Coeficientes del Modelo",
      subtitle = "exam_score ~ mental_health_rating"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      term      = "Término",
      estimate  = "Estimado",
      std.error = "Error Estándar",
      statistic = "Estadístico (t)",
      p.value   = "P-valor"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_metricas_modelo_mental <- function(datos) {
  modelo <- lm(exam_score ~ mental_health_rating, data = datos)
  glance(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Métricas Globales del Modelo",
      subtitle = "exam_score ~ mental_health_rating"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      r.squared     = "R²",
      adj.r.squared = "R² Ajustado",
      sigma         = "Sigma",
      statistic     = "F-statistic",
      p.value       = "P-valor",
      df            = "GL",
      logLik        = "Log-Likelihood",
      AIC           = "AIC",
      BIC           = "BIC",
      deviance      = "Devianza",
      df.residual   = "GL Residuos",
      nobs          = "N observaciones"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_residuos_modelo_mental <- function(datos) {
  modelo <- lm(exam_score ~ mental_health_rating, data = datos)
  augment(modelo) %>%
    head(10) %>%
    gt() %>%
    tab_header(
      title    = "Valores Ajustados y Residuos",
      subtitle = "Primeras 10 observaciones"
    ) %>%
    fmt_number(decimals = 4) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_cor_spearman_mental <- function(datos) {
  cor.test(datos$mental_health_rating, datos$exam_score,
           method = "spearman") %>%
    tidy() %>%
    gt() %>%
    tab_header(
      title    = "Test de Correlación de Spearman",
      subtitle = "Mental Health Rating vs Exam Score"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      estimate    = "Correlación (ρ)",
      statistic   = "Estadístico (S)",
      p.value     = "P-valor",
      method      = "Método",
      alternative = "Hipótesis"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_coef_modelo_ejercicio <- function(datos) {
  modelo <- lm(exam_score ~ exercise_frequency + diet_quality, data = datos)
  tidy(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Modelo de Regresión Simple",
      subtitle = "exam_score ~ exercise_frequency + diet_quality"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      term      = "Variable",
      estimate  = "Estimado",
      std.error = "Error Estándar",
      statistic = "Estadístico (t)",
      p.value   = "P-valor"
    ) %>%
    tab_style(
      style     = cell_fill(color = "#d4f4dd"),
      locations = cells_body(rows = p.value < 0.05)
    ) %>%
    tab_style(
      style     = cell_fill(color = "#ffd4d4"),
      locations = cells_body(rows = p.value >= 0.05)
    ) %>%
    tab_footnote(
      footnote = "Verde: p < 0.05 (significativo) | Rojo: p ≥ 0.05 (no significativo)"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_metricas_modelo_ejercicio <- function(datos) {
  modelo <- lm(exam_score ~ exercise_frequency + diet_quality, data = datos)
  glance(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Métricas Globales del Modelo",
      subtitle = "exam_score ~ exercise_frequency + diet_quality"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      r.squared     = "R²",
      adj.r.squared = "R² Ajustado",
      sigma         = "Sigma",
      statistic     = "F-statistic",
      p.value       = "P-valor",
      df            = "GL",
      logLik        = "Log-Likelihood",
      AIC           = "AIC",
      BIC           = "BIC",
      deviance      = "Devianza",
      df.residual   = "GL Residuos",
      nobs          = "N observaciones"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_ttest_trabajo <- function(datos) {
  con_trabajo <- datos %>% filter(part_time_job == "Yes") %>% pull(exam_score)
  sin_trabajo <- datos %>% filter(part_time_job == "No")  %>% pull(exam_score)
  resultado   <- t.test(con_trabajo, sin_trabajo)
  tidy(resultado) %>%
    gt() %>%
    tab_header(
      title    = "Test T de Medias Independientes",
      subtitle = "Exam Score: Con trabajo vs Sin trabajo"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      estimate    = "Diferencia de medias",
      estimate1   = "Media con trabajo",
      estimate2   = "Media sin trabajo",
      statistic   = "Estadístico (t)",
      p.value     = "P-valor",
      parameter   = "Grados de libertad",
      conf.low    = "IC inferior (95%)",
      conf.high   = "IC superior (95%)",
      method      = "Método",
      alternative = "Hipótesis"
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}
tabla_modelo_interacciones <- function(datos) {
  modelo <- lm(
    exam_score ~
      study_hours_per_day * sleep_hours +
      study_hours_per_day * mental_health_rating +
      social_media_hours * sleep_hours +
      diet_quality + parental_education_level +
      netflix_hours,
    data = datos
  )
  tidy(modelo) %>%
    gt() %>%
    tab_header(
      title    = "Resumen del Modelo con Interacciones",
      subtitle = "Variable dependiente: Exam Score"
    ) %>%
    fmt_number(decimals = 4) %>%
    cols_label(
      term      = "Variable",
      estimate  = "Estimado",
      std.error = "Error Estándar",
      statistic = "Estadístico (t)",
      p.value   = "P-valor"
    ) %>%
    tab_style(
      style     = cell_fill(color = "#d4f4dd"),
      locations = cells_body(rows = p.value < 0.05)
    ) %>%
    tab_style(
      style     = cell_fill(color = "#ffd4d4"),
      locations = cells_body(rows = p.value >= 0.05)
    ) %>%
    tab_options(
      table.font.size           = 14,
      heading.background.color  = "steelblue",
      heading.title.font.size   = 16,
      column_labels.font.weight = "bold"
    )
}

tabla_coef_modelo_multiple <- function(df) {
  modelo <- lm(exam_score ~ study_hours_per_day + sleep_hours +
                 social_media_hours + mental_health_rating +
                 exercise_frequency + attendance_percentage,
               data = df)
  
  tidy(modelo) |>
    mutate(
      term = case_when(
        term == "(Intercept)"          ~ "Intercepto",
        term == "study_hours_per_day"  ~ "Horas de estudio",
        term == "sleep_hours"          ~ "Horas de sueño",
        term == "social_media_hours"   ~ "Redes sociales",
        term == "mental_health_rating" ~ "Salud mental",
        term == "exercise_frequency"   ~ "Ejercicio",
        term == "attendance_percentage"~ "Asistencia",
        TRUE ~ term
      ),
      p.value = round(p.value, 4),
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3)
    ) |>
    rename(
      Variable   = term,
      Coeficiente = estimate,
      `Error estándar` = std.error,
      `Estadístico t`  = statistic,
      `p-value`        = p.value
    ) |>
    gt() |>
    tab_header(title = "Coeficientes del modelo de regresión múltiple") |>
    tab_options(
      table.background.color         = "#ffffff",
      heading.background.color       = "#0a0a0a",
      heading.title.font.color       = "#ffffff",
      column_labels.background.color = "#0a0a0a",
      column_labels.font.color       = "#ffffff",
      column_labels.font.weight      = "bold",
      table.border.top.color         = "#0a0a0a",
      table.border.top.width         = px(2)
    )
}

tabla_metricas_modelo_multiple <- function(df) {
  modelo <- lm(exam_score ~ study_hours_per_day + sleep_hours +
                 social_media_hours + mental_health_rating +
                 exercise_frequency + attendance_percentage,
               data = df)
  
  glance(modelo) |>
    select(r.squared, adj.r.squared, sigma, statistic, p.value, df) |>
    mutate(
      r.squared     = round(r.squared, 4),
      adj.r.squared = round(adj.r.squared, 4),
      sigma         = round(sigma, 3),
      statistic     = round(statistic, 3),
      p.value       = round(p.value, 6)
    ) |>
    rename(
      `R²`            = r.squared,
      `R² ajustado`   = adj.r.squared,
      `Error estándar`= sigma,
      `Estadístico F` = statistic,
      `p-value`       = p.value,
      `Grados libertad` = df
    ) |>
    gt() |>
    tab_header(title = "Métricas del modelo de regresión múltiple") |>
    tab_options(
      table.background.color         = "#ffffff",
      heading.background.color       = "#0a0a0a",
      heading.title.font.color       = "#ffffff",
      column_labels.background.color = "#0a0a0a",
      column_labels.font.color       = "#ffffff",
      column_labels.font.weight      = "bold",
      table.border.top.color         = "#0a0a0a",
      table.border.top.width         = px(2)
    )
}

tabla_coef_modelo_multiple <- function(df) {
  modelo <- lm(exam_score ~ study_hours_per_day + sleep_hours +
                 social_media_hours + mental_health_rating +
                 exercise_frequency + attendance_percentage,
               data = df)
  
  tidy(modelo) |>
    mutate(
      term = case_when(
        term == "(Intercept)"           ~ "Intercepto",
        term == "study_hours_per_day"   ~ "Horas de estudio",
        term == "sleep_hours"           ~ "Horas de sueño",
        term == "social_media_hours"    ~ "Redes sociales",
        term == "mental_health_rating"  ~ "Salud mental",
        term == "exercise_frequency"    ~ "Ejercicio",
        term == "attendance_percentage" ~ "Asistencia",
        TRUE ~ term
      ),
      p.value   = round(p.value, 4),
      estimate  = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3)
    ) |>
    rename(
      Variable         = term,
      Coeficiente      = estimate,
      `Error estándar` = std.error,
      `Estadístico t`  = statistic,
      `p-value`        = p.value
    ) |>
    gt() |>
    tab_header(title = "Coeficientes del modelo de regresión múltiple") |>
    tab_options(
      table.background.color         = "#ffffff",
      column_labels.background.color = "#0a0a0a",
      column_labels.font.weight      = "bold",
      table.border.top.color         = "#0a0a0a",
      table.border.top.width         = px(2)
    ) |>
    tab_style(
      style = cell_text(color = "#ffffff"),
      locations = cells_column_labels()
    )
}

tabla_metricas_modelo_multiple <- function(df) {
  modelo <- lm(exam_score ~ study_hours_per_day + sleep_hours +
                 social_media_hours + mental_health_rating +
                 exercise_frequency + attendance_percentage,
               data = df)
  
  glance(modelo) |>
    select(r.squared, adj.r.squared, sigma, statistic, p.value, df) |>
    mutate(
      r.squared     = round(r.squared, 4),
      adj.r.squared = round(adj.r.squared, 4),
      sigma         = round(sigma, 3),
      statistic     = round(statistic, 3),
      p.value       = round(p.value, 6)
    ) |>
    rename(
      `R²`               = r.squared,
      `R² ajustado`      = adj.r.squared,
      `Error estándar`   = sigma,
      `Estadístico F`    = statistic,
      `p-value`          = p.value,
      `Grados libertad`  = df
    ) |>
    gt() |>
    tab_header(title = "Métricas del modelo de regresión múltiple") |>
    tab_options(
      table.background.color         = "#ffffff",
      column_labels.background.color = "#0a0a0a",
      column_labels.font.weight      = "bold",
      table.border.top.color         = "#0a0a0a",
      table.border.top.width         = px(2)
    ) |>
    tab_style(
      style = cell_text(color = "#ffffff"),
      locations = cells_column_labels()
    )
}
    