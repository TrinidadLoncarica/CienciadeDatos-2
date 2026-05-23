library(readr)
library(dplyr)
traer_datos <- function() {
  read_csv("student_habits_performance.csv") %>%
    mutate(
      diet_quality = factor(
        diet_quality,
        levels = c("Poor", "Fair", "Good"),
        ordered = TRUE
      ),
      parental_education_level = factor(
        parental_education_level,
        levels = c("None", "High School", "Bachelor", "Master"),
        ordered = TRUE
      ),
      internet_quality = factor(
        internet_quality,
        levels = c("Poor", "Average", "Good"),
        ordered = TRUE
      )
    )
}
