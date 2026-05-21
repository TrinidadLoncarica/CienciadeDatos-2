traer_datos <- function(){
  my_filename <- here::here("data/student_habits_performance.csv")
  
  read_csv(my_filename) %>%
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
