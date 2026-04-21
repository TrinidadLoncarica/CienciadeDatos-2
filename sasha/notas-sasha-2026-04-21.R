library(ggplot2)
student_habits_performance %>% 
  ggplot(aes(exam_score))+
  geom_histogram()

student_habits_performance %>% 
  ggplot(aes(exam_score))+
  geom_histogram(fill="white", color="steelblue")

student_habits_performance %>% 
  ggplot(aes(exam_score))+
  geom_density(fill="white", color="steelblue")


student_habits_performance %>% 
  ggplot(aes(study_hours_per_day))+
  geom_density(fill="white", color="steelblue")

student_habits_performance$study_hours_per_day %>% 
  summary()
student_habits_performance$study_hours_per_day %>% 
  sd()


student_habits_performance$gender %>% 
  table()

student_habits_performance %>% 
  count(gender) %>% 
  ggplot(aes(x=gender,y=n ,fill=gender))+
  geom_col()

student_habits_performance %>% 
  count(parental_education_level) 

student_habits_performance %>% 
  count(parental_education_level) %>% 
  ggplot(aes(x=parental_education_level,y=n ,fill=parental_education_level))+
  geom_col()


