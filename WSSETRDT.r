library(readxl)
library(dplyr)
library(writexl)



df <- read_excel("/Users/kunal/Desktop/Work/Whole Survey Set Elementary Teacher Report data TEST.xlsx")
names(df)


#Converts all the schools according to the number assigned.
custom_mapping <- c(
  "Wilson Elementary School (Logan School District)" = 1,
  "Helen M Knight Elementary School(Grand County School District)" = 2,
  "Trailside Elementary School (Park City School District)" = 3,
  "North Summit Elementary School (North Summit School District)" = 4
)

df$name_of_school <- as.numeric(factor(df$name_of_school, levels = names(custom_mapping)))



#Converts the DOB into correct format and calculates age.
df$dob <- as.Date(df$dob, "%m/%d/%Y")
df$age <- floor(as.numeric(difftime(Sys.Date(), df$dob, units = "days") / 365.25))



#Converts all the Grades from 6th(0) to 12th(6).
grade_mapping <- c(
  "Kindergarten" = 0,
  "1st Grade" = 1,
  "2nd Grade" = 2,
  "3rd Grade" = 3,
  "4th Grade" = 4,
  "5th Grade" = 5
)

df$student_grade <- grade_mapping[df$student_grade]


#Converts the YES/NO columns to 1 and 0.
cols_to_convert <- c("race_ethnicity_0", "race_ethnicity_1", "race_ethnicity_2", "race_ethnicity_3", "race_ethnicity_4", "race_ethnicity_5", "race_ethnicity_6", "race_ethnicity_7", "race_ethnicity_8", "race_ethnicity_9","race_ethnicity_10")
df <- df %>%
  mutate_at(cols_to_convert, ~ifelse(. == "Yes", 1, 0))


#Converts all the gender identity values to numeric values from 0 to 9.
df$gender_identity <- recode(df$gender_identity,
                               "Female" = 0,
                               "Male" = 1,
                               "Non-binary/third gender" = 2,
                               "Transgender" = 3,
                               "Agender" = 4,
                               "Genderqueer" = 5,
                               "A gender not listed" = 7,
                               "Prefer to self-describe" = 8,
                               "Prefer not to say" = 9,
                               "Do not know" = 10)


#Converts all the Student Wellbeing Teacher Report Scale(swtrs) values to numeric values from 1 to 12.
columns_to_recode <- c("swtrs_1", "swtrs_2", "swtrs_3", "swtrs_4", "swtrs_5", "swtrs_6", "swtrs_7", "swtrs_8", "swtrs_9", "swtrs_10", "swtrs_11", "swtrs_12")

df[columns_to_recode] <- lapply(df[columns_to_recode], function(x) {
  recode(x,
         "Never" = 0,
         "Rarely" = 1,
         "Sometimes" = 2,
         "Often" = 3,
         "Almost Always" = 4)
})

#Sum of all the swtrs columns(1-12).
df$swtrs_wellbeing <- rowSums(df[, c("swtrs_1", "swtrs_2", "swtrs_3", "swtrs_4", "swtrs_5", "swtrs_6", "swtrs_7", "swtrs_8", "swtrs_9", "swtrs_10", "swtrs_11", "swtrs_12")], na.rm = TRUE)

#Sum of all swtrs columns(1,6,9,10).
df$swtrs_academic <- rowSums(df[, c("swtrs_1","swtrs_6", "swtrs_9", "swtrs_10")])

#Sum of all swtrs columns(3,4,7,12).
df$swtrs_social <- rowSums(df[, c("swtrs_3","swtrs_4", "swtrs_7", "swtrs_12")])

#Sum of all swtrs columns(2,5,8,11).
df$swtrs_emotional <- rowSums(df[, c("swtrs_2","swtrs_5", "swtrs_8", "swtrs_11")])



#Converts all the Student Risk Screening Scale Internalizing and Externalizing(ssrs) values to numeric values from 1 to 12.
columns_to_recode <- c("ssrs_1", "ssrs_2", "ssrs_3", "ssrs_4", "ssrs_5", "ssrs_6", "ssrs_7", "ssrs_8", "ssrs_9", "ssrs_10", "ssrs_11", "ssrs_12")

df[columns_to_recode] <- lapply(df[columns_to_recode], function(x) {
  recode(x,
         "Never" = 0,
         "Occasionally" = 1,
         "Sometimes" = 2,
         "Frequently" = 3)
})

#Sum of all ssrs columns(1-7).
df$ssrs_externalizing <- rowSums(df[, c("ssrs_1","ssrs_2", "ssrs_3", "ssrs_4","ssrs_5","ssrs_6","ssrs_7")])

#Sum of all ssrs columns(8-12).
df$ssrs_internalizing <- rowSums(df[, c("ssrs_8","ssrs_9", "ssrs_10", "ssrs_11","ssrs_12")])

#Exports the modified file into the given path.
write_xlsx(df, path = "/Users/kunal/Desktop/Work/Output_Whole_Survey_Set_Elementary_Teacher_Report_Data_Test.xlsx")