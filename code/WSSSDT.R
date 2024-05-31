library(readxl)
library(dplyr)
library(writexl)



df <- read_excel("/Users/kunal/Desktop/Work/Whole Survey Set Secondary data TEST.xlsx")
names(df)

#Converts the YES/NO columns to 1 and 0.
cols_to_convert <- c("consent_sec", "race_ethnicity_v2_0", "race_ethnicity_v2_1", "race_ethnicity_v2_2", "race_ethnicity_v2_3", "race_ethnicity_v2_4", "race_ethnicity_v2_5", "race_ethnicity_v2_6", "race_ethnicity_v2_7", "race_ethnicity_v2_8", "race_ethnicity_v2_9")
df <- df %>%
  mutate_at(cols_to_convert, ~ifelse(. == "Yes", 1, 0))



#Converts all the schools according to the number assigned.
custom_mapping <- c(
  "SLCSE Bryant Middle School (Salt Lake City School District)" = 1,
  "SLCSE Rose Park High School (Salt Lake City School District)" = 2,
  "San Juan High School (San Juan School District)" = 3,
  "Margret L. Hopkin Middle School (Grand County School District)" = 4,
  "Grand County High School (Grand County School District)" = 5
)

df$name_of_school_v2 <- as.numeric(factor(df$name_of_school_v2, levels = names(custom_mapping)))



#Converts all the Grades from 6th(0) to 12th(6).
grade_mapping <- c(
  "6th Grade" = 0,
  "7th Grade" = 1,
  "8th Grade" = 2,
  "9th Grade" = 3,
  "10th Grade" = 4,
  "11th Grade" = 5,
  "12th Grade" = 6
)

df$student_grade_v2 <- grade_mapping[df$student_grade_v2]


#Converts all the gender identity values to numeric values from 0 to 9.
df$gender_identity_v2 <- recode(df$gender_identity_v2,
                               "Female" = 0,
                               "Male" = 1,
                               "Non-binary/third gender" = 2,
                               "Transgender" = 3,
                               "Agender" = 4,
                               "Genderqueer" = 5,
                               "A gender not listed" = 7,
                               "Prefer to self-describe" = 8,
                               "Prefer not to say" = 9)

#Converts all the sexual orientation values to numeric values from 0 to 9.
df$sexual_orientation <- recode(df$sexual_orientation,
                                "Heterosexual or straight" = 0,
                                "Gay or Lesbian" = 1,
                                "Bisexual" = 2,
                                "Fluid" = 3,
                                "Pansexual" = 4,
                                "Queer" = 5,
                                "Demisexual" = 6,
                                "Questioning" = 7,
                                "Asexual" = 8,
                                "Prefer to self-describe" = 9,
                                "Prefer not to say" = 10)


#Open responses
df$sex_or_other <- ifelse(df$sexual_orientation == 9, "Open response item", NA)
df$eth_other_v2 <- ifelse(df$race_ethnicity_v2_8 == 1, "Open response item", NA)
df$gender_other_v2 <- ifelse(df$gender_identity_v2 == 8, "Open response item", NA)


#Converts the DOB into correct format and calculates age.
df$dob_v2 <- as.Date(df$dob_v2, "%m/%d/%Y")
df$age_v2 <- floor(as.numeric(difftime(Sys.Date(), df$dob_v2, units = "days") / 365.25))



#Converts all the Student Subjective Wellbeing Questionnaire(SSWQ) values to numeric values from 1 to 16.
columns_to_recode <- c("sswq_1", "sswq_2", "sswq_3", "sswq_4", "sswq_5", "sswq_6", "sswq_7", "sswq_8", "sswq_9", "sswq_10", "sswq_11", "sswq_12", "sswq_13", "sswq_14", "sswq_15", "sswq_16")

df[columns_to_recode] <- lapply(df[columns_to_recode], function(x) {
  recode(x,
         "Almost Never" = 1,
         "Sometimes" = 2,
         "Often" = 3,
         "Almost Always" = 4)
})

#Mean calculation of all the sswq columns(1-16).
df$sswq_wellbeing <- rowMeans(df[, c("sswq_1", "sswq_2", "sswq_3", "sswq_4", "sswq_5", "sswq_6", "sswq_7", "sswq_8", "sswq_9", "sswq_10", "sswq_11", "sswq_12", "sswq_13", "sswq_14", "sswq_15", "sswq_16")], na.rm = TRUE)

#Mean calculation of all the sswq columns(1, 5, 9, 13).
df$sswq_joy <- rowMeans(df[, c("sswq_1", "sswq_5", "sswq_9", "sswq_13")], na.rm = TRUE)

#Mean calculation of all the sswq columns(2, 6, 10, 14).
df$sswq_connect <- rowMeans(df[, c("sswq_2", "sswq_6", "sswq_10", "sswq_14")], na.rm = TRUE)

#Mean calculation of all the sswq columns(3, 7, 11, 15).
df$sswq_purpose <- rowMeans(df[, c("sswq_3", "sswq_7", "sswq_11", "sswq_15")], na.rm = TRUE)

#Mean calculation of all the sswq columns(4, 8, 12, 16).
df$sswq_efficacy <- rowMeans(df[, c("sswq_4", "sswq_8", "sswq_12", "sswq_16")], na.rm = TRUE)


#Converts all the Youth Internalizing Externalizing Problems Scale(YIEPS) values to numeric values from 1 to 20.
columns_to_recode <- c("yieps_1", "yieps_2", "yieps_3", "yieps_4", "yieps_5", "yieps_6", "yieps_7", "yieps_8", "yieps_9", "yieps_10", "yieps_11", "yieps_12", "yieps_13", "yieps_14", "yieps_15", "yieps_16","yieps_17","yieps_18","yieps_19","yieps_20")

df[columns_to_recode] <- lapply(df[columns_to_recode], function(x) {
  recode(x,
         "Almost Never" = 1,
         "Sometimes" = 2,
         "Often" = 3,
         "Almost Always" = 4)
})

#Mean calculation of all the yieps columns(1-10).
df$yieps_intern <- rowMeans(df[, c("yieps_1", "yieps_2", "yieps_3", "yieps_4","yieps_5","yieps_6","yieps_7","yieps_8","yieps_9","yieps_10")], na.rm = TRUE)

#Mean calculation of all the yieps columns(11-20).
df$yieps_extern <- rowMeans(df[, c("yieps_11", "yieps_12", "yieps_13", "yieps_14","yieps_15","yieps_16","yieps_17","yieps_18","yieps_19","yieps_20")], na.rm = TRUE)


# Function to categorize scores into risk levels
categorize_sswq_risk <- function(score, at_risk_cutoff, high_risk_cutoff) {
  if (score > at_risk_cutoff) {
    return("No Risk")
  } else if (score >= high_risk_cutoff) {
    return("At Risk")
  } else {
    return("High Risk")
  }
}

# Apply the categorization function to each column
df$risk_sswq_wellbeing <- mapply(categorize_sswq_risk, df$sswq_wellbeing, at_risk_cutoff = 2.2, high_risk_cutoff = 1.6)
df$risk_sswq_joy <- mapply(categorize_sswq_risk, df$sswq_joy, at_risk_cutoff = 1.7, high_risk_cutoff = 1.0)
df$risk_sswq_connect <- mapply(categorize_sswq_risk, df$sswq_connect, at_risk_cutoff = 2.0, high_risk_cutoff = 1.2)
df$risk_sswq_purpose <- mapply(categorize_sswq_risk, df$sswq_purpose, at_risk_cutoff = 2.1, high_risk_cutoff = 1.3)
df$risk_sswq_efficacy <- mapply(categorize_sswq_risk, df$sswq_efficacy, at_risk_cutoff = 2.2, high_risk_cutoff = 1.4)


# Function to categorize scores into risk levels
categorize_yieps_risk <- function(score, at_risk_cutoff, high_risk_cutoff) {
  if (score < at_risk_cutoff) {
    return("No Risk")
  } else if (score < high_risk_cutoff) {
    return("At Risk")
  } else {
    return("High Risk")
  }
}

# Apply the categorization function to each column
df$risk_yieps_extern <- mapply(categorize_yieps_risk, df$yieps_extern, at_risk_cutoff = 2.0, high_risk_cutoff = 2.5)
df$risk_yieps_intern <- mapply(categorize_yieps_risk, df$yieps_intern, at_risk_cutoff = 2.6, high_risk_cutoff = 3.3)

# Print the modified data frame
print(df)

calculate_risk_percentages <- function(data, columns) {
  result <- data.frame(matrix(0, nrow = 3, ncol = length(columns)))
  colnames(result) <- columns
  rownames(result) <- c("No Risk", "At Risk", "High Risk")
  
  for (col in columns) {
    table_data <- table(data[[col]])
    percentages <- round(prop.table(table_data) * 100, 2)
    
    # Fill in the result dataframe
    for (i in 1:length(names(table_data))) {
      category <- names(table_data)[i]
      result[category, col] <- percentages[category]
    }
  }
  
  return(result)
}

# Specify the columns you want to calculate percentages for
risk_columns <- c("risk_sswq_wellbeing", "risk_sswq_joy", "risk_sswq_connect", "risk_sswq_purpose", "risk_sswq_efficacy", "risk_yieps_extern", "risk_yieps_intern")

# Calculate risk percentages for all specified columns
risk_percentages <- calculate_risk_percentages(df, risk_columns)

# Print the result
print(risk_percentages)

# Create vectors for the variables sswq_wellbeing, sswq_joy, sswq_connect, sswq_purpose, and sswq_efficacy
sswq_wellbeing <- c(2.625, 2.8125, 1.8125, 2, 2.25, 2, 2, 2.4375, 2.9375, 2.5)
sswq_joy <- c(1.5, 1.75, 1.75, 2.25, 2.25, 2.25, 2.25, 2.75, 3.5, 2.75)
sswq_connect <- c(3, 3.25, 2.25, 2.5, 2.5, 1.75, 1.75, 1.75, 2, 3)
sswq_purpose <- c(3.25, 3.5, 1.5, 1.5, 1.75, 1.5, 1.75, 3, 3.5, 2.25)
sswq_efficacy <- c(2.75, 2.75, 1.75 ,1.75 ,2.5 ,2.5 ,2.25 ,2.25 ,3.5 ,2)
yieps_intern <- c(2.444444, 2.5, 1.8, 1.9, 3, 3.3, 2.2, 2.5, 1.9, 1.7)
yieps_extern <- c(2.4, 2.2, 2.1, 2.3, 1.8, 2.4, 3, 2.2, 2, 2.7)

# Calculate the mean for each variable
mean_sswq_wellbeing <- mean(sswq_wellbeing)
mean_sswq_joy <- mean(sswq_joy)
mean_sswq_connect <- mean(sswq_connect)
mean_sswq_purpose <- mean(sswq_purpose)
mean_sswq_efficacy <- mean(sswq_efficacy)
mean_yieps_intern <- mean(yieps_intern)
mean_yieps_extern <- mean(yieps_extern)

# Print the mean values for each variable
print(mean_sswq_wellbeing)
print(mean_sswq_joy)
print(mean_sswq_connect)
print(mean_sswq_purpose)
print(mean_sswq_efficacy)
print(mean_yieps_intern)
print(mean_yieps_extern)


#Exports the modified file into the given path.
# write_xlsx(df, path = "/Users/kunal/Desktop/Work/Output_Whole_Survey_Set_Secondary_Df_Test.xlsx")


library(plotly)

# Convert row names to a column in risk_percentages dataframe
risk_percentages$Risk_Category <- rownames(risk_percentages)

# Reshape the data for plotting
plot_data <- reshape2::melt(risk_percentages, id.vars = "Risk_Category")

# Create an interactive bar plot
p <- plot_ly(plot_data, x = ~variable, y = ~value, color = ~Risk_Category, type = 'bar') %>%
  layout(title = "Risk Category Distribution",
         xaxis = list(title = "Risk Factors"),
         yaxis = list(title = "Percentage"),
         barmode = 'group')

# Print the interactive plot
print(p)

# Create a subset of the data frame with only the demographic columns
demographic_cols <- c("name_of_school_v2", "dob_v2", "age_v2", "student_grade_v2", "race_ethnicity_v2_0", "race_ethnicity_v2_1", "race_ethnicity_v2_2", "race_ethnicity_v2_3", "race_ethnicity_v2_4", "race_ethnicity_v2_5", "race_ethnicity_v2_6", "race_ethnicity_v2_7", "race_ethnicity_v2_8", "race_ethnicity_v2_9", "gender_identity_v2", "sexual_orientation")
demographic_data <- df[, demographic_cols]

# Loop through each column and calculate frequency and percentage
for (col in demographic_cols) {
  # Calculate frequency
  freq <- table(demographic_data[[col]])
  
  # Calculate percentage
  perc <- round(prop.table(freq) * 100, 2)
  
  # Print results
  cat(paste0("Variable: ", col, "\n"))
  print(cbind(Frequency = freq, Percentage = perc))
  cat("\n\n")
}

# Load required packages
library(ggplot2)
library(dplyr)
library(cowplot)

# Function to plot frequency and percentage for a categorical variable
plot_freq_perc <- function(data, var) {
  freq_table <- data %>%
    group_by(.data[[var]]) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
    arrange(Count)
  
  plot <- ggplot(freq_table, aes(x = reorder(.data[[var]], Count), y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(Count, " (", Percentage, "%)"),
                  y = Count + max(Count) * 0.05), vjust = 0, size = 3, color = "firebrick") +
    labs(title = paste("Frequency and Percentage of", var),
         x = var, y = "Count") +
    theme_minimal()
  
  return(plot)
}

# Define demographic variables
demographic_vars <- c("name_of_school_v2", "student_grade_v2", "race_ethnicity_v2_0", "race_ethnicity_v2_1",
                      "race_ethnicity_v2_2", "race_ethnicity_v2_3", "race_ethnicity_v2_4", "race_ethnicity_v2_5",
                      "race_ethnicity_v2_6", "race_ethnicity_v2_7", "race_ethnicity_v2_8", "race_ethnicity_v2_9",
                      "gender_identity_v2", "sexual_orientation")

# Create a list of plots
plots <- lapply(demographic_vars, function(var) plot_freq_perc(data = df, var = var))

# Arrange plots in a grid
plot_grid(plotlist = plots, ncol = 3)



# Load required libraries
library(ggplot2)
library(reshape2)
library(plotly)

# Define demographic and outcome variables
demographic_vars <- c("name_of_school_v2", "age_v2", "student_grade_v2", "race_ethnicity_v2_0", "race_ethnicity_v2_1", "race_ethnicity_v2_2", "race_ethnicity_v2_3", "race_ethnicity_v2_4", "race_ethnicity_v2_5", "race_ethnicity_v2_6", "race_ethnicity_v2_7", "race_ethnicity_v2_8", "race_ethnicity_v2_9", "gender_identity_v2", "sexual_orientation")
outcome_vars <- c("sswq_wellbeing", "sswq_joy", "sswq_connect", "sswq_purpose", "sswq_efficacy", "yieps_intern", "yieps_extern")
all_vars <- c(demographic_vars, outcome_vars)

# Calculate correlation matrix
cor_matrix <- cor(df[, all_vars], use = "pairwise.complete.obs")

# Reshape the correlation matrix into long format
cor_matrix_long <- melt(cor_matrix)

# Create the interactive heatmap
interactive_heatmap <- ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Heatmap")

# Convert the ggplot object to a plotly object
interactive_heatmap <- ggplotly(interactive_heatmap)

# Display the interactive heatmap
interactive_heatmap


# Example data
group <- c(rep("A", 10), rep("B", 10), rep("C", 10))  # Example groups
outcome <- c(rnorm(10, mean = 5, sd = 2),            # Example outcome variable
             rnorm(10, mean = 7, sd = 2),
             rnorm(10, mean = 6, sd = 2))

# Create a data frame
data <- data.frame(Group = group, Outcome = outcome)

# Perform ANOVA
anova_result <- aov(Outcome ~ Group, data = data)

# Print ANOVA result
print(summary(anova_result))
