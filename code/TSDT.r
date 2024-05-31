library(readxl)
library(dplyr)
library(writexl)


df <- read_excel("/Users/kunal/Desktop/Work/Teacher Survey Data TEST.xlsx")
names(df)

# Apply the mapping to your data
df$tswq_yrsteaching <- recode(df$tswq_yrsteaching,
                                     "1st year teaching" = 0,
                                     "1-3 years teaching" = 1,
                                     "3-5 years teaching" = 2,
                                     "5-8 years teaching" = 3,
                                     "8-12 years teaching" = 4,
                                     "12 years teaching or more" = 5)

# Print the updated column
print(df$tswq_yrsteaching)


#Converts all the Teacher Wellbeing Teacher Report Scale(tswq) values to numeric values from 1 to 8.
columns_to_recode <- c("tswq_1", "tswq_2", "tswq_3", "tswq_4", "tswq_5", "tswq_6", "tswq_7", "tswq_8")

df[columns_to_recode] <- lapply(df[columns_to_recode], function(x) {
  recode(x,
         "Almost Never" = 1,
         "Sometimes" = 2,
         "Often" = 3,
         "Almost Always" = 4)
})

#Mean of all the tswq columns(2,4,6,8).
df$tswq_efficacy <- rowMeans(df[, c("tswq_2", "tswq_4", "tswq_6", "tswq_8")], na.rm = TRUE)

#Mean of all the tswq columns(1,3,5,7).
df$tswq_connect <- rowMeans(df[, c("tswq_1", "tswq_3", "tswq_5", "tswq_7")], na.rm = TRUE)

#Mean of all the tswq columns(1,2,3,4,5,6,7,8).
df$tswq_wellbeing <- rowMeans(df[, c("tswq_1", "tswq_2", "tswq_3", "tswq_4", "tswq_5", "tswq_6", "tswq_7", "tswq_8")], na.rm = TRUE)


#Converts all the Social Emotional Learning Integration Scale(sel) values to numeric values from 1 to 8.
columns_to_recode <- c("sel_1", "sel_2", "sel_3", "sel_4", "sel_5", "sel_6", "sel_7", "sel_8", "sel_9", "sel_10", "sel_11", "sel_12")

df[columns_to_recode] <- lapply(df[columns_to_recode], function(x) {
  recode(x,
         "Strongly Agree" = 1,
         "Agree" = 2,
         "Neither Agree nor Disagree" = 3,
         "Disagree" = 4,
         "Strongly Disagree" = 5)
})

#Mean of all the sel columns(5,7,8,9).
df$sel_comfort <- rowMeans(df[, c("sel_5", "sel_7", "sel_8", "sel_9")], na.rm = TRUE)

#Mean of all the sel columns(3,4,11,12).
df$sel_commitment <- rowMeans(df[, c("sel_3", "sel_4", "sel_11", "sel_12")], na.rm = TRUE)

#Mean of all the sel columns(1,2,6,10).
df$sel_culture <- rowMeans(df[, c("sel_1", "sel_2", "sel_6", "sel_10")], na.rm = TRUE)

#Mean of all the sel columns(1-12).
df$sel_total <- rowMeans(df[, c("sel_1", "sel_2", "sel_3", "sel_4", "sel_5", "sel_6", "sel_7", "sel_8","sel_9","sel_10", "sel_11", "sel_12")], na.rm = TRUE)



#Exports the modified file into the given path.
# write_xlsx(df, path = "/Users/kunal/Desktop/Work/Output_Teacher_Survey_df_Test.xlsx")

categorize_risk <- function(score, at_risk_cutoff, high_risk_cutoff) {
  if (score < at_risk_cutoff) {
    return("No Risk")
  } else if (score < high_risk_cutoff) {
    return("At Risk")
  } else {
    return("High Risk")
  }
}

# Apply the categorization function to each column
df$risk_tswq_efficacy <- mapply(categorize_risk, df$tswq_efficacy, at_risk_cutoff = 2.5, high_risk_cutoff = 1.8)
df$risk_tswq_connect <- mapply(categorize_risk, df$tswq_connect, at_risk_cutoff = 2.4, high_risk_cutoff = 1.7)
df$risk_tswq_wellbeing <- mapply(categorize_risk, df$tswq_wellbeing, at_risk_cutoff = 2.5, high_risk_cutoff = 1.8)

# Print the modified data frame
print(df$risk_tswq_efficacy)
# write_xlsx(df, path = "/Users/kunal/Desktop/Work/Output_Teacher_Survey_df_Test.xlsx")


categorize_sel_risk <- function(score, at_risk_cutoff, high_risk_cutoff) {
  if (score < at_risk_cutoff) {
    return("No Risk")
  } else if (score < high_risk_cutoff) {
    return("At Risk")
  } else {
    return("High Risk")
  }
}

# Apply the categorization function to each column
df$risk_sel_comfort <- mapply(categorize_sel_risk, df$sel_comfort, at_risk_cutoff = 2.7, high_risk_cutoff = 1.8)
df$risk_sel_commitment <- mapply(categorize_sel_risk, df$sel_commitment, at_risk_cutoff = 2.9, high_risk_cutoff = 2.0)
df$risk_sel_culture <- mapply(categorize_sel_risk, df$sel_culture, at_risk_cutoff = 3.1, high_risk_cutoff = 2.3)
df$risk_sel_total_score <- mapply(categorize_sel_risk, df$sel_total, at_risk_cutoff = 3.1, high_risk_cutoff = 2.4)

print(df)

write_xlsx(df, path = "/Users/kunal/Desktop/Work/Output_Teacher_Survey_df_Test.xlsx")


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
risk_columns <- c("risk_tswq_efficacy", "risk_tswq_connect", "risk_tswq_wellbeing",
                  "risk_sel_comfort", "risk_sel_commitment", "risk_sel_culture", "risk_sel_total_score")

# Calculate risk percentages for all specified columns
risk_percentages <- calculate_risk_percentages(df, risk_columns)

# Print the result
print(risk_percentages)


# Create vectors for the variables tswq_efficacy, tswq_connect, and tswq_wellbeing
tswq_efficacy <- c(2.25, 1.75, 2, 2.75, 1.75, 2.5, 2, 3, 3.75, 3.25)
tswq_connect <- c(2.25, 2, 2.25, 3, 1.75, 2, 1.75, 2.75, 3, 2)
tswq_wellbeing <- c(2.25, 1.875, 2.125, 2.875, 1.75, 2.25, 1.875, 2.875, 3.375, 2.625)

# Calculate the mean for each variable
mean_tswq_efficacy <- mean(tswq_efficacy)
mean_tswq_connect <- mean(tswq_connect)
mean_tswq_wellbeing <- mean(tswq_wellbeing)

# Calculate the standard deviation for each variable
sd_tswq_efficacy <- sd(tswq_efficacy)
sd_tswq_connect <- sd(tswq_connect)
sd_tswq_wellbeing <- sd(tswq_wellbeing)

# Calculate the range (lowest and highest score) for each variable
range_tswq_efficacy <- range(tswq_efficacy)
range_tswq_connect <- range(tswq_connect)
range_tswq_wellbeing <- range(tswq_wellbeing)

# Print the mean, standard deviation, and range values for each variable
print(paste("tswq_efficacy: Mean =", mean_tswq_efficacy, ", SD =", sd_tswq_efficacy, ", Range =", range_tswq_efficacy))
print(paste("tswq_connect: Mean =", mean_tswq_connect, ", SD =", sd_tswq_connect, ", Range =", range_tswq_connect))
print(paste("tswq_wellbeing: Mean =", mean_tswq_wellbeing, ", SD =", sd_tswq_wellbeing, ", Range =", range_tswq_wellbeing))

# Load the plotly library for interactive plots
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


# Get frequency counts for tswq_yrsteaching
freq_tswq_yrsteaching <- table(df$tswq_yrsteaching)

# Calculate percentages
percent_tswq_yrsteaching <- prop.table(freq_tswq_yrsteaching) * 100

# Create a dataframe with only frequency and percentage columns
freq_perc_tswq_yrsteaching <- data.frame(Frequency = as.numeric(freq_tswq_yrsteaching), Percentage = as.numeric(percent_tswq_yrsteaching))

# Print the result
print(freq_perc_tswq_yrsteaching)

# Create a matrix containing frequency and percentages
data_matrix <- cbind(freq_tswq_yrsteaching, percent_tswq_yrsteaching)

# Define bar colors
bar_colors <- c("blue", "red")

# Plot the grouped bar plot
barplot(data_matrix, beside = TRUE, col = bar_colors,
        main = "Frequency and Percentage of Years Teaching",
        xlab = "Years Teaching", ylab = "Frequency/Percentage",
        legend.text = c("Frequency", "Percentage"), args.legend = list(x = "topright"))

# Load required libraries
library(ggplot2)
library(reshape2)
library(plotly)

# Define demographic and outcome variables
demographic_vars <- c("tswq_yrsteaching")
outcome_vars <- c("tswq_efficacy", "tswq_connect", "tswq_wellbeing", "sel_comfort", "sel_commitment", "sel_culture", "sel_total")
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

# Perform ANOVA for each outcome variable
for (var in outcome_vars) {
  anova_result <- aov(formula(paste(var, "~", "tswq_yrsteaching")), data = df)
  print(paste("ANOVA for", var))
  print(summary(anova_result))
  print("----------------------------------")
}