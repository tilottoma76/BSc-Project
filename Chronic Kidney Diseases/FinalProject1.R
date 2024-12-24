# Load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("moments")
install.packages("naniar")
install.packages("fmsb")
install.packages("GGally")

library(dplyr)
library(ggplot2)
library(tidyr)
library(moments)
library(naniar)
library(fmsb)
library(GGally)

# Load the dataset (update the file path if needed)
Dataset <- read.csv("C:/Users/Acer/OneDrive/Desktop/Data Science Final Project/Chronic_Kidney_Dsease_data.csv")
Dataset

# Inspect the dataset to identify columns
summary(Dataset)
str(Dataset)

# Select numeric columns
numeric_data <- select_if(Dataset, is.numeric)

# Correlation Analysis
correlation <- cor(numeric_data, method = "pearson")

# Identify positive, negative, and zero correlations
positive_correlation <- correlation[correlation > 0.5]
negative_correlation <- correlation[correlation < -0.5]
zero_correlation <- correlation[correlation > -0.1 & correlation < 0.1]

# Correlation Data Frame
correlation_data <- data.frame(
  Correlation = c(positive_correlation, negative_correlation, zero_correlation),
  Type = c(rep("Positive", length(positive_correlation)),
           rep("Negative", length(negative_correlation)),
           rep("Zero", length(zero_correlation)))
)

# Histogram of Correlation Values
histogram_plot <- ggplot(correlation_data, aes(x = Correlation, fill = Type)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7, color = "black") +
  labs(title = "Histogram of Correlation Values",
       x = "Correlation Value",
       y = "Frequency",
       fill = "Correlation Type") +
  theme_minimal()
print(histogram_plot)

# Example Line Histogram (Replace with actual column name)

specific_column_name <- c("PatientID", "Age", "Gender", "Ethnicity", "SocioeconomicStatus", "EducationLevel", "BMI", "Smoking", "AlcoholConsumption", "PhysicalActivity", "DietQuality", "SleepQuality", "FamilyHistoryKidneyDisease", "FamilyHistoryHypertension", "FamilyHistoryDiabetes", "PreviousAcuteKidneyInjury", "UrinaryTractInfections", "SystolicBP", "DiastolicBP", "FastingBloodSugar", "HbA1c", "SerumCreatinine", "BUNLevels", "GFR", "ProteinInUrine", "ACR", "SerumElectrolytesSodium", "SerumElectrolytesPotassium", "SerumElectrolytesCalcium", "SerumElectrolytesPhosphorus", "Diuretics", "NSAIDsUse", "Statins", "AntidiabeticMedications", "Edema", "FatigueLevels", "NauseaVomiting", "MuscleCramps", "Itching", "QualityOfLifeScore", "HeavyMetalsExposure", "OccupationalExposureChemicals", "WaterQuality", "MedicalCheckupsFrequency", "MedicationAdherence", "HealthLiteracy", "Diagnosis", "DoctorInCharge")

line_histogram_age <- ggplot(numeric_data, aes(x = Age)) +
  geom_density(color = "blue", fill = "lightblue") +
  labs(title = "Line Histogram of Age",
       x = "Age",
       y = "Density") +
  theme_minimal()

print(line_histogram_age)

line_histogram_serum_creatinine <- ggplot(numeric_data, aes(x = SerumCreatinine)) +
  geom_density(color = "blue", fill = "lightblue") +
  labs(title = "Line Histogram of Serum Creatinine",
       x = "Serum Creatinine",
       y = "Density") +
  theme_minimal()

print(line_histogram_serum_creatinine)


# Mean, Median, Mode
mean_value <- colMeans(numeric_data, na.rm = TRUE)
median_value <- apply(numeric_data, 2, median, na.rm = TRUE)

# Mode Function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

modes <- sapply(numeric_data, Mode)

# Skewness
skewness_values <- apply(numeric_data, 2, skewness, na.rm = TRUE)

# Data frame of mean, median, mode
Mean_Median_Mode <- data.frame(
  attributes = names(numeric_data),
  mean = mean_value,
  median = median_value,
  mode = modes
)

# Plot Mean, Median, Mode
stats_long <- pivot_longer(Mean_Median_Mode, cols = -attributes, names_to = "stat", values_to = "value")
MMM_Plot <- ggplot(stats_long, aes(x = attributes, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Plot of Mean, Median, Mode value",
       x = "Attributes",
       y = "Values",
       fill = "Statistics") +
  theme_minimal()

print(MMM_Plot)

# Scatter Plot Example (Replace with actual column names)
scatter_plot <- ggplot(numeric_data, aes(x = Age, y = SerumCreatinine)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Scatter Plot of Age vs Serum Creatinine",
       x = "Age",
       y = "Serum Creatinine") +
  theme_minimal()

print(scatter_plot)

# Scatter Matrix
scatter_matrix <- ggpairs(numeric_data, title = "Scatter Matrix for Numeric Data")
print(scatter_matrix)

# Violin Plot (Replace 'categorical_column' with an appropriate column name)
violin_plot <- ggplot(numeric_data, aes(x = as.factor(categorical_column), y = numeric_column, fill = categorical_column)) +
  geom_violin() +
  labs(title = "Violin Plot of Numeric Column vs Categorical Column",
       x = "Categorical Column",
       y = "Numeric Column") +
  theme_minimal()
print(violin_plot)

# Radar Chart (Adjust the attributes based on the actual data)
# Placeholder example assuming you have columns like 'age', 'bp', etc.
radar_data <- data.frame(
  max = c(100, 100, 100), # replace with actual max values
  min = c(0, 0, 0), # replace with actual min values
  Age = c(average_age_value, ..., ...),  # Fill in with actual numeric attributes
  ...
)
radarchart(radar_data, axistype = 1,
           pcol = c("blue", "red"),
           pfcol = rgb(0.2, 0.5, 0.5, 0.5),
           plwd = 4,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 20),
           vlcex = 0.8,
           title = "Radar Chart")

# Line Graph Example
line_graph <- ggplot(numeric_data, aes(x = age_column, y = value_column, color = category, group = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Line Graph Example",
       x = "Age",
       y = "Value") +
  theme_minimal()
print(line_graph)
