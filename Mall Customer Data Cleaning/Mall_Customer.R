install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("moments")
library(moments)
install.packages("rlang")
library(rlang)
install.packages("naniar")
library(naniar)
install.packages("fmsb")
library(fmsb)
install.packages("GGally")
library(GGally)
install.packages("factoextra")
library(factoextra)


Dataset <- read.csv("C:/Users/tahni.TAHNIA_TISHA/OneDrive/Desktop/10th Semester/Data Science/Final/Project_1/Mall_Customers.csv")
Dataset

summary(Dataset)
str(Dataset)

numeric_data <- select_if(Dataset, is.numeric)
numeric_data

correlation <- cor(numeric_data, method = "pearson")
correlation
positive_correlation <- correlation[correlation > 0.5]  
negative_correlation <- correlation[correlation < -0.5] 
zero_correlation <- correlation[correlation > -0.1 & correlation < 0.1] 
positive_correlation
negative_correlation
zero_correlation

correlation_data <- data.frame(
  Correlation = c(positive_correlation, negative_correlation, zero_correlation),
  Type = c(rep("Positive", length(positive_correlation)),
           rep("Negative", length(negative_correlation)),
           rep("Zero", length(zero_correlation)))
)
correlation_data
head(correlation_data)

#Histogram
histogram_plot <- ggplot(correlation_data, aes(x = Correlation, fill = Type)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7, color = "black") +
  labs(title = "Histogram of Correlation Values",
       x = "Correlation Value",
       y = "Frequency",
       fill = "Correlation Type") +
  theme_minimal()
histogram_plot

#Line Histogram
line_histogram_age <- ggplot(numeric_data, aes(x = Age)) +
  geom_density(color = "blue", fill = "lightblue") +
  labs(title = "Line Histogram of Age",
       x = "Age",
       y = "Density") +
  theme_minimal()
line_histogram_age

line_histogram_Annual.Income..k.. <- ggplot(numeric_data, aes(x = Annual.Income..k..)) +
  geom_density(color = "blue", fill = "lightgreen") +
  labs(title = "Line Histogram of Annual.Income..k..",
       x = "Age",
       y = "Density") +
  theme_minimal()
line_histogram_Annual.Income..k..

line_histogram_Spending.Score..1.100. <- ggplot(numeric_data, aes(x = Spending.Score..1.100.)) +
  geom_density(color = "blue", fill = "lightsalmon") +
  labs(title = "Line Histogram of Spending.Score..1.100.",
       x = "Age",
       y = "Density") +
  theme_minimal()
line_histogram_Spending.Score..1.100.

#Mean, Median, Mode
mean_value <- colMeans(numeric_data)
mean_value
median_value <- apply(numeric_data,2,median)
median_value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modes <- function(df) {
  modes <- sapply(df, function(column) {
    Mode(column)
  })
  return(modes)
}

mode_value <- modes(numeric_data)
mode_value

skewness_values <- apply(numeric_data, 2, skewness)
skewness_values

col_name <- c("CustomerID","Age","Annual.Income..k..","Spending.Score..1.100.")

Mean_Median_Mode <- data.frame(
  attributes = col_name,
  mean = mean_value,
  median = median_value,
  mode = mode_value
)

stats_long <- pivot_longer(Mean_Median_Mode, cols = -attributes, names_to = "stat", values_to = "value")

MMM_Plot <- ggplot(stats_long, aes(x = attributes, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Plot of Mean, Median, Mode value",
       x = "Attributes",
       y = "Mean, Median & Mode Values",
       fill = "Statistics") +
  theme_minimal()
MMM_Plot

positive_skew <- skewness_values[skewness_values > 0]
negative_skew <- skewness_values[skewness_values < 0]
zero_skew <- skewness_values[skewness_values >= -0.1 & skewness_values <= 0.1]

positive_data <- data.frame(attribute = names(positive_skew), skewness = positive_skew, type = "Positive Skewness")
negative_data <- data.frame(attribute = names(negative_skew), skewness = negative_skew, type = "Negative Skewness")
zero_data <- data.frame(attribute = names(zero_skew), skewness = zero_skew, type = "Zero Skewness")

combined_data <- rbind(positive_data, negative_data, zero_data)

skewness_plot <- ggplot(combined_data, aes(x = attribute, y = skewness, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Skewness Values by Attribute",
       x = "Attributes",
       y = "Skewness",
       fill = "Skewness Type") +
  facet_wrap(~type, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
skewness_plot

MMM_filtered <- Mean_Median_Mode %>%
  filter(mean < median & median < mode)
MMM_filtered1 <- Mean_Median_Mode %>%
  filter(mean > median & median > mode)
MMM_filtered2 <- Mean_Median_Mode %>%
  filter(mean == median & median == mode)

stats_long <- pivot_longer(MMM_filtered, cols = -attributes, names_to = "stat", values_to = "value")
stats_long1 <- pivot_longer(MMM_filtered1, cols = -attributes, names_to = "stat", values_to = "value")
stats_long2 <- pivot_longer(MMM_filtered2, cols = -attributes, names_to = "stat", values_to = "value")

#Zero skewness
zero_skew_plot <- ggplot(stats_long2, aes(x = attributes, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Zero skewness",
       x = "Attributes",
       y = "Mean, Median & Mode Values",
       fill = "Statistics") +
  theme_minimal()
zero_skew_plot

#Negative Skewness
negative_skew_plot <- ggplot(stats_long1, aes(x = attributes, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Negative Skewness",
       x = "Attributes",
       y = "Mean, Median & Mode Values",
       fill = "Statistics") +
  theme_minimal()
negative_skew_plot
#Positive Skewness
positive_skew_plot <- ggplot(stats_long, aes(x = attributes, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Positive Skewness",
       x = "Attributes",
       y = "Mean, Median & Mode Values",
       fill = "Statistics") +
  theme_minimal()
positive_skew_plot

#bar graph
Dataset$Gender <- as.factor(Dataset$Gender)
gender_bar_plot <- ggplot(Dataset, aes(x = Gender)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal()
gender_bar_plot

# Convert Age into age groups (e.g., bins of 10 years)
Dataset$AgeGroup <- cut(Dataset$Age, breaks = seq(20, 70, by = 10), right = FALSE)
age_group_counts <- Dataset %>%
  count(AgeGroup)
bar_plot_age_group <- ggplot(age_group_counts, aes(x = AgeGroup, y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Number of Customers by Age Group",
       x = "Age Group",
       y = "Number of Customers") +
  theme_minimal()
bar_plot_age_group

long_data <- Dataset %>%
  select(Age, Annual.Income..k.., Spending.Score..1.100.) %>%
  pivot_longer(cols = c(Annual.Income..k.., Spending.Score..1.100.), 
               names_to = "Metric", 
               values_to = "Value")

combined_bar_plot <- ggplot(long_data, aes(x = Age, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Annual Income and Spending Score by Age",
       x = "Age",
       y = "Value") +
  scale_fill_manual(values = c("skyblue", "orange")) +  # Custom colors for bars
  theme_minimal()
combined_bar_plot


# Box plot
Box_CustomerID <- ggplot(data = Dataset, aes(x="",y=CustomerID))+geom_boxplot()+labs(title = "Boxplotting of Customer ID")
Box_CustomerID
Box_Gender <- ggplot(data = Dataset, aes(x="",y=Gender))+geom_boxplot()+labs(title = "Boxplotting of Gender")
Box_Gender
Box_Age <- ggplot(data = Dataset, aes(x="",y=Age))+geom_boxplot()+labs(title = "Boxplotting of Age")
Box_Age
Box_Annual.Income..k.. <- ggplot(data = Dataset, aes(x="",y=Annual.Income..k..))+geom_boxplot()+labs(title = "Boxplotting of Annual.Income..k..")
Box_Annual.Income..k..
Box_Spending.Score..1.100. <- ggplot(data = Dataset, aes(x="",y=Spending.Score..1.100.))+geom_boxplot()+labs(title = "Boxplotting of Spending.Score..1.100.")
Box_Spending.Score..1.100.

#Scatter Plot
scatter_plot <- ggplot(numeric_data, aes(x = Age, y = Annual.Income..k..)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Scatter Plot of Age vs Annual Income",
       x = "Age",
       y = "Annual Income (k$)") +
  theme_minimal()
scatter_plot

#Scatter Matrix
scatter_matrix <- ggpairs(numeric_data, title = "Scatter Matrix for Numeric Data")
scatter_matrix

#Violin Plot
violin_plot <- ggplot(numeric_data, aes(x = as.factor(Spending.Score..1.100.), y = Age, fill = Spending.Score..1.100.)) +
  geom_violin() +
  labs(title = "Violin Plot of Spending Score vs Age",
       x = "Spending Score",
       y = "Age") +
  theme_minimal()
violin_plot

#radar chart
gender_counts <- table(Dataset$Gender)
gender_props <- as.numeric(gender_counts) / sum(gender_counts) * 100

average_age <- tapply(Dataset$Age, Dataset$Gender, mean, na.rm = TRUE)
average_income <- tapply(Dataset$`Annual Income (k$)`, Dataset$Gender, mean, na.rm = TRUE)
average_spending <- tapply(Dataset$`Spending Score (1-100)`, Dataset$Gender, mean, na.rm = TRUE)

radar_data <- as.data.frame(rbind(
  rep(100, 4),                  # Max values
  rep(0, 4),                    # Min values
  c(gender_props[1], average_age["Male"], average_income["Male"], average_spending["Male"]),   # Male data
  c(gender_props[2], average_age["Female"], average_income["Female"], average_spending["Female"]) # Female data
))

colnames(radar_data) <- c("Gender Proportion", "Age", "Annual Income", "Spending Score")
rownames(radar_data) <- c("Max", "Min", "Male", "Female")

radarchart(radar_data,
           axistype = 1,
           pcol = c("blue", "red"),
           pfcol = rgb(0.2, 0.5, 0.5, 0.5),
           plwd = 4,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 20),
           vlcex = 0.8,
           title = "Radar Chart of Gender Distribution and Metrics")

# Line graph
long_data <- Dataset %>%
  select(Age, Annual.Income..k.., Spending.Score..1.100.) %>%
  pivot_longer(cols = c(Annual.Income..k.., Spending.Score..1.100.), 
               names_to = "Metric", 
               values_to = "Value")

line_graph <- ggplot(long_data, aes(x = Age, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual Income and Spending Score by Age",
       x = "Age",
       y = "Value") +
  scale_color_manual(values = c("skyblue", "orange")) +  # Custom colors for lines
  theme_minimal()
line_graph

distribution_features <- data.frame(
  Mean = mean_value,
  Median = median_value,
  Mode = mode_value,
  Skewness = skewness_values
)

normalized_features <- scale(distribution_features)

set.seed(123)  
kmeans_result <- kmeans(normalized_features, centers = 3, nstart = 25)

distribution_features$Cluster <- as.factor(kmeans_result$cluster)

fviz_cluster(kmeans_result, data = normalized_features, geom = "point", 
             stand = FALSE, ellipse = TRUE, show.clust.cent = TRUE) +
  labs(title = "Clustering of Data Columns Based on Distribution Properties")

distribution_features

col_name <- c("CustomerID","Age","Annual.Income..k..","Spending.Score..1.100.")

# Correlation
correlation <- cor(numeric_data, method = "pearson")
correlation
positive_correlation <- correlation[correlation > 0.5]  
negative_correlation <- correlation[correlation < -0.5] 
zero_correlation <- correlation[correlation > -0.1 & correlation < 0.1] 
positive_correlation
negative_correlation
zero_correlation

#Skewness 
skewness_values <- apply(numeric_data, 2, skewness)
skewness_values
positive_skew <- skewness_values[skewness_values > 0]
negative_skew <- skewness_values[skewness_values < 0]
zero_skew <- skewness_values[skewness_values >= -0.1 & skewness_values <= 0.1]

positive_data <- data.frame(attribute = names(positive_skew), skewness = positive_skew, type = "Positive Skewness")
negative_data <- data.frame(attribute = names(negative_skew), skewness = negative_skew, type = "Negative Skewness")
zero_data <- data.frame(attribute = names(zero_skew), skewness = zero_skew, type = "Zero Skewness")

positive_data
negative_data
zero_data

# 1. Histogram for Age, Annual Income, Spending Score, and Gender distribution
# Histogram for Age
ggplot(Dataset, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
  theme_minimal()

# Histogram for Annual Income
ggplot(Dataset, aes(x = Annual.Income..k..)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Annual Income", x = "Annual Income (k$)", y = "Frequency") +
  theme_minimal()

# Histogram for Spending Score
ggplot(Dataset, aes(x = Spending.Score..1.100.)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Spending Score", x = "Spending Score", y = "Frequency") +
  theme_minimal()

# 2. Line Histograms (overlay density curve)
# Line Histogram for Age vs Annual Income
ggplot(Dataset, aes(x = Age, y = Annual.Income..k..)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Line Histogram: Age vs Annual Income", x = "Age", y = "Annual Income (k$)") +
  theme_minimal()

# Line Histogram for Age vs Spending Score
ggplot(Dataset, aes(x = Age, y = Spending.Score..1.100.)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Line Histogram: Age vs Spending Score", x = "Age", y = "Spending Score") +
  theme_minimal()

# Line Histogram for Gender vs Annual Income
ggplot(Dataset, aes(x = Gender, y = Annual.Income..k..)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Boxplot: Gender vs Annual Income", x = "Gender", y = "Annual Income (k$)") +
  theme_minimal()

# Line Histogram for Gender vs Spending Score
ggplot(Dataset, aes(x = Gender, y = Spending.Score..1.100.)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Boxplot: Gender vs Spending Score", x = "Gender", y = "Spending Score") +
  theme_minimal()

# 3. Scatter Plots
# Scatter Plot for Age vs Annual Income
ggplot(Dataset, aes(x = Age, y = Annual.Income..k..)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot: Age vs Annual Income", x = "Age", y = "Annual Income (k$)") +
  theme_minimal()


# Scatter Plot for Age vs Spending Score
ggplot(Dataset, aes(x = Age, y = Spending.Score..1.100.)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot: Age vs Spending Score", x = "Age", y = "Spending Score") +
  theme_minimal()

# Scatter Plot for Annual Income vs Spending Score
ggplot(Dataset, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(color = "green") +
  labs(title = "Scatter Plot: Annual Income vs Spending Score", x = "Annual Income (k$)", y = "Spending Score") +
  theme_minimal()

#radar chart
summary_data <- Dataset %>%
  group_by(Gender) %>%
  summarise(
    Avg_Age = mean(Age),
    Avg_Annual_Income = mean(Annual.Income..k..),
    Avg_Spending_Score = mean(Spending.Score..1.100.)
  )
summary_data <- as.data.frame(summary_data)
radar_data <- rbind(
  max = c(80, 150, 100),   # Max values for Age, Annual Income, and Spending Score
  min = c(18, 15, 0),      # Min values for Age, Annual Income, and Spending Score
  summary_data[summary_data$Gender == "Male", 2:4],
  summary_data[summary_data$Gender == "Female", 2:4]
)
rownames(radar_data) <- c("Max", "Min", "Male", "Female")
radarchart(radar_data, 
           axistype = 1, 
           pcol = c("blue", "red"), 
           pfcol = c(rgb(0.2,0.5,0.5,0.5), rgb(0.8,0.2,0.5,0.5)), 
           plwd = 2, 
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, 100, 20), 
           title = "Radar Chart Comparing Male and Female Customers"
           )
legend(x = "topright", 
       legend = c("Male", "Female"), 
       col = c("blue", "red"), 
       lwd = 2)

# 4. Violin Plot for Gender vs Age, Annual Income, and Spending Score
# Violin Plot for Age by Gender
ggplot(Dataset, aes(x = Gender, y = Age, fill = Gender)) +
  geom_violin() +
  labs(title = "Violin Plot: Gender vs Age", x = "Gender", y = "Age") +
  theme_minimal()

# Violin Plot for Annual Income by Gender
ggplot(Dataset, aes(x = Gender, y = Annual.Income..k.., fill = Gender)) +
  geom_violin() +
  labs(title = "Violin Plot: Gender vs Annual Income", x = "Gender", y = "Annual Income (k$)") +
  theme_minimal()

# Violin Plot for Spending Score by Gender
ggplot(Dataset, aes(x = Gender, y = Spending.Score..1.100., fill = Gender)) +
  geom_violin() +
  labs(title = "Violin Plot: Gender vs Spending Score", x = "Gender", y = "Spending Score") +
  theme_minimal()

# 5. Box Plots
# Box Plot for Age
ggplot(Dataset, aes(y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot: Age Distribution", y = "Age") +
  theme_minimal()

# Box Plot for Annual Income
ggplot(Dataset, aes(y = Annual.Income..k..)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot: Annual Income Distribution", y = "Annual Income (k$)") +
  theme_minimal()

# Box Plot for Spending Score
ggplot(Dataset, aes(y = Spending.Score..1.100.)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Boxplot: Spending Score Distribution", y = "Spending Score") +
  theme_minimal()


