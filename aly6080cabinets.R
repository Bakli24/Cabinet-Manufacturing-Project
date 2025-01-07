cabinets<- read.csv("/Users/tanishqbakliwal/Desktop/XN Project/group/Quote_Project Register (V2) (1).csv")

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Display the structure of the dataset
str(cabinets)

# Summary statistics for numerical columns
summary(cabinets)

# Summary statistics for categorical columns
sapply(cabinets, function(x) length(unique(x)))

# Check for missing values
colSums(is.na(cabinets))

# Convert date columns to Date objects using lubridate (only if they are not empty)
date_columns <- c("Tender_Close", "Fab_Start_Date", "Estimator_Approval", "Requested_Due_Date", "Ship_Date")
for (col in date_columns) {
  if (!all(is.na(cabinets[[col]]))) {
    cabinets[[col]] <- lubridate::mdy(cabinets[[col]])
  }
}

#barplot
ggplot(cabinets, aes(x = Type)) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Project Types", y = "Count")


# Scatter plot for Job Value and Sales Value
ggplot(cabinets, aes(x = Job_Value, y = Sales_Value)) +
  geom_point() +
  labs(title = "Scatter Plot of Job Value vs Sales Value")

# Boxplot for Closing Ratio
ggplot(cabinets, aes(x = Closing_Ratio)) +
  geom_boxplot() +
  labs(title = "Boxplot of Closing Ratio")

# Correlation matrix
num_cols <- sapply(cabinets, is.numeric)
cor(cabinets[, num_cols])
