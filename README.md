# Data_Visualization_Credit_Risk_Analysis
This project explores data visualization techniques in R. It includes data cleaning, analysis, and the creation of visualizations such as histograms, scatter plots, boxplots, and bar charts using ggplot2 and tidyverse to highlight key patterns and insights in the dataset.

This repository contains the work completed for **Data Visualization Assignment 2** in R.  
The project explores data visualization techniques in R using `ggplot2` and other relevant packages.

## Contents

- `Data Visualization Assignment2.Rproj` – RStudio project file to manage the workspace.
- `scripts/` – R scripts containing data cleaning, analysis, and visualization code.
- `data/` – Raw and processed datasets used in the assignment.
- `plots/` – Exported charts and visualizations.

## Key Work Completed

- Imported and cleaned dataset(s) for analysis.
- Created a range of visualizations including:
  - Histograms and density plots for distribution analysis
  - Scatter plots with trend lines for correlation analysis
  - Boxplots and violin plots for comparison between groups
  - Heatmaps and bar charts for categorical insights
- Applied appropriate themes, labels, and color palettes for clarity and aesthetics.
- Interpreted results to highlight patterns and insights from the data.

## Requirements

- R (≥ 4.0)
- RStudio (optional, recommended)
- Packages:
  - `ggplot2`
  - `dplyr`
  - `tidyr`
  - `readr`
  - `cowplot` (or other visualization helpers)

Install dependencies with:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "cowplot"))


---
title: "Final_assignment_2617228"
author: "Shreeja_2617228"
date: "`r Sys.Date()`"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)


# Read the Excel file
file_path <- "C:/Users/shree/Downloads/Data Visualization/BankData/BankChurners.xlsx"
bank_data <- read_excel(file_path, sheet = "BankChurners")

# Check for missing values
missing_values <- colSums(is.na(bank_data))
print("Missing Values in Each Column:")
print(missing_values)
```
```{r}
# Clean and Prepare Data
# Step 1: Remove duplicates (if any)
bank_data <- bank_data %>% distinct()
print("Duplicate Values in Each Column:")
print(bank_data)
```
```{r}
# Example: Convert "Attrition_Flag" column to binary
# Existing Customer = 1, Attrited Customer = 0
bank_data$Attrition_Flag_Binary <- ifelse(bank_data$Attrition_Flag == "Existing Customer", 1, 0)

```


```{r}
# Create Credit Limit Range bins
bank_data <- bank_data %>%
  mutate(
    Credit_Limit_Range = cut(
      Credit_Limit,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("$0-$5K", "$5K-$10K", "$10K-$15K", "$15K-$20K", ">$20K"),
      right = FALSE
    )
  )
```

```{r}
# Ensure all required columns are in proper format
# Convert categorical variables to factors
bank_data$Attrition_Flag <- as.factor(bank_data$Attrition_Flag)
bank_data$Card_Category <- as.factor(bank_data$Card_Category)
bank_data$Income_Category <- as.factor(bank_data$Income_Category)
```

```{r}
#Sort data by Credit Limit Range and Total Transaction Amount
bank_data <- bank_data %>%
  arrange(Credit_Limit_Range, desc(Total_Trans_Amt))

# Remove unnecessary columns
bank_data <- bank_data[ , !names(bank_data) %in% c("CLIENTNUM", "Marital_Status", "Education_Level")]

```

```{r}
# Display cleaned and sorted data
print("Cleaned and Sorted Data Sample:")
print(head(bank_data))
view(bank_data)
```
```{r}
summary(bank_data)
```
```{r}
# Load necessary libraries
library(ggplot2)
library(readxl)
library(viridis)  # For colorblind-friendly palettes



# Convert 'Attrition_Flag_Binary' 
bank_data$Attrition_Flag_Binary <- as.factor(bank_data$Attrition_Flag_Binary)


# Create the boxplot with colorblind-friendly colors and a legend
ggplot(bank_data, aes(x = Attrition_Flag_Binary, y = Avg_Utilization_Ratio, fill = Attrition_Flag)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Remove outliers for clarity and add transparency
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +  # Add mean points
  scale_fill_viridis(discrete = TRUE, option = "D", name = "Attrition Status") +  # Use Viridis palette and add legend title
  labs(
    title = "Credit Utilization vs Attrition Status",
    x = "Attrition Status (Binary)",
    y = "Average Utilization Ratio"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend on the right
    plot.title = element_text(hjust = 0.5)  # Center-align title
  )

```


```{r}
# Create age group bins for categorization
bank_data$Age_Group <- cut(
  bank_data$Customer_Age,
  breaks = c(18, 30, 40, 50, 60, 70, Inf),
  labels = c("18-30", "31-40", "41-50", "51-60", "61-70", "71+"),
  right = FALSE
)

# Create the scatter plot with colorblind-friendly colors
ggplot(bank_data, aes(x = Customer_Age, y = Total_Trans_Amt, color = Age_Group)) +
  geom_point(alpha = 0.7) +  # Semi-transparent points
  geom_smooth(method = "loess", color = "black", se = FALSE) +  # Black trend line without confidence interval
  scale_color_viridis(discrete = TRUE, option = "D", name = "Age Groups") +  # Colorblind-friendly color palette
  labs(
    title = "Age vs Total Transaction Amount",
    x = "Age",
    y = "Total Transaction Amount"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center-align title
    legend.position = "right"  # Position legend on the right
  )
```


```{r}
# Create Credit Limit Range bins for grouping
bank_data <- bank_data %>%
  mutate(
    Credit_Limit_Bins = cut(
      Credit_Limit,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("$0-$5K", "$5K-$10K", "$10K-$15K", "$15K-$20K", ">$20K"),
      right = FALSE
    )
  )

# Calculate the average transaction amount for each Credit Limit Range and Attrition Status
trans_heatmap <- bank_data %>%
  group_by(Credit_Limit_Bins, Attrition_Flag) %>%
  summarise(Avg_Trans_Amt = mean(Total_Trans_Amt, na.rm = TRUE)) %>%
  ungroup()

# Create the heatmap
ggplot(trans_heatmap, aes(x = Credit_Limit_Bins, y = Attrition_Flag, fill = Avg_Trans_Amt)) +
  geom_tile(color = "white") +  # Add white gridlines between tiles
  scale_fill_viridis(option = "G", name = "Avg Transaction Amount") +  # Use colorblind-friendly palette
  labs(
    title = "Average Transaction Amount by Credit Limit Range and Attrition Status",
    x = "Credit Limit Range",
    y = "Attrition Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center-align the title
    
  )

```


```{r}
ggplot(data_summary, aes(x = Count, y = reorder(Card_Category, Count), fill = Income_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE, name = "Income Category") +
  labs(
    title = "Card Type Preferences by Income Category",
    x = "Count",
    y = "Card Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )



```

```{r}
# Create bins for Credit_Limit
data$Credit_Limit_Bins <- cut(
  data$Credit_Limit,
  breaks = quantile(data$Credit_Limit, probs = seq(0, 1, 0.2), na.rm = TRUE),  # Use quintiles for balanced bins
  labels = c("Very Low", "Low", "Medium", "High", "Very High"),
  include.lowest = TRUE
)

# Boxplot: Total Transaction Amount by Credit Limit Range
ggplot(data, aes(x = Credit_Limit_Bins, y = Total_Trans_Amt, fill = Credit_Limit_Bins)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.size = 1.5) +  # Boxplot with colored outliers
  scale_fill_viridis_d(option = "viridis") +  # Apply colorblind-friendly palette
  labs(
    title = "Total Transaction Amount by Credit Limit Range",
    x = "Credit Limit Range",
    y = "Total Transaction Amount",
    fill = "Credit Limit Range"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),  # Center-align and increase title size
    axis.text.x = element_text(size = 10),  # Increase x-axis text size
    axis.text.y = element_text(size = 10)   # Increase y-axis text size
  )

```

```{r}

```

