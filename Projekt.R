# Load necessary libraries
library(readr)
library(dplyr)
library(editrules)
library(VIM)
library(mice)
library(naniar)
library(ggplot2)
library(tidyverse)
library(validate)
library(errorlocate)
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
if (!require("corrplot")) install.packages("ggstatsplot")

library(ggstatsplot) 
library(e1071)  # Dla obliczania skośności i kurtozy
library(DMwR)    # Dla wykrywania punktów odstających
# Load the apartments dataset
apartments <- read_csv("apartments_pl_2024_06.csv")

# Check for missing values
n_miss(apartments)  # Count of NA values: 14149
n_complete(apartments)  # Count of complete values: 587879
pct_miss(apartments)  # Percentage of NA values: [1] 2.350223

# Calculate the percentage of missing data for each column
missing_percentage <- sapply(apartments, function(x) mean(is.na(x)) * 100)

# Create a data frame to display the results
missing_data_summary <- data.frame(
  Variable = names(missing_percentage),
  Missing_Percentage = missing_percentage
)

# Display the result sorted by percentage of missing data
missing_data_summary <- missing_data_summary %>%
  arrange(desc(Missing_Percentage))

# Print the summary
print(missing_data_summary)


vis_miss(apartments)  # Heatmap of missing data
# Grouped summaries for missing data
apartments %>% 
  group_by(city) %>% 
  miss_var_summary() %>% 
  print(n = Inf)

apartments %>% 
  miss_case_table()  # Summary of missing observations in rows

# Replace special values (e.g., NaN, Inf) with NA
is.special <- function(x) {
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}
for (n in colnames(apartments)) {
  is.na(apartments[[n]]) <- is.special(apartments[[n]])
}
summary(apartments)

#korzystanie z hotdeck żeby wypełnić luki danych
czyste <- hotdeck(apartments)
n_miss(czyste)  # Count of NA values: 0
n_complete(czyste)  # Count of complete values: 1204056
pct_miss(czyste)  # Percentage of NA values: [1] 0


# Apply validation rules
validation_results <- confront(czyste, rules)

# Clean the data by removing rows with errors
clean_data <- czyste %>% 
  replace_errors(rules)

# Remove columns with _imp suffix if they exist
clean_data <- clean_data %>%
  select(-ends_with("_imp"))

# Round the floor values
czyste <- czyste %>%
  mutate(floor = round(floor))

# Visualizations
ggplot(czyste, aes(x = squareMeters, y = price)) +
  geom_point(aes(color = city, shape = ownership), size = 2) +
  theme_minimal()

ggplot(czyste, aes(x = city, y = price)) +
  geom_boxplot(aes(fill = city), alpha = 0.85) +
  theme_minimal() +
  labs(title = "Price Distribution by City", x = "City", y = "Price")

# Heatmap of missing data correlations
# Select specific numeric variables for the heatmap
selected_data <- czyste %>%
  select(squareMeters, rooms, floor, floorCount, buildYear, price, centreDistance, latitude, longitude)

# Compute correlation matrix for selected variables
correlation_matrix <- cor(selected_data, use = "pairwise.complete.obs")

# Visualize the correlation matrix with heatmap
corrplot(correlation_matrix, method = "square", 
         tl.cex = 0.8, cl.cex = 0.8, 
         type = "upper", order = "hclust", 
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Heatmap for Selected Variables", mar = c(0, 0, 2, 0))


# Bar plot of average price by city
czyste %>%
  group_by(city) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(city, avg_price), y = avg_price, fill = city)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Price by City", x = "City", y = "Average Price") 


# ggstatsplot visualizations

# Example of bar chart for missing data visualization using ggbarstats
ggbarstats(
  data = apartments,
  x = city,
  y = condition,  # Replace with a categorical variable
  bf.message = FALSE,
  title = "Comparison of Condition Across Cities"
)


# aov and interaction plot
# Using aov to assess interaction effects between variables
aov <- aov(price ~ city * condition, data = apartments)
summary(aov)

# Interaction plot
interaction.plot(
  apartments$city,
  apartments$condition,
  apartments$price,
  main = "Interaction Plot: City x Condition",
  xlab = "City",
  ylab = "Average Price",
  col = 1:6
)

ggplot(czyste, aes(x = factor(hasSecurity, labels = c("No Security", "Has Security")), y = price)) +
  geom_boxplot(aes(fill = factor(hasSecurity)), alpha = 0.8) +
  theme_minimal() +
  labs(title = "Impact of Security on Apartment Prices", x = "Security", y = "Price") +
  scale_fill_manual(values = c("red", "green"))
czyste %>%
  group_by(hasSecurity) %>%
  summarise(
    count = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE)  # Standard deviation for variability
  )

ggplot(czyste, aes(x = centreDistance, y = price)) +
  geom_point(aes(color = city), alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Effect of Distance from Center on Price", x = "Distance from City Center (km)", y = "Price")

czyste %>%
  group_by(city) %>%
  summarise(avg_size = mean(squareMeters, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(city, avg_size), y = avg_size, fill = city)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Apartment Size by City", x = "City", y = "Average Square Meters")

ggplot(czyste, aes(x = buildingMaterial, y = price, fill = buildingMaterial)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Impact of Building Material on Price", x = "Building Material", y = "Price")

# Select relevant POI variables + price
poi_vars <- c("schoolDistance", "clinicDistance", "postOfficeDistance", "kindergartenDistance", 
              "restaurantDistance", "collegeDistance", "pharmacyDistance", "centreDistance", "price")

poi_correlation <- cor(czyste %>% select(all_of(poi_vars)), use = "pairwise.complete.obs")

# Plot correlation heatmap
corrplot(poi_correlation, method = "square", tl.cex = 0.8, cl.cex = 0.8, 
         type = "upper", order = "hclust", addCoef.col = "black", 
         number.cex = 0.7, title = "POI Distance Correlations with Price", mar = c(0, 0, 2, 0))


ggplot(czyste, aes(x = floor)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(czyste$floor, na.rm = TRUE), 
                                  max(czyste$floor, na.rm = TRUE), 
                                  by = 1)) 
labs(title = "Distribution of Apartments by Floor", x = "Floor", y = "Count")


ggplot(czyste, aes(x = condition, y = price, fill = condition)) +
  geom_violin(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Impact of Apartment Condition on Prices", x = "Condition", y = "Price")
czyste %>%
  group_by(condition) %>%
  summarise(
    count = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE)  # Standard deviation for variability
  )


corrplot(correlation_matrix, method = "square", 
         tl.cex = 0.8, cl.cex = 0.8, 
         type = "upper", order = "hclust", 
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Heatmap for Selected Variables", mar = c(0, 0, 2, 0))


# Calculate the count and percentage of each building type
type_summary <- czyste %>%
  count(type) %>%
  mutate(percentage = n / sum(n) * 100)

# Create a bar plot to show the count and percentage of each building type with fixed bar height
ggplot(type_summary, aes(x = type, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = 0.5, hjust = 0.5, size = 5, color = "black") +  # Center the percentage
  theme_minimal() +
  labs(title = "Distribution of Building Types", x = "Building Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate the average price for each building type
average_price_by_type <- czyste %>%
  group_by(type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

# Load the scales package for formatting numbers
library(scales)

# Calculate the average price for each building type
average_price_by_type <- czyste %>%
  group_by(type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

# Create a bar plot to visualize the average price by building type with currency formatting
ggplot(average_price_by_type, aes(x = type, y = avg_price, fill = type)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +  # Currency format
  theme_minimal() +
  labs(title = "Average Price by Building Type", x = "Building Type", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate the x-axis labels if needed
  geom_text(aes(label = scales::comma(avg_price, big.mark = ".", decimal.mark = ",")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold")
