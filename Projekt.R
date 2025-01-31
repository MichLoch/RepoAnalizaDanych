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
if (!require("ggstatsplot")) install.packages("ggstatsplot")
library(ggstatsplot) 

# Load the apartments dataset
apartments <- read_csv("apartments_pl_2024_06.csv")

# Check for missing values
n_miss(apartments)  # Count of NA values: 14149
n_complete(apartments)  # Count of complete values: 587879
pct_miss(apartments)  # Percentage of NA values: [1] 2.350223

vis_miss(apartments)  # Heatmap of missing data
# Grouped summaries for missing data
apartments %>% 
  group_by(city) %>% 
  miss_var_summary() %>% 
  print(n = Inf) %>% 
  filter(variable %in% c("price", "rooms", "squareMeters"))


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

# Define validation rules
rules <- validator(
  price > 0,                               # Price must be greater than 0
  squareMeters > 0,                        # Square meters must be positive
  rooms > 0,                               # Rooms must be positive
  floor >= 0 & floor <= floorCount,        # Floor must be within bounds
  buildYear > 1800 & buildYear <= 2024,    # Valid building years
  condition %in% c("Good", "Very Good", "Excellent")  # Valid conditions
)

# Apply validation rules
validation_results <- confront(czyste, rules)

# Summarize validation results
summary(validation_results)

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

