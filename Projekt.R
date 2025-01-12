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

# Load the apartments dataset
apartments <- read_csv("apartments_pl_2024_06.csv")

# Check for missing values
n_miss(apartments)  # Count of NA values: 14149
n_complete(apartments)  # Count of complete values: 587879
pct_miss(apartments)  # Percentage of NA values: [1] 2.350223

# Replace special values (e.g., NaN, Inf) with NA
is.special <- function(x) {
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}
for (n in colnames(apartments)) {
  is.na(apartments[[n]]) <- is.special(apartments[[n]])
}
summary(apartments)

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
validation_results <- confront(apartments, rules)

# Summarize validation results
summary(validation_results)

# Clean the data by removing rows with errors
clean_data <- apartments %>% 
  replace_errors(rules)

# Visualize missing data
vis_miss(apartments)  # Heatmap of missing data

# Grouped summaries for missing data
apartments %>% 
  group_by(city) %>% 
  miss_var_summary() %>% 
  print(n = Inf) %>% 
  filter(variable %in% c("price", "rooms", "squareMeters"))

apartments %>% 
  miss_case_table()  # Summary of missing observations in rows

# Replace NA values
apartments <- apartments %>%
  mutate(
    condition = replace_na(condition, "Good"),  # Replace missing condition with "Good"
    across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm = TRUE), .))  # Replace numeric NAs with column means
  )

# Remove columns with `_imp` suffix if they exist
clean_data <- clean_data %>%
  select(-ends_with("_imp"))

# Round the `floor` values
apartments <- apartments %>%
  mutate(floor = round(floor))

# Visualizations
ggplot(apartments, aes(x = squareMeters, y = price)) +
  geom_point(aes(color = city, shape = ownership), size = 2) +
  theme_minimal()

ggplot(clean_data, aes(x = city, y = price)) +
  geom_boxplot(aes(fill = city), alpha = 0.85) +
  theme_minimal() +
  labs(title = "Price Distribution by City", x = "City", y = "Price")

# Heatmap of missing data correlations
correlation_matrix <- cor(is.na(apartments), use = "pairwise.complete.obs")
corrplot(correlation_matrix, method = "square")

# Bar plot of average price by city
clean_data %>%
  group_by(city) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(city, avg_price), y = avg_price, fill = city)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Price by City", x = "City", y = "Average Price")
