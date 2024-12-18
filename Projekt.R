## Reguły do sortowania danych
# biblioteki
library(readr)
library(dplyr)
library(editrules)
library(VIM)
library(mice)
library(naniar)
library(ggplot2)
library(tidyverse)
library(dlookr)
library(validate)
library(errorlocate)

# Wczytanie danych
dane <- read_csv("apartments_pl_2024_06.csv")

# Check if required columns are present
if (!"price" %in% colnames(dane)) {
  stop("Missing column: 'price'")
}

# Sprawdzanie brakujących danych
n_miss(dane)  # 37990 NA w danych
n_complete(dane)  # 564038 pełnych wartości w danych
pct_miss(dane)  # Procent NA = 6,31 %

# Funkcja sprawdzająca wartości specjalne
is.special <- function(x) {
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

# Zmieniamy wartości specjalne (NaN, Inf) na NA
for (n in colnames(dane)) {
  is.na(dane[[n]]) <- is.special(dane[[n]])
}

# Podstawowa analiza danych po zmianie
summary(dane)

# Tworzenie zbioru reguł walidacji
rules <- validator(
  price > 0,                              # Cena musi być większa od 0
  Total <= price * rooms,                  # Total musi być mniejsze lub równe price * rooms
  # Remove Rating validation or replace with a suitable rule based on your dataset
  # For example, if you have a column related to condition or other metrics, you can replace the Rating rule
  condition %in% c("Good", "Very Good", "Excellent")  # Example: condition could be validated (if applicable)
)

# Aplikowanie reguł do danych
validation_results <- confront(dane, rules)

# Podsumowanie wyników walidacji
summary(validation_results)

# Szczegóły wyników walidacji
print(validation_results)

# Zastąpienie błędów według reguł
czyste_dane <- dane %>%
  replace_errors(rules)

# Podsumowanie błędów usuniętych z danych
errors_removed(czyste_dane)

# Analiza brakujących danych w kolumnach
miss_var_summary(dane)  # Podsumowanie brakujących wartości w kolumnach


# Zastępowanie NA w kolumnie condition najczęstszą wartością
dane <- dane %>%
  mutate(condition = replace_na(condition, as.character(names(sort(table(condition), decreasing = TRUE))[1])))


# Zamiana NA w danych
# 1. Zastępujemy NA wartościami średnimi (dla danych numerycznych)
dane <- dane %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm = TRUE), .)))

# 2. Zastępujemy NA wartościami zerowymi (jeśli nie chcemy imputacji, tylko zerowanie)
dane[is.na(dane)] <- 0

# 3. Zastępujemy NA w zmiennych kategorycznych najczęstszymi wartościami (tryb)
dane <- dane %>%
  mutate(across(where(is.character), ~ replace_na(., as.character(names(sort(table(.), decreasing = TRUE))[1]))))

# Zastąpienie NA w danych numerycznych medianą (przykład, jeśli trzeba)
dane$column_name <- ifelse(is.na(dane$column_name), median(dane$column_name, na.rm = TRUE), dane$column_name)

# Przykład wizualizacji brakujących danych
gg_miss_var(dane)

# Usuwanie dodatkowych kolumn z sufiksem "_imp"
czyste_dane <- czyste_dane %>%
  select(-ends_with("_imp"))

# Zaokrąglanie wartości w kolumnie floor do pełnych liczb
dane <- dane %>%
  mutate(floor = round(floor))


# Wyświetlanie danych w tabeli
View(czyste_dane)
View(dane)


##### STARY KOD #####
#####

dane <- data.frame(id=1:5,
                   floor=c(25,-5, 30, 40, 156),
                   buildingYear=c(3000,4000,-500,6000,7000)
                   #reguły:
                   reguly <editset(c(
                     "floor">=0,
                     "buildYear"<=120,
                     "buildingMaterial"
                     "condition"
                   ))
                   #walidacja
                   summary(violatedEdits(reguly,dane))
                   bledy <- violatedEdits(reguly,dane)
View(dane)
bledy <- violatedEdits(reguly.dane)

# zastosowanie reguły do walidacji
summary(violatedEdits(reguly,dane))

# podsumuj na wykresie
bledy <- violatedEdits(reguly.dane)

dane <- read_csv("apartments_pl_2024_06.csv")
# poprawiam bledy w danych
dane[localizeErrors(reguly,dane)$adapt] <- NA
# coś zostało?
any(violatedEdits(reguly,dane),na.rm=TRUE)
# nic

# imputacja braków danych
library(VIM)
czyste_dane <- hotdeck(dane)
view(czyste_dane)

library(naniar)

# Wykres procentowego udziału brakujących danych w kolumnach
gg_miss_var(dane) +
  labs(title = "Procent brakujących danych w kolumnach",
       x = "Kolumny",
       y = "Procent brakujących wartości") +
  theme_minimal()

library(ggplot2)

# Wykres wyników walidacji
validation_summary <- as.data.frame(summary(validation_results))

ggplot(validation_summary, aes(x = rule, y = fails)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Wyniki walidacji reguł",
       x = "Reguły",
       y = "Liczba błędów") +
  theme_minimal()

# Histogram ceny (price)
ggplot(dane, aes(x = price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Rozkład cen mieszkań",
       x = "Cena",
       y = "Częstotliwość") +
  theme_minimal()

# Brakujące dane przed imputacją
miss_before <- pct_miss(dane)

# Brakujące dane po imputacji
miss_after <- pct_miss(czyste_dane)

# Wykres porównawczy
data_comparison <- data.frame(
  Stage = c("Przed imputacją", "Po imputacji"),
  MissingPercentage = c(miss_before, miss_after)
)

ggplot(data_comparison, aes(x = Stage, y = MissingPercentage, fill = Stage)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Porównanie brakujących danych przed i po imputacji",
       x = "Etap",
       y = "Procent brakujących danych") +
  theme_minimal()

# Wykres liczby błędów dla reguł
errors_summary <- as.data.frame(summary(violatedEdits(reguly, dane)))

ggplot(errors_summary, aes(x = edit, y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Liczba błędów w danych",
       x = "Reguły",
       y = "Liczba błędów") +
  theme_minimal()

# Boxplot ceny w zależności od liczby pokoi
ggplot(dane, aes(x = as.factor(rooms), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cena mieszkań a liczba pokoi",
       x = "Liczba pokoi",
       y = "Cena") +
  theme_minimal()

