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
  `Unit price` > 0,                               # Cena jednostkowa musi być większa od 0
  Total <= `Unit price` * Quantity + `Tax 5%`,     # Total musi być obliczone poprawnie
  Rating >= 1 & Rating <= 10                       # Rating musi być w przedziale 1-10
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

# Można także zastosować imputację wielokrotną, jeśli bardziej zaawansowane podejście jest wymagane:
# Przykład imputacji z użyciem pakietu mice:
# imputed_data <- mice(dane, m = 5, method = 'pmm', seed = 500)
# dane_imputed <- complete(imputed_data, 1)  # Użycie jednej wersji imputowanych danych

# Przykład wizualizacji brakujących danych
gg_miss_var(dane)

# Usuwanie dodatkowych kolumn z sufiksem "_imp"
czyste_dane <- czyste_dane %>%
  select(-ends_with("_imp"))

# Wyświetlanie danych w tabeli
View(czyste_dane)
View(dane)
# Sprawdzanie brakujących danych w Czyste Dane czy wszystko się dobrze zrobiło
n_miss(czyste_dane)  # 0 NA w danych
n_complete(czyste_dane)  # 602028 pełnych wartości w danych
pct_miss(czyste_dane)  # Procent NA = 0 %

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

