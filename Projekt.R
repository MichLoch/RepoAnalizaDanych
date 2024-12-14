## Reguły do sortowania danych
# biblioteki

library(readr)
library(dplyr)
library(editrules)
library(VIM)

install.packages("naniar")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("prettydoc")
install.packages("mice")
install.packages("shape")
install.packages("jomo")
install.packages("pan")
install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules") #reguły
install.packages("VIM")
install.packages("validate")
install.packages("editrules")
install.packages("errorlocate")
library(errorlocate)
library(mice)
library(naniar)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(validate)
library(editrules)

library(readr)
dane <- read_csv("apartments_pl_2024_06.csv")

n_miss(dane)
# jest 37990 NA w danych

n_complete(dane)
# jest 564038  pełnych wartości w danych


pct_miss(dane)
#procent NA = 6,31 %

is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}
sapply(dane, is.special)
#Czy dane zawierają inne wartości specjalne? Jeśli tak, zastąp je wartością NA.
for (n in colnames(dane)){
  is.na(dane[[n]]) <- is.special(dane[[n]])
}
summary(dane)

# Tworzenie zbioru reguł walidacji
rules <- validator(
  `Unit price` > 0,                              # Cena jednostkowa musi być większa od 0
  Total <= `Unit price` * Quantity + `Tax 5%`,  # Total musi być obliczone poprawnie
  Rating >= 1 & Rating <= 10                    # Rating musi być w przedziale 1-10
)

# Aplikowanie reguł do danych
validation_results <- confront(dane, rules)

# Podsumowanie wyników walidacji
summary(validation_results)

# Szczegóły wyników
print(validation_results)

czyste_dane <-
  dane %>%
  replace_errors(rules)

errors_removed(czyste_dane)

miss_var_summary(dane)
# tabelka pokazująca w jakich kolumnach mamy NA (gross income - 150, Rating - 150, City - 100)



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

