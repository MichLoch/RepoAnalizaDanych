## Reguły do sortowania danych
# biblioteki

library(readr)
library(dplyr)
library(editrules)
library(VIM)

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

